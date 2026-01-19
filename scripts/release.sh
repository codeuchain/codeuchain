#!/bin/sh
################################################################################
# CodeUChain Release Automation Script
# 
# Comprehensive CLI-based release automation for polyglot monorepo.
# Supports interactive, non-interactive, and CI/agent modes with smart defaults.
#
# Usage: scripts/release.sh [OPTIONS]
# 
# License: Apache 2.0
################################################################################

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Global variables with defaults
VERSION=""
RELEASE_TYPE=""
LANGUAGES=""
BRANCH=""
AUTO_DETECT=false
DRY_RUN=false
NO_INTERACTIVE=false
JSON_OUTPUT=false
VERBOSE=false

# Available languages
AVAILABLE_LANGUAGES="python,go,javascript,csharp,rust,java,cpp"

# Exit codes
EXIT_SUCCESS=0
EXIT_ERROR=1
EXIT_CANCELLED=2

################################################################################
# Utility Functions
################################################################################

print_usage() {
  cat << 'EOF'
Usage: scripts/release.sh [OPTIONS]

Options:
  --version VERSION      Target version (e.g., 1.2.3)
  --type TYPE           Release type: major, minor, patch, hotfix
  --languages LANGS     Comma-separated languages (default: all)
                        Available: python,go,javascript,csharp,rust,java,cpp
  --branch BRANCH       Release branch name (default: current branch)
  --auto                Auto-detect from conventional commits
  --dry-run             Show what would happen without creating tags
  --no-interactive      Non-interactive mode (fail instead of prompt)
  --json                Output result as JSON for machine parsing
  --verbose             Enable verbose output
  --help                Show this help message

Examples:
  # Interactive mode (prompts user)
  ./scripts/release.sh

  # Non-interactive (for CI/automation)
  ./scripts/release.sh --version 1.2.3 --type minor --languages go,rust

  # Auto-detect from commits
  ./scripts/release.sh --auto --type minor

  # Preview without creating tags
  ./scripts/release.sh --version 1.2.3 --type patch --dry-run

EOF
}

log_info() {
  printf "${BLUE}ℹ${NC} %s\n" "$1"
}

log_success() {
  printf "${GREEN}✓${NC} %s\n" "$1"
}

log_warn() {
  printf "${YELLOW}⚠${NC} %s\n" "$1"
}

log_error() {
  printf "${RED}✗${NC} %s\n" "$1" >&2
}

log_verbose() {
  if [ "$VERBOSE" = true ]; then
    printf "${BLUE}→${NC} %s\n" "$1"
  fi
}

# Prompt user for input (interactive mode only)
prompt_input() {
  local prompt=$1
  local default=$2
  local input=""
  
  # Skip prompt if non-interactive or no TTY
  if [ "$NO_INTERACTIVE" = true ] || ! [ -t 0 ]; then
    if [ -n "$default" ]; then
      printf "%s" "$default"
      return 0
    else
      log_error "Input required but in non-interactive mode: $prompt"
      return 1
    fi
  fi
  
  if [ -n "$default" ]; then
    printf "%s [%s]: " "$prompt" "$default"
  else
    printf "%s: " "$prompt"
  fi
  
  read -r input
  
  if [ -z "$input" ] && [ -n "$default" ]; then
    printf "%s" "$default"
  else
    printf "%s" "$input"
  fi
}

# Prompt yes/no
prompt_confirm() {
  local prompt=$1
  local default=$2
  local response=""
  
  # Skip prompt if non-interactive or no TTY
  if [ "$NO_INTERACTIVE" = true ] || ! [ -t 0 ]; then
    if [ "$default" = "yes" ] || [ "$default" = "y" ]; then
      return 0
    else
      return 1
    fi
  fi
  
  printf "%s [%s/no]: " "$prompt" "$default"
  read -r response
  
  case "$response" in
    y|yes|Y|YES) return 0 ;;
    n|no|N|NO) return 1 ;;
    "") [ "$default" = "yes" ] && return 0 || return 1 ;;
    *) return 1 ;;
  esac
}

################################################################################
# Git Utilities
################################################################################

get_current_branch() {
  git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown"
}

get_last_version_for_language() {
  local lang=$1
  git tag -l "${lang}-v*" --sort=-version:refname --merged HEAD 2>/dev/null | head -1 | sed "s/${lang}-v//"
}

is_release_branch() {
  local branch=$1
  case "$branch" in
    release/*|hotfix/*|main|master) return 0 ;;
    *) return 1 ;;
  esac
}

################################################################################
# Changelog Utilities
################################################################################

get_last_tag() {
  git describe --tags --abbrev=0 2>/dev/null || echo ""
}

get_commits_since_tag() {
  local tag=$1
  if [ -z "$tag" ]; then
    # First release - get all commits
    git log --pretty=format:%B HEAD
  else
    git log ${tag}..HEAD --pretty=format:%B
  fi
}

parse_conventional_commits() {
  local commits="$1"
  local features=""
  local fixes=""
  local breaking=""
  
  # Parse commits line by line
  printf '%s' "$commits" | while IFS= read -r line; do
    case "$line" in
      feat\(*)
        scope=$(printf '%s' "$line" | sed 's/feat(\([^)]*\)).*/\1/')
        msg=$(printf '%s' "$line" | sed 's/feat[^:]*: *//')
        features="${features}  - $msg (scope: $scope)
"
        ;;
      fix\(*)
        scope=$(printf '%s' "$line" | sed 's/fix(\([^)]*\)).*/\1/')
        msg=$(printf '%s' "$line" | sed 's/fix[^:]*: *//')
        fixes="${fixes}  - $msg (scope: $scope)
"
        ;;
      BREAKING\ CHANGE*)
        msg=$(printf '%s' "$line" | sed 's/BREAKING CHANGE: *//')
        breaking="${breaking}  - $msg
"
        ;;
    esac
  done
}

generate_changelog_entry() {
  local version=$1
  local last_tag=$2
  local release_date
  
  release_date=$(date -u +%Y-%m-%d 2>/dev/null || date -u +"%Y-%m-%d")
  
  log_verbose "Generating changelog for v$version (since $last_tag)"
  
  # Get commits since last tag
  local commits
  commits=$(get_commits_since_tag "$last_tag")
  
  if [ -z "$commits" ]; then
    log_verbose "No commits since last tag, creating minimal entry"
  fi
  
  # Build entry
  printf "## [%s] - %s\n\n### Changed\n  - Release version %s\n" "$version" "$release_date" "$version"
}

update_changelog() {
  local version=$1
  local dry_run=$2
  
  if [ ! -f "CHANGELOG.md" ]; then
    log_verbose "CHANGELOG.md not found, skipping"
    return 0
  fi
  
  local last_tag
  last_tag=$(get_last_tag)
  
  local changelog_entry
  changelog_entry=$(generate_changelog_entry "$version" "$last_tag")
  
  if [ "$dry_run" = true ]; then
    log_info "DRY RUN: Would update CHANGELOG.md with v$version"
    printf '%s\n' "$changelog_entry" | head -3
    return 0
  fi
  
  # Insert new entry after "## [Unreleased]" line
  local temp_file
  temp_file=$(mktemp)
  
  {
    # Find line number of "## [Unreleased]"
    awk '
    /^## \[Unreleased\]/ {
      print
      print ""
      exit
    }
    {print}
    ' CHANGELOG.md
    
    # Add the new version entry
    printf '%s\n' "$changelog_entry"
    
    # Add rest of file starting from first existing version
    awk '
    NR > 1 && /^## \[Unreleased\]/ {
      getline
      while (getline) {
        if (NF || NR == 1) {print}
      }
    }
    ' CHANGELOG.md
  } > "$temp_file"
  
  if mv "$temp_file" CHANGELOG.md; then
    log_success "Updated CHANGELOG.md"
    return 0
  else
    log_error "Failed to update CHANGELOG.md"
    rm -f "$temp_file"
    return 1
  fi
}

commit_changelog() {
  local version=$1
  local dry_run=$2
  
  if [ "$dry_run" = true ]; then
    log_info "DRY RUN: Would commit changelog and version updates"
    return 0
  fi
  
  # Check if CHANGELOG.md exists and has changes (tracked or untracked)
  if [ -f "CHANGELOG.md" ]; then
    # Add the file (works for both tracked and untracked)
    git add CHANGELOG.md 2>/dev/null || true
  fi
  
  # Also add VERSIONS.json if it has changes
  if [ -f "VERSIONS.json" ]; then
    git add VERSIONS.json 2>/dev/null || true
  fi
  
  # Check if there are staged changes
  if git diff --cached --quiet 2>/dev/null; then
    log_verbose "No changelog or version changes to commit"
    return 0
  fi
  
  # Commit the changes
  if git commit -m "chore: update changelog and versions for v$version"; then
    log_success "Committed changelog and version updates"
    return 0
  else
    log_warn "Failed to commit updates"
    return 1
  fi
}

tag_exists() {
  git rev-parse "refs/tags/$1" >/dev/null 2>&1
}

################################################################################
# Version File Utilities
################################################################################

get_version_from_file() {
  local lang=$1
  
  if [ ! -f "VERSIONS.json" ]; then
    log_verbose "VERSIONS.json not found"
    return 1
  fi
  
  # Try to extract version using awk/grep (works without jq)
  local version
  version=$(grep -A 2 "\"$lang\":" VERSIONS.json 2>/dev/null | grep '"version"' | head -1 | sed 's/.*"version": "\([^"]*\)".*/\1/')
  
  if [ -z "$version" ]; then
    log_verbose "Version not found in VERSIONS.json for $lang"
    return 1
  fi
  
  printf '%s' "$version"
  return 0
}

################################################################################
# Version Utilities
################################################################################

validate_semver() {
  local version=$1
  if ! echo "$version" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    return 1
  fi
  return 0
}

parse_semver() {
  local version=$1
  echo "$version" | awk -F. '{print $1","$2","$3}'
}

calculate_next_version() {
  local current=$1
  local release_type=$2
  
  if [ -z "$current" ]; then
    case "$release_type" in
      major) echo "1.0.0" ;;
      minor) echo "0.1.0" ;;
      patch) echo "0.0.1" ;;
      hotfix) echo "0.0.1" ;;
      *) return 1 ;;
    esac
    return 0
  fi
  
  local parts=$(parse_semver "$current")
  local major=$(echo "$parts" | cut -d, -f1)
  local minor=$(echo "$parts" | cut -d, -f2)
  local patch=$(echo "$parts" | cut -d, -f3)
  
  case "$release_type" in
    major)
      major=$((major + 1))
      minor=0
      patch=0
      ;;
    minor)
      minor=$((minor + 1))
      patch=0
      ;;
    patch)
      patch=$((patch + 1))
      ;;
    hotfix)
      patch=$((patch + 1))
      ;;
    *)
      return 1
      ;;
  esac
  
  printf "%d.%d.%d" "$major" "$minor" "$patch"
}

################################################################################
# Language Utilities
################################################################################

validate_languages() {
  local langs=$1
  local valid=true
  
  for lang in $(printf '%s' "$langs" | tr ',' '\n'); do
    lang=$(printf '%s' "$lang" | xargs)
    if ! echo "$AVAILABLE_LANGUAGES" | grep -q "$lang"; then
      log_error "Unknown language: $lang"
      valid=false
    fi
  done
  
  [ "$valid" = true ]
}

normalize_languages() {
  local langs=$1
  if [ -z "$langs" ]; then
    echo "$AVAILABLE_LANGUAGES"
  else
    echo "$langs" | tr -d ' '
  fi
}

################################################################################
# Release State Management
################################################################################

save_release_config() {
  local version=$1
  local release_type=$2
  local languages=$3
  local branch=$4
  
  cat > ./.release.json << EOF
{
  "version": "$version",
  "type": "$release_type",
  "languages": "$languages",
  "branch": "$branch",
  "created_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
  
  log_verbose "Release config saved to .release.json"
}

load_release_config() {
  if [ ! -f "./.release.json" ]; then
    return 1
  fi
  
  # Extract values from JSON (simple sed parsing)
  VERSION=$(sed -n 's/.*"version": "\([^"]*\)".*/\1/p' ./.release.json)
  RELEASE_TYPE=$(sed -n 's/.*"type": "\([^"]*\)".*/\1/p' ./.release.json)
  LANGUAGES=$(sed -n 's/.*"languages": "\([^"]*\)".*/\1/p' ./.release.json)
  BRANCH=$(sed -n 's/.*"branch": "\([^"]*\)".*/\1/p' ./.release.json)
  
  log_verbose "Loaded release config: v$VERSION ($RELEASE_TYPE)"
  return 0
}

################################################################################
# Release Process
################################################################################

create_release_tags() {
  local version=$1
  local langs=$2
  local dry_run=$3
  
  local tags_created=0
  local failed_langs=""
  
  for lang in $(printf '%s' "$langs" | tr ',' '\n'); do
    lang=$(printf '%s' "$lang" | xargs)
    
    if [ -z "$lang" ]; then
      continue
    fi
    
    local tag="${lang}-v${version}"
    
    log_info "Processing: $tag"
    
    if tag_exists "$tag"; then
      log_error "Tag already exists: $tag"
      failed_langs="${failed_langs:+$failed_langs,}$lang"
      continue
    fi
    
    if [ ! -d "packages/$lang" ]; then
      log_warn "Package not found: packages/$lang (skipping)"
      continue
    fi
    
    if [ "$dry_run" = true ]; then
      log_info "DRY RUN: Would create $tag"
    else
      if git tag -a "$tag" -m "Release $lang v$version"; then
        log_success "Created: $tag"
        tags_created=$((tags_created + 1))
      else
        log_error "Failed: $tag"
        failed_langs="${failed_langs:+$failed_langs,}$lang"
      fi
    fi
  done
  
  if [ $tags_created -gt 0 ] && [ "$dry_run" = false ]; then
    log_success "Created $tags_created tags"
  fi
  
  if [ -n "$failed_langs" ]; then
    log_error "Failed to process: $failed_langs"
    return 1
  fi
  
  return 0
}

verify_release_branch() {
  local branch=$1
  
  if ! is_release_branch "$branch"; then
    log_error "Not on a release branch: $branch"
    log_info "Valid branches: main, master, release/*, hotfix/*"
    return 1
  fi
  
  log_success "Release branch: $branch"
  return 0
}

################################################################################
# Interactive Mode
################################################################################

interactive_input() {
  log_info "CodeUChain Release Automation"
  echo ""
  
  local current_branch=$(get_current_branch)
  BRANCH=$(prompt_input "Release branch" "$current_branch")
  
  if ! verify_release_branch "$BRANCH"; then
    return 1
  fi
  
  LANGUAGES=$(prompt_input "Languages (comma-separated)" "$AVAILABLE_LANGUAGES")
  if ! validate_languages "$LANGUAGES"; then
    return 1
  fi
  LANGUAGES=$(normalize_languages "$LANGUAGES")
  
  RELEASE_TYPE=$(prompt_input "Release type (major/minor/patch/hotfix)" "minor")
  case "$RELEASE_TYPE" in
    major|minor|patch|hotfix) ;;
    *) log_error "Invalid: $RELEASE_TYPE"; return 1 ;;
  esac
  
  local first_lang=$(echo "$LANGUAGES" | cut -d, -f1)
  local last_version=$(get_last_version_for_language "$first_lang")
  local suggested=$(calculate_next_version "$last_version" "$RELEASE_TYPE")
  
  VERSION=$(prompt_input "Version" "${suggested:-1.0.0}")
  
  if ! validate_semver "$VERSION"; then
    log_error "Invalid version: $VERSION (expected X.Y.Z)"
    return 1
  fi
  
  return 0
}

################################################################################
# Main Function
################################################################################

main() {
  # Parse command-line arguments
  while [ $# -gt 0 ]; do
    case "$1" in
      --version)
        VERSION="$2"
        shift 2
        ;;
      --type)
        RELEASE_TYPE="$2"
        shift 2
        ;;
      --languages)
        LANGUAGES="$2"
        shift 2
        ;;
      --branch)
        BRANCH="$2"
        shift 2
        ;;
      --auto)
        AUTO_DETECT=true
        shift
        ;;
      --dry-run)
        DRY_RUN=true
        shift
        ;;
      --no-interactive)
        NO_INTERACTIVE=true
        shift
        ;;
      --json)
        JSON_OUTPUT=true
        shift
        ;;
      --verbose)
        VERBOSE=true
        shift
        ;;
      --help)
        print_usage
        exit $EXIT_SUCCESS
        ;;
      *)
        log_error "Unknown option: $1"
        print_usage
        exit $EXIT_ERROR
        ;;
    esac
  done
  
  if ! command -v git >/dev/null 2>&1; then
    log_error "git is not installed"
    exit $EXIT_ERROR
  fi
  
  if [ -z "$BRANCH" ]; then
    BRANCH=$(get_current_branch)
  fi
  
  log_verbose "Branch: $BRANCH"
  log_verbose "TTY: $([ -t 0 ] && echo 'yes' || echo 'no')"
  
  if [ -z "$VERSION" ] || [ -z "$RELEASE_TYPE" ] || [ -z "$LANGUAGES" ]; then
    if ! [ -t 0 ] && [ "$NO_INTERACTIVE" = false ]; then
      log_error "Not a TTY and missing required options"
      exit $EXIT_ERROR
    fi
    
    if ! interactive_input; then
      exit $EXIT_ERROR
    fi
  else
    LANGUAGES=$(normalize_languages "$LANGUAGES")
  fi
  
  if ! validate_languages "$LANGUAGES"; then
    exit $EXIT_ERROR
  fi
  
  if ! validate_semver "$VERSION"; then
    log_error "Invalid version: $VERSION"
    exit $EXIT_ERROR
  fi
  
  case "$RELEASE_TYPE" in
    major|minor|patch|hotfix) ;;
    *)
      log_error "Invalid type: $RELEASE_TYPE"
      exit $EXIT_ERROR
      ;;
  esac
  
  if ! verify_release_branch "$BRANCH"; then
    exit $EXIT_ERROR
  fi
  
  echo ""
  log_info "Release Summary:"
  printf "  Version:    %s\n" "$VERSION"
  printf "  Type:       %s\n" "$RELEASE_TYPE"
  printf "  Branch:     %s\n" "$BRANCH"
  printf "  Languages:  %s\n" "$LANGUAGES"
  printf "  Dry-run:    %s\n" "$([ "$DRY_RUN" = true ] && echo 'yes' || echo 'no')"
  echo ""
  
  if [ "$DRY_RUN" = false ] && [ "$AUTO_DETECT" = false ]; then
    if ! prompt_confirm "Proceed with release?" "yes"; then
      exit $EXIT_CANCELLED
    fi
  fi
  
  # Update changelog before creating tags
  log_info "Preparing changelog..."
  if ! update_changelog "$VERSION" "$DRY_RUN"; then
    if [ "$DRY_RUN" = false ]; then
      exit $EXIT_ERROR
    fi
  fi
  
  # Commit changelog before tagging
  if ! commit_changelog "$VERSION" "$DRY_RUN"; then
    if [ "$DRY_RUN" = false ]; then
      log_warn "Changelog commit failed, continuing with release"
    fi
  fi
  
  if ! create_release_tags "$VERSION" "$LANGUAGES" "$DRY_RUN"; then
    exit $EXIT_ERROR
  fi
  
  # Save release config for post-push hook
  if [ "$DRY_RUN" = false ]; then
    save_release_config "$VERSION" "$RELEASE_TYPE" "$LANGUAGES" "$BRANCH"
  fi
  
  if [ "$DRY_RUN" = true ]; then
    log_success "Dry run completed"
  else
    log_success "Release tags created"
    log_info "Push with: git push origin --tags"
  fi
  
  exit $EXIT_SUCCESS
}

main "$@"
