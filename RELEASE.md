# CodeUChain Release Automation

Complete automated release workflow for the CodeUChain polyglot monorepo.

## Overview

The release system is fully automated using:
- **Pre-push hook**: Detects release branches and prompts for release details
- **Release script**: Generates changelog, validates, and creates version tags
- **Changelog automation**: Updates CHANGELOG.md with Keep a Changelog format
- **Post-push hook**: Cleans up release state after successful push
- **GitHub Actions**: Automatically publishes to package registries

### State Management

Release state is tracked in `.release.json` (gitignored):
1. User runs release on a release branch → `release.sh` runs
2. Changelog is automatically generated and updated
3. Changelog commit is created
4. Git tags are created (referencing the changelog commit)
5. Push succeeds
6. Post-push hook runs → deletes `.release.json`
7. State reset, ready for next release

### Automatic Changelog Generation

The release script automatically:
- Generates a changelog entry for the new version
- Uses Keep a Changelog format (https://keepachangelog.com)
- Inserts entry under "## [Unreleased]" section
- Creates a git commit before tagging
- Ensures tags point to the changelog-updated commit

## Release Branches

Standard practice: create release branches from `main`

```bash
# Create a release branch
git checkout -b release/1.2.3 main

# Make final fixes/version bumps
git commit -am "chore: version bump 1.2.3"

# Push will trigger automated release
git push origin release/1.2.3
```

## Usage

### For Humans (Interactive)

```bash
# Push to release branch
git push origin release/1.2.3

# Pre-push hook triggers → prompts interactively:
# 🚀 Release branch detected: release/1.2.3
# ℹ CodeUChain Release Automation
# 
# Release branch [release/1.2.3]: 
# Languages (comma-separated) [python,go,javascript,csharp,rust,java,cpp]: go,rust
# Release type (major/minor/patch/hotfix) [minor]: patch
# Version [1.0.1]: 1.0.1
# 
# Proceed with release? [yes/no]: yes
# 
# ✓ Release tags created
# ℹ Push with: git push origin --tags
```

### For AI/Automation (Non-Interactive)

```bash
# Agent runs release with all flags
./scripts/release.sh \
  --version 1.2.3 \
  --type minor \
  --languages go,rust \
  --no-interactive

# Or via CI/CD
echo "Creating release..."
scripts/release.sh \
  --version $VERSION \
  --type $RELEASE_TYPE \
  --languages $LANGUAGES \
  --no-interactive \
  --json
```

### Preview Mode

```bash
# Dry run to see what would happen
./scripts/release.sh \
  --version 1.2.3 \
  --type patch \
  --dry-run
```

## Release Types

| Type | Version Change | Usage |
|------|---|---|
| **major** | X.0.0 | Breaking changes |
| **minor** | X.Y.0 | New features |
| **patch** | X.Y.Z | Bug fixes |
| **hotfix** | X.Y.Z | Emergency fixes |

## CLI Options

```
--version VERSION      Target version (e.g., 1.2.3)
--type TYPE           Release type: major, minor, patch, hotfix
--languages LANGS     Comma-separated languages (default: all)
--branch BRANCH       Release branch name (default: current branch)
--auto                Auto-detect from conventional commits
--dry-run             Show what would happen without creating tags
--no-interactive      Non-interactive mode (fail instead of prompt)
--json                Output result as JSON for machine parsing
--verbose             Enable verbose output
--help                Show this help message
```

## Complete Release Workflow

### Step 1: Create Release Branch

```bash
git checkout -b release/1.2.3 main
git push -u origin release/1.2.3
```

### Step 2: Make Fixes (Optional)

```bash
# Any final bug fixes go here
git commit -am "fix: critical bug in parser"
```

### Step 3: Push (Triggers Automation)

```bash
git push origin release/1.2.3
```

**What happens automatically:**
1. Pre-push hook runs verification
2. Detects release branch
3. Asks for release details (interactive) or uses flags
4. Creates git tags: `go-v1.2.3`, `rust-v1.2.3`, etc
5. Push completes
6. Post-push hook cleans up state
7. GitHub Actions detects tags
8. Publishes to registries: crates.io, npm, PyPI, NuGet

### Step 4: Monitor Publishing

GitHub Actions automatically:
- Detects the release tags
- Sets `IS_RELEASE=true`
- Publishes to production registries
- Creates GitHub release with artifacts

## Advanced Usage

### Auto-Detect Version

Analyzes commits to suggest release type:

```bash
./scripts/release.sh --auto
```

### Partial Releases

Release only specific languages:

```bash
git push origin release/1.2.3
# When prompted: "Languages [all]: go,rust"
# Only Go and Rust get tagged
```

### JSON Output

For parsing in scripts:

```bash
./scripts/release.sh \
  --version 1.2.3 \
  --type minor \
  --json
```

Output:
```json
{
  "status": "success",
  "message": "Release tags created",
  "data": {
    "version": "1.2.3",
    "type": "minor",
    "languages": "python,go,javascript,csharp,rust,java,cpp"
  }
}
```

## Git State Files

The following files are gitignored (local only):

| File | Purpose | Auto-Deleted |
|------|---|---|
| `.env` | Publishing tokens | Manual |
| `.release.json` | Release state | Yes (post-push) |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Error |
| 2 | Cancelled by user |

## Versions Tracking

CodeUChain uses **VERSIONS.json** to track the current version of each language implementation.

### Version File Structure

```json
{
  "versions": {
    "python": {
      "version": "1.0.0",
      "registry": "PyPI",
      "packageName": "codeuchain"
    },
    "go": {
      "version": "1.0.0",
      "registry": "pkg.go.dev",
      "packageName": "github.com/codeuchain/codeuchain/packages/go"
    },
    "rust": {
      "version": "1.0.0",
      "registry": "crates.io",
      "packageName": "codeuchain"
    }
  }
}
```

### Independent Versions

Each language maintains its own version:
- **python**: 1.0.0
- **go**: 1.0.0
- **javascript**: 1.1.1 (ahead of others)
- **rust**: 1.0.0
- **java**: 0.1.0 (still in development)

### Updating Versions

Versions are tracked centrally in `VERSIONS.json` for reference, but the source of truth for each package is:
- **Python**: `packages/python/setup.py`
- **Go**: `packages/go/go.mod`
- **JavaScript**: `packages/javascript/package.json`
- **C#**: `packages/csharp/*.csproj`
- **Rust**: `packages/rust/Cargo.toml`
- **Java**: `packages/java/pom.xml`
- **C++**: `packages/cpp/CMakeLists.txt`

The release script can be extended to auto-update these files during release.

## Changelog Management

The release script automatically manages `CHANGELOG.md` using Keep a Changelog format.

### Automatic Changelog Updates

When you run a release, the script:
1. Generates a changelog entry for the new version
2. Inserts it under the `## [Unreleased]` section
3. Formats using Keep a Changelog standard
4. Creates a git commit: `chore: update changelog for vX.Y.Z`
5. Tags are created AFTER the changelog commit

### Changelog Format

```markdown
# Changelog

## [Unreleased]

## [1.2.3] - 2026-01-19

### Changed
  - Release version 1.2.3

## [1.2.2] - 2026-01-18

### Added
  - New feature description

### Fixed
  - Bug fix description
```

### Viewing Changes

```bash
# See what was committed
git log --oneline -5

# Sample output:
# a1b2c3d (tag: go-v1.2.3) chore: update changelog for v1.2.3
# b2c3d4e Add feature X
# c3d4e5f Fix bug Y

# View the changelog
cat CHANGELOG.md | head -20
```

### Conventional Commits (Optional)

The changelog generation supports conventional commit parsing. While not required, using them improves the changelog:

```bash
git commit -m "feat(parser): add support for new syntax"
git commit -m "fix(core): resolve memory leak"
git commit -m "BREAKING CHANGE: removed legacy API"
```

## Troubleshooting

### Pre-push hook not running

```bash
# Verify hook is executable
ls -la .git/hooks/pre-push
chmod +x .git/hooks/pre-push
```

### Release branch not detected

```bash
# Valid branches for release:
# - main, master
# - release/*
# - hotfix/*

# Example:
git checkout -b release/1.2.3 main  # ✓ Works
git checkout -b feature/xyz main     # ✗ Not recognized
```

### Tag already exists

```bash
# View existing tags
git tag -l

# Delete old tag if needed
git tag -d go-v1.2.3
git push origin --delete go-v1.2.3
```

### State stuck

```bash
# Manual cleanup if needed
rm .release.json
git status  # Should show clean
```

## Integration Points

### Pre-Push Hook
- Runs: Before push to any release branch
- Actions: Validates, creates config, prompts user
- Output: `.release.json` config file

### Release Script
- Called by: Pre-push hook or manually
- Creates: Git tags for all languages
- Updates: `.release.json` with status

### Post-Push Hook
- Runs: After successful push
- Actions: Cleans up `.release.json`
- Result: State reset for next release

### GitHub Actions
- Triggered by: Git tags
- Detects: `IS_RELEASE=true`
- Actions: Publishes to registries

## Examples

### Example 1: Interactive Human Release

```bash
# Create branch
git checkout -b release/2.0.0 main

# Make changes
echo "2.0.0" > VERSION
git commit -am "chore: version 2.0.0"

# Push (triggers interactive prompt)
git push origin release/2.0.0

# User responds to prompts
# ✓ Tags created automatically
# ✓ GitHub Actions publishes
```

### Example 2: CI/CD Pipeline

```bash
#!/bin/bash
VERSION="1.2.3"
TYPE="minor"
LANGS="go,rust"

git checkout -b release/$VERSION main
git push -u origin release/$VERSION

./scripts/release.sh \
  --version $VERSION \
  --type $TYPE \
  --languages $LANGS \
  --no-interactive

git push origin --tags
# GitHub Actions automatically publishes
```

### Example 3: Dry Run Preview

```bash
./scripts/release.sh \
  --version 1.2.3 \
  --type patch \
  --dry-run

# Output shows what would happen
# No actual tags created
```

## See Also

- [Universal CI Configuration](../universal-ci.config.json)
- [GitHub Actions Workflow](.github/workflows/universal-ci.yml)
- [Publishing Secrets Setup](../README.md#publishing-secrets)
