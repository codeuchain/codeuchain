# Quick Start - Release Automation

## TL;DR

```bash
# Create release branch
git checkout -b release/1.2.3 main

# Make final changes (optional)
git commit -am "fix: final tweaks"

# Push - automation handles everything
git push origin release/1.2.3

# Select release type when prompted (or use flags for CI)
# ✓ Tags created automatically
# ✓ GitHub Actions publishes to registries
```

## For Humans

```bash
# Interactive mode - just answer questions
./scripts/release.sh

# Shows:
# ℹ CodeUChain Release Automation
# 
# Release branch [release/1.2.3]: 
# Languages [python,go,javascript,csharp,rust,java,cpp]: go,rust
# Release type [minor]: patch
# Version [1.0.1]: 1.0.1
# 
# Proceed with release? [yes/no]: yes
# 
# ✓ Release tags created
```

## For AI/Automation

```bash
# Non-interactive - all flags provided
./scripts/release.sh \
  --version 1.2.3 \
  --type minor \
  --languages go,rust \
  --no-interactive

# Returns exit code 0 on success, never hangs
```

## Preview Before Committing

```bash
# Dry run - see what would happen
./scripts/release.sh \
  --version 1.2.3 \
  --type patch \
  --dry-run

# DRY RUN: Would create go-v1.2.3
# DRY RUN: Would create rust-v1.2.3
```

## Full CLI Help

```bash
./scripts/release.sh --help
```

## Release Types

- **major**: Breaking changes (1.0.0 → 2.0.0)
- **minor**: New features (1.0.0 → 1.1.0)  
- **patch**: Bug fixes (1.0.0 → 1.0.1)
- **hotfix**: Emergency fixes (1.0.0 → 1.0.1)

## What Happens Automatically

```
1. Pre-push hook detects release branch
2. Asks for release details (or uses flags)
3. Updates CHANGELOG.md (Keep a Changelog format)
4. Commits changelog: chore: update changelog for v1.2.3
5. Creates git tags:
   - python-v1.2.3
   - go-v1.2.3
   - javascript-v1.2.3
   - (etc for all languages)
6. Saves state to .release.json
7. Push completes
8. Post-push hook deletes .release.json
9. GitHub Actions detects tags
10. Publishes to registries:
    - Rust → crates.io
    - Python → PyPI
    - JS → npm
    - C# → NuGet
```

### Changelog

- **Automatically generated** in Keep a Changelog format
- **Inserted** under `## [Unreleased]` section
- **Committed** before tagging
- **Tracked** in git history

View the changelog:
```bash
cat CHANGELOG.md | head -20
```

## File Notes

- `.release.json` - Created during release, deleted after push (gitignored)
- `.env` - Local tokens file (gitignored, keep safe)

## Common Issues

**"Not on a release branch"**
- Valid branches: `main`, `master`, `release/*`, `hotfix/*`
- Current branch must match one of these

**"Tag already exists"**
- This version was already released
- Use a different version number

**Pre-push hook not running**
```bash
# Make sure it's executable
chmod +x .git/hooks/pre-push
chmod +x .git/hooks/post-push
```

## Examples

### Example 1: Simple Release (Interactive)
```bash
git checkout -b release/2.0.0 main
git push -u origin release/2.0.0
# Answer prompts
# Done!
```

### Example 2: Specific Languages (Interactive)
```bash
git push origin release/1.5.0
# When asked languages: go,rust
# Only Go and Rust get tagged
```

### Example 3: CI/CD Pipeline
```bash
scripts/release.sh \
  --version $VERSION \
  --type $TYPE \
  --languages $LANGS \
  --no-interactive \
  --json
```

### Example 4: Preview
```bash
scripts/release.sh \
  --version 1.0.1 \
  --type patch \
  --languages go \
  --dry-run
```

## Got Questions?

- Full docs: `RELEASE.md`
- Architecture: `docs/RELEASE_FLOW.md`
- Implementation: `IMPLEMENTATION_SUMMARY.md`
- Help: `scripts/release.sh --help`

---

**That's it!** Push to a release branch and let the automation handle the rest. 🚀
