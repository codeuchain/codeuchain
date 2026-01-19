# CodeUChain Release Automation System - Complete Flow

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     Developer/Agent on Release Branch                    │
│                         (release/X.Y.Z)                                 │
└─────────────────────────┬───────────────────────────────────────────────┘
                          │
                          │ git push origin release/X.Y.Z
                          ▼
        ┌─────────────────────────────────────┐
        │   🔍 PRE-PUSH HOOK TRIGGERS         │
        │  (.git/hooks/pre-push)               │
        │                                     │
        │  1. Run ./verify.sh (tests/lint)   │
        │  2. Detect release branch           │
        │  3. Check for .release.json         │
        │  4. If missing: call release.sh    │
        └─────────────────────────────────────┘
                          │
        ┌─────────────────┴──────────────────┐
        │                                    │
        ▼ Interactive Mode                ▼ CI/Agent Mode
   (Human input via TTY)           (All flags provided)
        │                                    │
  ┌─────────────────────┐         ┌─────────────────────┐
  │ Prompt for:         │         │ Use provided args:  │
  │ - Languages         │         │ --version 1.2.3    │
  │ - Release type      │         │ --type minor       │
  │ - Version           │         │ --languages go     │
  │ - Confirm           │         │ --no-interactive   │
  └────────────┬────────┘         └────────────┬───────┘
               │                               │
               └───────────────┬───────────────┘
                               │
                               ▼
        ┌─────────────────────────────────────┐
        │  📝 scripts/release.sh executes      │
        │                                     │
        │  1. Validate inputs                │
        │  2. Create git tags:               │
        │     - python-v1.2.3               │
        │     - go-v1.2.3                   │
        │     - javascript-v1.2.3           │
        │     - (etc for all languages)     │
        │  3. Save .release.json            │
        └────────────┬────────────────────────┘
                     │
                     ▼
        ┌─────────────────────────────────────┐
        │  ✓ Tags created (local)            │
        │  ✓ .release.json exists            │
        └────────────┬────────────────────────┘
                     │
                     │ git push origin --tags
                     ▼
        ┌─────────────────────────────────────┐
        │  🌐 GitHub receives push            │
        │                                     │
        │  - Tags pushed to remote           │
        │  - Pre-push exit(0)                │
        │  - Push succeeds                   │
        └────────────┬────────────────────────┘
                     │
                     ▼
        ┌─────────────────────────────────────┐
        │  ✨ POST-PUSH HOOK TRIGGERS         │
        │  (.git/hooks/post-push)             │
        │                                     │
        │  1. Delete .release.json           │
        │  2. Reset state                    │
        │  3. Show completion message        │
        └────────────┬────────────────────────┘
                     │
        ┌────────────┴────────────────────────┐
        │                                     │
        ▼ (Parallel)                       ▼
    Local State Reset          GitHub detects tags
    ✓ .release.json deleted    (IS_RELEASE=true)
    ✓ Ready for next release       │
                                   ▼
                      ┌─────────────────────────────────┐
                      │   🚀 GitHub Actions CI/CD        │
                      │  (.github/workflows/universal-ci)│
                      │                                 │
                      │  1. Run tests                   │
                      │  2. Set IS_RELEASE=true         │
                      │  3. Publish to registries:      │
                      │     - crates.io (Rust)          │
                      │     - npm (JavaScript)          │
                      │     - PyPI (Python)             │
                      │     - NuGet (C#)                │
                      │  4. Create GitHub release       │
                      │  5. Upload artifacts            │
                      └─────────────────────────────────┘
                                   │
                                   ▼
                      ┌─────────────────────────────────┐
                      │  ✅ Package Release Complete    │
                      │                                 │
                      │  All languages published        │
                      │  GitHub release created         │
                      │  Ready for users                │
                      └─────────────────────────────────┘
```

## State Transitions

```
                    ┌──────────────────────┐
                    │  No Release Config   │
                    │  (.release.json)     │
                    └──────────┬───────────┘
                               │
            ┌──────────────────┴──────────────────┐
            │                                     │
            ▼ Interactive or flags provided      │
   (release.sh called)                           │
            │                                     │
            └──────────────────┬──────────────────┘
                               │
                               ▼
                    ┌──────────────────────┐
                    │  Release Config      │
                    │  Created             │
                    │  (.release.json)     │
                    │                      │
                    │ {                    │
                    │  version: "1.2.3",   │
                    │  type: "minor",      │
                    │  languages: "go,...",│
                    │  branch: "release/..." │
                    │ }                    │
                    └──────────┬───────────┘
                               │
                    (pre-push continues)
                    (git push origin)
                               │
                               ▼
                    ┌──────────────────────┐
                    │  Tags Pushed         │
                    │  to Remote           │
                    └──────────┬───────────┘
                               │
                      (post-push hook runs)
                               │
                               ▼
                    ┌──────────────────────┐
                    │  Release Config      │
                    │  Deleted             │
                    │  (.release.json)     │
                    │  removed             │
                    └──────────┬───────────┘
                               │
                               ▼
                    ┌──────────────────────┐
                    │  State Reset         │
                    │  Ready for next      │
                    │  release             │
                    └──────────────────────┘
```

## File Organization

```
.codeuchain/
├── .git/
│   └── hooks/
│       ├── pre-push          ← Triggers release automation
│       └── post-push         ← Cleans up release state
│
├── scripts/
│   ├── release.sh            ← Main release automation
│   ├── create_release_archives.sh
│   ├── upload_release_assets.sh
│   └── ...
│
├── .env                       ← Publishing tokens (gitignored)
├── .release.json              ← Release state (gitignored, auto-deleted)
├── .gitignore                 ← Includes .env, .release.json
└── RELEASE.md                 ← This documentation
```

## Decision Tree

```
        Is this a release branch?
        (release/*, hotfix/*)
              │
        ┌─────┴─────┐
        │           │
       NO          YES
        │           │
        ▼           ▼
   Skip      Is .release.json present?
   release       │
        ┌────────┴────────┐
        │                 │
       NO                YES
        │                 │
        ▼                 ▼
   Call release.sh   Use existing config
        │                 │
        ├─ Interactive ◄──┘
        │  or
        ├─ Non-interactive
        │
        ▼
   Create git tags
        │
        ▼
   Save .release.json
        │
        ▼
   Continue with push
        │
        ▼
   Post-push: Clean up
        │
        ▼
   GitHub Actions detects tags
        │
        ▼
   Publish to registries
```

## Timing and Parallel Operations

```
Timeline:
─────────────────────────────────────────────────────────────

T0: Developer runs git push origin release/X.Y.Z
    │
    ├─ Pre-push hook (blocking)
    │  ├─ verify.sh (tests, lint)
    │  └─ release.sh (tags creation)
    │
    └─► Git push (blocking until pre-push succeeds)

T1: Push completes successfully
    │
    ├─ Post-push hook (background)
    │  └─ Cleanup (.release.json removal)
    │
    └─► Local: State reset, ready for next release

T2: GitHub receives push
    │
    ├─ GitHub Actions detects tags
    │
    └─► CI/CD starts (parallel to local operations)

T3: GitHub Actions completes
    │
    ├─ Tests passed
    ├─ IS_RELEASE=true set
    ├─ Publish to crates.io
    ├─ Publish to npm
    ├─ Publish to PyPI
    ├─ Publish to NuGet
    └─► Release complete for users
```

## Exit Scenarios

```
Scenario 1: Successful Release
──────────────────────────────
push origin
  → pre-push: verification OK
  → release.sh: tags created ✓
  → .release.json saved
  → git push succeeds
  → post-push: cleanup
  → GitHub Actions publishes ✓

Scenario 2: Verification Failed
────────────────────────────────
push origin
  → pre-push: verification FAILED ✗
  → Push blocked
  → User fixes issues
  → retry push

Scenario 3: User Cancels
────────────────────────
push origin release/X.Y.Z
  → pre-push: asks for confirmation
  → User selects: "skip" or "no"
  → .release.json NOT created
  → Push continues WITHOUT release tags
  → No tags pushed to GitHub

Scenario 4: Tags Already Exist
───────────────────────────────
push origin release/X.Y.Z
  → release.sh detects: python-v1.2.3 exists
  → Fails with error
  → Push blocked
  → User must delete old tag or use different version

Scenario 5: AI Agent (Non-Interactive)
─────────────────────────────────────
./scripts/release.sh \
  --version 1.2.3 --type minor --no-interactive
  → No prompts
  → Creates tags immediately
  → Saves .release.json
  → Exits with status 0
```

## Security & State

```
.release.json (gitignored - local only)
├─ Version: stored locally
├─ Type: stored locally
├─ Languages: stored locally
├─ Created timestamp: local reference
└─ Automatically deleted after push

.env (gitignored - local only)
├─ CARGO_REGISTRY_TOKEN: never committed
├─ NPM_TOKEN: never committed
├─ NUGET_API_KEY: never committed
└─ TWINE_PASSWORD: never committed

GitHub Secrets (secure storage)
├─ CARGO_REGISTRY_TOKEN: used in Actions
├─ NPM_TOKEN: used in Actions
├─ NUGET_API_KEY: used in Actions
└─ TWINE_PASSWORD: used in Actions
```

## Integration Points

```
Developer
    │
    ├─ Interactive: TTY input
    ├─ Non-interactive: CLI flags
    └─ CI/Agent: Full automation
         │
         ▼
    Pre-Push Hook
         │
         ├─ Verification (tests, lint)
         ├─ Release detection
         ├─ Config creation
         └─ Tag generation
             │
             ▼
         Release Script
             │
             ├─ Input validation
             ├─ Tag creation
             ├─ State tracking
             └─ Config persistence
                 │
                 ▼
             Git Tags (local)
                 │
                 ▼
             Push to GitHub
                 │
                 ▼
         Post-Push Hook
             │
             └─ State cleanup
                 │
                 ▼
         GitHub (remote)
             │
             ├─ Webhook triggers
             └─ Actions CI/CD
                 │
                 ▼
         GitHub Actions
             │
             ├─ Verify tests (IS_RELEASE=true)
             ├─ Publish to crates.io
             ├─ Publish to npm
             ├─ Publish to PyPI
             ├─ Publish to NuGet
             └─ Create GitHub release
```
