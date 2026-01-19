---
title: CodeUChain Monorepo Maintenance Guide
description: Comprehensive guide for maintaining and developing the CodeUChain polyglot monorepo
version: 1.0.0
created: 2026-01-18
maintained_by: git (see releases and commit history)
---

# CodeUChain Monorepo Maintenance Guide

> **A living document that defines how we maintain, develop, and evolve the CodeUChain polyglot monorepo with consistency, quality, and excellence.**

## 📖 Table of Contents

- [Overview](#overview)
- [Core Philosophy](#core-philosophy)
- [CodeUChain Fundamentals](#codeuchain-fundamentals)
- [Monorepo Structure](#monorepo-structure)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing Strategy](#testing-strategy)
- [CI/CD & Local Testing](#cicd--local-testing)
- [Release Management](#release-management)
- [Typed Features Guidelines](#typed-features-guidelines)
- [Documentation Standards](#documentation-standards)
- [Troubleshooting Common Issues](#troubleshooting-common-issues)
- [Maintenance Checklist](#maintenance-checklist)

---

## Overview

CodeUChain is a polyglot monorepo providing a universal framework for composable software across 8+ programming languages. This guide ensures that all contributions, maintenance, and evolution follow consistent patterns while respecting language-specific idioms.

### Key Facts
- **Type**: Polyglot Monorepo
- **Languages**: Go, Python, JavaScript/TypeScript, C#, Rust, Java, C++, COBOL (meme), Pseudocode
- **Architecture**: Context-Link-Chain pattern
- **License**: Apache 2.0
- **Status**: Production-ready in core languages

### Document Purpose
This guide serves as the **single source of truth** for:
- How we structure code across languages
- How we test and validate changes
- How we release and maintain packages
- How we enforce quality and consistency

---

## Core Philosophy

### Universal Principles

#### 1. **User-Centric Focus**
> "Delight the user; resolve headaches before they reach them"

We put ourselves in the user's shoes and proactively eliminate pain points through comprehensive testing and thoughtful design.

#### 2. **Consistency Across Languages**
Same mental models, universal patterns, language-specific idioms. A developer familiar with CodeUChain in one language should feel at home in any other.

#### 3. **Test-Driven Development**
We follow a layered testing approach with a **"trust but verify"** mentality:
- **Unit Tests**: Isolate components with mocked dependencies
- **Integration Tests**: Test component interactions with mocked services
- **End-to-End Tests**: Validate with real services, real data, real infrastructure

#### 4. **Local-First Development**
Use `act` for GitHub Actions testing locally before pushing. No more trial-and-error CI/CD cycles.

#### 5. **CodeUChain in All Projects**
We use CodeUChain for all projects. This ensures we experience our own framework and understand developer pain points.

#### 6. **Opt-In Complexity**
Start simple, add features when needed. Typing, advanced orchestration, and tooling are available but never required.

---

## CodeUChain Fundamentals

Before diving into maintenance, understand the core concepts that define CodeUChain:

### Universal Pseudocode Pattern

```pseudocode
// 1. Setup Data
ctx = new Context({ id: 123, raw_text: "  hello  " })

// 2. Build Workflow
chain = new Chain()
    .add(Link("clean", (ctx) -> trim(ctx.raw_text)))
    .add(Link("verify", (ctx) -> if empty(ctx.raw_text) throw error))
    .add(Link("save", (ctx) -> database.save(ctx.raw_text)))

// 3. Run
result = chain.run(ctx)

// 4. Check Result
if result.hasError():
    handleFailure(result.error)
else:
    showSuccess(result.get("saved_id"))
```

### Core Concepts

#### **Context**
The "box" moving down the conveyor belt:
- Immutable key-value data structure
- Carries state through the chain
- Creates new instances instead of mutating
- Thread-safe and predictable

#### **Link**
A "station" on the belt:
- Individual processing unit with single responsibility
- Accepts Context → Returns modified Context
- One well-defined purpose
- Sync or async (framework handles both)

#### **Chain**
The "conveyor belt" itself:
- Ordered sequence of Links
- Manages Context flow between Links
- Handles error propagation automatically
- Provides orchestration capabilities

#### **Middleware**
Observes and reacts to execution:
- Operates outside main flow in parallel observation context
- Monitors execution without modifying business logic context
- Handles cross-cutting concerns (logging, metrics, caching, validation)
- Pure observation layer - cannot interfere with Link logic
- Clean separation preserves business logic integrity

### Mental Model Alignment

```
Problem → Analysis → Solution → Verification → Refinement
    ↓         ↓          ↓            ↓              ↓
 Context → Link 1 → Link 2 →    Link 3   →     Link 4
```

This sequential, composable nature matches how humans think and how AI agents reason.

---

## Monorepo Structure

### Directory Layout

```
codeuchain/
├── .github/                    # GitHub workflows, templates, instructions
│   ├── workflows/              # CI/CD automation
│   ├── ISSUE_TEMPLATE/         # Issue templates
│   ├── PULL_REQUEST_TEMPLATE.md
│   └── instructions/           # Internal guidelines
├── docs/                       # Documentation and website
│   ├── assets/                 # Images, logos
│   ├── components/             # HTML components
│   ├── [language]/             # Language-specific docs
│   └── index.html              # Main website
├── packages/                   # Language implementations
│   ├── python/                 # Reference implementation
│   ├── go/                     # Production-ready (97.5% coverage)
│   ├── javascript/             # JS/TS implementation
│   ├── csharp/                 # C# implementation
│   ├── rust/                   # Rust implementation
│   ├── java/                   # In development
│   ├── cpp/                    # C++ implementation
│   ├── pseudo/                 # Conceptual foundation
│   └── cobol/                  # Meme implementation
├── releases/                   # Packaged releases
├── scripts/                    # Build and release automation
├── CODING_STANDARDS.md         # Enforced coding standards
├── MAINTENANCE_GUIDE.md        # This document
├── README.md                   # Project overview
└── TYPED_FEATURES_IMPLEMENTATION_PLAN.md  # Typed features roadmap
```

### Package Structure (Per Language)

Each language implementation follows a consistent structure:

```
packages/[language]/
├── src/ or lib/               # Source code
├── tests/                     # Comprehensive test suite
├── examples/                  # Working examples
├── docs/                      # Language-specific documentation
├── README.md                  # Language-specific guide
├── LICENSE                    # Apache 2.0
└── [build files]              # Language-specific build configuration
```

### Key Files

- **CODING_STANDARDS.md**: Enforced linting rules across all languages
- **TYPED_FEATURES_IMPLEMENTATION_PLAN.md**: Opt-in generics implementation status
- **go.work**: Go workspace configuration for multi-module support
- **codeuchain.code-workspace**: VS Code workspace configuration

---

## Development Workflow

### Setting Up Your Environment

1. **Clone the repository**
   ```bash
   git clone https://github.com/codeuchain/codeuchain.git
   cd codeuchain
   ```

2. **Install `act` for local workflow testing**
   ```bash
   # macOS
   brew install act
   
   # Linux
   curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
   ```

3. **Choose your language and navigate**
   ```bash
   cd packages/[language]
   ```

4. **Install dependencies**
   Follow language-specific instructions in `packages/[language]/README.md`

### Making Changes

#### 1. **Create a Feature Branch**
```bash
git checkout -b feature/your-feature-name
```

#### 2. **Write Code Following Standards**
- Organize Links in separate files
- Organize Chains in separate files
- Keep methods focused and single-purpose
- Follow language-specific naming conventions (see [CODING_STANDARDS.md](CODING_STANDARDS.md))

#### 3. **Write Tests (TDD)**

**RED PHASE**: Write failing test first
```typescript
// example: tests/validate_email.test.ts
test('ValidateEmail should reject invalid emails', () => {
  const ctx = new Context({ email: 'invalid' });
  expect(() => validateEmail.call(ctx)).toThrow();
});
```

**GREEN PHASE**: Implement to pass test
```typescript
// example: src/links/validate_email.ts
export const validateEmail = new Link('validate_email', (ctx) => {
  const email = ctx.get('email');
  if (!email.includes('@')) {
    throw new Error('Invalid email format');
  }
  return ctx;
});
```

#### 4. **Test Locally Before Pushing**

Run unit tests:
```bash
# Language-specific commands
npm test              # JavaScript/TypeScript
pytest                # Python
go test ./...         # Go
dotnet test           # C#
cargo test            # Rust
```

Run GitHub Actions locally with `act`:
```bash
# Test all workflows
act -l

# Run specific workflow
act -j [job-name]
```

#### 5. **Lint Your Code**

**Direct language linting** (recommended for development):
```bash
# JavaScript/TypeScript
npx eslint packages/javascript/ --config packages/javascript/.eslintrc.json

# Python
pylint packages/python/

# Go
golangci-lint run ./packages/go/...

# C#
dotnet format
```

**MegaLinter** (for comprehensive checks):
```bash
# Run MegaLinter locally
mega-linter-runner

# Note: MegaLinter has known issues with Jest globals in JS tests.
# Use direct ESLint for JavaScript linting.
```

#### 6. **Update Documentation**

If your change affects:
- **Public APIs**: Update language-specific README
- **Core concepts**: Update `docs/pseudo/README.md`
- **Typed features**: Update `TYPED_FEATURES_IMPLEMENTATION_PLAN.md`
- **Standards**: Update `CODING_STANDARDS.md`

#### 7. **Commit and Push**

```bash
git add .
git commit -m "feat: your descriptive commit message"
git push origin feature/your-feature-name
```

#### 8. **Create Pull Request**

Follow the PR template and ensure:
- [ ] Tests pass locally
- [ ] Linting passes
- [ ] Documentation updated
- [ ] Examples provided if needed
- [ ] Follows coding standards

---

## Coding Standards

Comprehensive standards are defined in [CODING_STANDARDS.md](CODING_STANDARDS.md). Key highlights:

### Global Standards

| Standard | Requirement |
|----------|-------------|
| **Line Length** | 100 characters max |
| **Indentation** | 4 spaces (2 for JS/TS) |
| **Naming** | `PascalCase` for classes, language-specific for functions |
| **Documentation** | Full documentation for public APIs |
| **Error Handling** | Explicit, meaningful error messages |

### CodeUChain-Specific Standards

#### File Organization
✅ **DO**: Keep Links and Chains in separate files
```
src/
├── links/
│   ├── validate_email.ts
│   ├── hash_password.ts
│   └── save_user.ts
└── chains/
    └── user_registration.ts
```

❌ **DON'T**: Mix multiple concerns in one file
```
src/
└── user_stuff.ts  // Contains links, chains, utilities
```

#### Link Design
✅ **DO**: Single responsibility, clear names
```python
class ValidateEmailLink(Link):
    """Validates email format using regex."""
    async def call(self, ctx: Context) -> Context:
        email = ctx.get("email")
        if not EMAIL_REGEX.match(email):
            raise ValueError("Invalid email format")
        return ctx
```

❌ **DON'T**: Multiple responsibilities
```python
class ProcessUserLink(Link):
    """Does validation, hashing, and saving."""  # Too much!
    async def call(self, ctx: Context) -> Context:
        # validation code
        # hashing code
        # database code
        # email code
        return ctx
```

### Language-Specific Standards

See [CODING_STANDARDS.md](CODING_STANDARDS.md) for detailed language-specific rules including:
- JavaScript/TypeScript: Airbnb style, async/await, ES6 modules
- Python: PEP 8, Google-style docstrings, type hints
- Go: Standard gofmt, godoc comments
- C#: Microsoft conventions, XML docs
- Rust: rustfmt, rustdoc
- Java: Google Java Style
- C++: Google C++ Style

---

## Testing Strategy

### Layered Testing Approach

We implement a three-tier testing strategy:

#### 1. **Unit Testing** (Isolation)
**Purpose**: Test individual components in complete isolation
**Environment**: All dependencies mocked
**Data**: Synthetic test data
**Goal**: Verify component logic correctness

```typescript
// Example: Unit test for ValidateEmail link
describe('ValidateEmail', () => {
  it('should accept valid email', () => {
    const ctx = new Context({ email: 'test@example.com' });
    const result = validateEmail.call(ctx);
    expect(result.get('email')).toBe('test@example.com');
  });

  it('should reject invalid email', () => {
    const ctx = new Context({ email: 'invalid' });
    expect(() => validateEmail.call(ctx)).toThrow('Invalid email');
  });
});
```

#### 2. **Integration Testing** (Component Interaction)
**Purpose**: Test components working together
**Environment**: Mocked external services (APIs, databases)
**Data**: Synthetic test data
**Goal**: Verify interfaces and data transformations

```typescript
// Example: Integration test for UserRegistration chain
describe('UserRegistration Chain', () => {
  it('should process valid user registration', async () => {
    const mockDb = new MockDatabase();
    const chain = new Chain()
      .add(validateEmail)
      .add(hashPassword)
      .add(saveToDatabase(mockDb));

    const ctx = new Context({ 
      email: 'test@example.com', 
      password: 'secure123' 
    });

    const result = await chain.execute(ctx);
    
    expect(result.get('userId')).toBeDefined();
    expect(mockDb.users).toHaveLength(1);
  });
});
```

#### 3. **End-to-End Testing** (Full System)
**Purpose**: Test complete system with real services
**Environment**: Real external services
**Data**: Real data (cleaned up after)
**Goal**: Verify production-like behavior

```typescript
// Example: E2E test hitting real GitHub API
describe('GitHub Integration E2E', () => {
  let testRepoName: string;

  afterEach(async () => {
    // Cleanup: Delete test repository
    await github.repos.delete({ owner: 'test-org', repo: testRepoName });
  });

  it('should create and configure real repository', async () => {
    testRepoName = `test-repo-${Date.now()}`;
    
    const chain = new Chain()
      .add(createRepo)
      .add(configureSettings)
      .add(addCollaborators);

    const ctx = new Context({ 
      name: testRepoName, 
      org: 'test-org' 
    });

    const result = await chain.execute(ctx);
    
    // Verify with direct GitHub API call
    const repo = await github.repos.get({ 
      owner: 'test-org', 
      repo: testRepoName 
    });
    
    expect(repo.data.name).toBe(testRepoName);
    expect(repo.data.private).toBe(true);
  });
});
```

### Trust But Verify Mentality

**Trust**: Our code reports what it does
**Verify**: We confirm with external checks

```typescript
// Trust: Our code says it created a webhook
result.get('webhookCreated'); // true

// Verify: Query GitHub API to confirm webhook exists
const webhooks = await github.repos.listWebhooks({ owner, repo });
expect(webhooks.data).toContainEqual(
  expect.objectContaining({ config: { url: expectedUrl } })
);
```

### Coverage Requirements

| Language | Current Coverage | Goal | Status |
|----------|------------------|------|--------|
| Go | 97.5% | 95%+ | ✅ Achieved |
| Python | Comprehensive | 90%+ | ✅ Achieved |
| JavaScript/TS | Comprehensive | 90%+ | ✅ Achieved |
| C# | Comprehensive | 90%+ | ✅ Achieved |
| Rust | Comprehensive | 90%+ | ✅ Achieved |
| Java | In Development | 90%+ | 🚧 Planned |

### Test Organization

```
tests/
├── unit/                      # Isolated component tests
│   ├── links/
│   ├── chains/
│   └── context/
├── integration/               # Component interaction tests
│   ├── chains/
│   └── middleware/
└── e2e/                       # Full system tests
    ├── github_integration/
    └── real_world_scenarios/
```

---

## CI/CD & Local Testing

### GitHub Actions Workflows

Located in `.github/workflows/`:

| Workflow | Purpose | Trigger |
|----------|---------|---------|
| `package-cpp-release.yml` | Build, test, package C++ releases | Tag push (cpp-v*) |
| `publish_release_assets.yml` | Upload release archives to GitHub | Tag push |
| `conan-center-publish.yml` | Publish C++ to Conan Center | Release |

### Local Testing with `act`

**Why**: Eliminates trial-and-error remote CI testing, provides immediate feedback

#### Installation
```bash
# macOS
brew install act

# Linux
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
```

#### Usage
```bash
# List all available workflows
act -l

# Run all jobs in a workflow
act

# Run specific job
act -j [job-name]

# Run with secrets (if needed)
act -s GITHUB_TOKEN=your_token

# Dry run (see what would be executed)
act -n
```

#### Example: Testing Release Workflow Locally
```bash
# Test C++ release packaging
act -j package-cpp-release

# Test with specific event
act push -e test-event.json
```

### Pre-Push Checklist

Before pushing to GitHub:
- [ ] Tests pass locally
- [ ] Linting passes
- [ ] `act` workflow tests pass
- [ ] Documentation updated
- [ ] Examples run successfully

---

## Release Management

### Release Process Overview

1. **Prepare Release**
   - Update version numbers
   - Update changelogs
   - Test all language implementations
   - Validate examples

2. **Create Release Tag**
   ```bash
   # Format: [language]-v[version]
   git tag python-v1.0.0
   git tag go-v1.0.0
   git push origin --tags
   ```

3. **Automated Packaging**
   - GitHub Actions trigger on tag push
   - Scripts in `scripts/` execute:
     - `create_release_archives.sh`: Create tar.gz and zip archives
     - `upload_release_assets.sh`: Upload to GitHub releases

4. **Manual Validation**
   - Download release assets
   - Test installation in clean environment
   - Verify package integrity

5. **Publish to Package Managers**
   - NPM (JavaScript)
   - PyPI (Python)
   - Cargo (Rust)
   - NuGet (C#)
   - Maven Central (Java)
   - Conan Center (C++)

### Release Scripts

Located in `scripts/`:

```bash
# Create all release archives
./scripts/create_release_archives.sh

# Package all languages for release
./scripts/package_all_releases.sh

# Upload assets to GitHub release by tag
./scripts/upload_release_assets_by_tag.sh [tag-name]

# Clean release artifacts
./scripts/clean_release_artifacts.sh
```

### Version Management

**Format**: Semantic Versioning (MAJOR.MINOR.PATCH)
- **MAJOR**: Breaking changes
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes, backward compatible

**Example**:
- `v1.0.0`: Initial stable release
- `v1.1.0`: Added typed features (backward compatible)
- `v1.1.1`: Fixed bug in Context.get() (patch)
- `v2.0.0`: Changed Link interface (breaking change)

### Changelog Management

**Location**: Git tags and GitHub releases
**Format**: Automated changelog generation from commits

Follow conventional commit format:
```
feat: add typed context evolution
fix: resolve memory leak in chain execution
docs: update installation instructions
test: add comprehensive e2e tests
```

---

## Typed Features Guidelines

CodeUChain provides **opt-in generics** for static type safety while maintaining runtime flexibility.

### Core Concepts

#### Generic Link Interface
```pseudocode
Link[Input, Output]
  - call(ctx: Context[Input]) -> Context[Output]
  - Transforms data from Input shape to Output shape
  - Compile-time type checking
  - Runtime flexibility maintained
```

#### Generic Context
```pseudocode
Context[T]
  - insert(key, value) -> Context[T]      // Preserve type
  - insert_as(key, value) -> Context[U]   // Evolve type
  - get(key) -> value
  - Runtime storage: Dict[str, Any]
```

### Implementation Status

| Language | Status | Features |
|----------|--------|----------|
| Python | ✅ Complete | TypedDict, covariance, insert_as() |
| Go | ✅ Complete | Generics (1.18+), InsertAs[U]() |
| TypeScript | ✅ Complete | Structural typing, insertAs<U>() |
| C# | ✅ Complete | Covariant generics, InsertAs<U>() |
| Rust | ✅ Complete | Trait-based generics, insert_as() |
| Java | 🚧 Planned | Wildcards, type erasure compatibility |

### Universal Requirements

1. **Same Mental Model**: `Link[Input, Output]` across all languages
2. **Type Evolution**: Clean transformation via `insert_as()` / `InsertAs()` / `insertAs()`
3. **Runtime Behavior**: Identical execution paths for typed and untyped code
4. **Backward Compatibility**: Untyped code continues to work
5. **Gradual Adoption**: Mix typed and untyped components

### Example Pattern (Universal)

```pseudocode
// Input data shape
type UserInput = {
  email: string,
  password: string
}

// Output data shape
type RegisteredUser = {
  email: string,
  userId: number,
  createdAt: timestamp
}

// Typed link
class RegisterUserLink implements Link[UserInput, RegisteredUser]:
  call(ctx: Context[UserInput]) -> Context[RegisteredUser]:
    email = ctx.get("email")
    userId = database.insert(email)
    
    // Type evolution: UserInput -> RegisteredUser
    return ctx.insert_as("userId", userId)
              .insert_as("createdAt", now())
```

### Guidelines for Implementation

See [TYPED_FEATURES_IMPLEMENTATION_PLAN.md](TYPED_FEATURES_IMPLEMENTATION_PLAN.md) for:
- Language-specific implementation patterns
- Test requirements
- Functionality validation
- Migration strategies

---

## Documentation Standards

### Code Documentation

#### Link Documentation Template
```typescript
/**
 * ValidateEmail - Validates email format using regex pattern
 * 
 * Input Context:
 * - email: string - Email address to validate
 * 
 * Output Context:
 * - email: string - Validated email (unchanged)
 * 
 * Errors:
 * - ValueError: If email format is invalid
 * 
 * Example:
 * ```
 * const ctx = new Context({ email: 'test@example.com' });
 * const result = await validateEmail.call(ctx);
 * ```
 */
export class ValidateEmail extends Link<EmailInput, EmailInput> {
  async call(ctx: Context<EmailInput>): Promise<Context<EmailInput>> {
    // Implementation
  }
}
```

#### Chain Documentation Template
```typescript
/**
 * UserRegistration Chain
 * 
 * Processes new user registration from input to database storage.
 * 
 * Chain Steps:
 * 1. ValidateEmailLink - Check email format
 * 2. CheckUserExistsLink - Verify email not already registered
 * 3. HashPasswordLink - Secure password with bcrypt
 * 4. SaveToDatabaseLink - Persist user record
 * 5. SendWelcomeEmailLink - Notify user of successful registration
 * 
 * Error Handling:
 * - ValidationError: Email format or existence issues
 * - DatabaseError: Failure to save user
 * - EmailError: Failure to send welcome email
 * 
 * Example:
 * ```
 * const result = await UserRegistrationChain.execute(
 *   new Context({ email: 'test@example.com', password: 'secure123' })
 * );
 * const userId = result.get('userId');
 * ```
 */
export const UserRegistrationChain = new Chain()
  .add(ValidateEmailLink)
  .add(CheckUserExistsLink)
  .add(HashPasswordLink)
  .add(SaveToDatabaseLink)
  .add(SendWelcomeEmailLink);
```

### README Standards

Each language implementation must have:

1. **Quick Start**: 5-minute getting started example
2. **Installation**: Package manager instructions
3. **Core Concepts**: Link, Context, Chain, Middleware
4. **Examples**: At least 3 working examples
5. **API Reference**: Complete public API documentation
6. **Testing**: How to run tests
7. **Contributing**: Link to this guide

### Website Documentation

Located in `docs/`:
- **index.html**: Main landing page
- **[language]/index.html**: Language-specific documentation
- **components/**: Reusable HTML components
- **assets/**: Images, logos, styles

#### Updating Website

1. Edit HTML components in `docs/components/`
2. Run build script: `node docs/components/build.js`
3. Test locally: Open `docs/index.html` in browser
4. Commit changes

---

## Troubleshooting Common Issues

### 1. MegaLinter Jest Globals Issue

**Problem**: MegaLinter reports `no-undef` errors for Jest globals (`describe`, `test`, `expect`, etc.) in JavaScript tests.

**Cause**: MegaLinter doesn't properly recognize Jest environment configuration.

**Solution**:
```bash
# Use direct ESLint for JavaScript linting
npx eslint packages/javascript/ --config packages/javascript/.eslintrc.json
```

**Status**: Known limitation in MegaLinter, does not affect code quality.

---

### 2. Go Module Not Found

**Problem**: `go: cannot find module providing package`

**Cause**: Go workspace not initialized or module cache needs refresh.

**Solution**:
```bash
# Initialize Go workspace
go work init

# Add all Go modules
go work use ./packages/go

# Sync dependencies
cd packages/go
go mod tidy
go mod download
```

---

### 3. TypeScript Types Not Resolving

**Problem**: Import errors in TypeScript despite correct code.

**Cause**: TypeScript compiler cache or node_modules out of sync.

**Solution**:
```bash
# Clean and reinstall
rm -rf node_modules package-lock.json
npm install

# Rebuild TypeScript declarations
npm run build
```

---

### 4. Act Workflow Fails Locally But Passes on GitHub

**Problem**: `act` workflow fails but GitHub Actions succeeds.

**Cause**: Environment differences between local Docker and GitHub runners.

**Solution**:
```bash
# Use GitHub runner image
act -P ubuntu-latest=ghcr.io/catthehacker/ubuntu:full-latest

# Check available images
docker images | grep act
```

---

### 5. Release Tag Not Triggering Workflow

**Problem**: Tagged release but workflow doesn't run.

**Cause**: Tag format doesn't match workflow trigger pattern.

**Solution**:
```bash
# Check workflow trigger pattern in .github/workflows/
# Example pattern: tags: ['python-v*', 'go-v*']

# Use correct tag format
git tag python-v1.0.0  # ✅ Correct
git tag v1.0.0         # ❌ Won't trigger
git push origin --tags
```

---

### 6. Cross-Language Type Inconsistencies

**Problem**: Typed features behave differently across languages.

**Cause**: Implementation diverged from universal pattern.

**Solution**:
1. Review [TYPED_FEATURES_IMPLEMENTATION_PLAN.md](TYPED_FEATURES_IMPLEMENTATION_PLAN.md)
2. Compare with Python reference implementation
3. Ensure `insert_as()` / `InsertAs()` / `insertAs()` naming consistency
4. Validate test patterns match across languages

---

### 7. Documentation Build Fails

**Problem**: `docs/components/build.js` errors or produces broken HTML.

**Cause**: Missing dependencies or malformed component files.

**Solution**:
```bash
# Reinstall dependencies
cd docs/components
npm install

# Check component syntax
node build.js --dry-run

# Rebuild
node build.js
```

---

### 8. E2E Tests Leaving Orphaned Resources

**Problem**: End-to-end tests fail to clean up resources (repos, webhooks, etc.).

**Cause**: Cleanup logic not in `afterEach` or error before cleanup.

**Solution**:
```typescript
// Always use afterEach for cleanup
afterEach(async () => {
  try {
    // Cleanup logic here
    await deleteTestResources();
  } catch (error) {
    console.warn('Cleanup failed:', error);
    // Don't fail test on cleanup errors
  }
});

// Add timeout for cleanup
jest.setTimeout(30000);
```

---

### 9. Package Not Found After Release

**Problem**: Released package but not available via package manager.

**Cause**: Publishing step failed or propagation delay.

**Solution**:
```bash
# Check release workflow logs
gh run list --workflow=publish_release_assets.yml

# Manually publish if needed
cd packages/[language]

# NPM
npm publish

# PyPI
python -m twine upload dist/*

# Cargo
cargo publish

# NuGet
dotnet nuget push *.nupkg --source https://api.nuget.org/v3/index.json
```

---

## Maintenance Checklist

### Daily
- [ ] Review and respond to issues
- [ ] Review open pull requests
- [ ] Check GitHub Actions for failures

### Weekly
- [ ] Run comprehensive test suites across all languages
- [ ] Update documentation for any merged changes
- [ ] Review and triage new issues
- [ ] Check for dependency updates

### Monthly
- [ ] Review coding standards for necessary updates
- [ ] Analyze test coverage and improve weak areas
- [ ] Update examples with new patterns
- [ ] Review roadmap and update timelines

### Quarterly
- [ ] Comprehensive functionality testing
- [ ] Cross-language consistency audit
- [ ] Documentation comprehensive review
- [ ] Security audit of dependencies
- [ ] Community feedback integration

### Annually
- [ ] Major version planning
- [ ] Standards review and updates
- [ ] Retrospective on development practices
- [ ] Strategic roadmap planning

### Before Each Release
- [ ] All tests pass across all languages
- [ ] Linting passes with no warnings
- [ ] Documentation updated and accurate
- [ ] Examples tested and working
- [ ] Version numbers updated
- [ ] Changelog prepared
- [ ] Local `act` workflow tests pass
- [ ] Cross-language consistency validated
- [ ] Functionality validation completed

---

## Contributing

### For New Contributors

1. Read this guide thoroughly
2. Review [CODING_STANDARDS.md](CODING_STANDARDS.md)
3. Study `docs/pseudo/README.md` for conceptual foundation
4. Pick a language you're comfortable with
5. Start with small improvements or bug fixes
6. Follow the development workflow outlined above

### For Maintainers

1. Ensure PRs follow this guide
2. Validate coding standards compliance
3. Verify test coverage requirements met
4. Check documentation updates
5. Test locally with `act` before merge
6. Update relevant tracking documents

---

## Version History

Version history and changelog are managed through Git tags and GitHub releases. For detailed version history:

```bash
# View all releases
gh release list

# View specific release notes
gh release view [tag-name]

# View commit history
git log --oneline
```

---

## Questions or Issues?

- **Documentation Issues**: Open issue with label `documentation`
- **Bug Reports**: Open issue with label `bug`
- **Feature Requests**: Open issue with label `enhancement`
- **Questions**: Open discussion in GitHub Discussions

---

**This guide is a living document. As CodeUChain evolves, so does this guide. Last updated: 2026-01-18**

*CodeUChain: Where simple code creates extraordinary systems 🌟*

---

# CodeUChain Monorepo Maintenance Guide

> **A living document that defines how we maintain, develop, and evolve the CodeUChain polyglot monorepo with consistency, quality, and excellence.**

## 📖 Table of Contents

- [Overview](#overview)
- [Core Philosophy](#core-philosophy)
- [CodeUChain Fundamentals](#codeuchain-fundamentals)
- [Monorepo Structure](#monorepo-structure)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing Strategy](#testing-strategy)
- [CI/CD & Local Testing](#cicd--local-testing)
- [Release Management](#release-management)
- [Typed Features Guidelines](#typed-features-guidelines)
- [Documentation Standards](#documentation-standards)
- [Troubleshooting Common Issues](#troubleshooting-common-issues)
- [Maintenance Checklist](#maintenance-checklist)

---

## Overview

CodeUChain is a polyglot monorepo providing a universal framework for composable software across 8+ programming languages. This guide ensures that all contributions, maintenance, and evolution follow consistent patterns while respecting language-specific idioms.

### Key Facts
- **Type**: Polyglot Monorepo
- **Languages**: Go, Python, JavaScript/TypeScript, C#, Rust, Java, C++, COBOL (meme), Pseudocode
- **Architecture**: Context-Link-Chain pattern
- **License**: Apache 2.0
- **Status**: Production-ready in core languages

### Document Purpose
This guide serves as the **single source of truth** for:
- How we structure code across languages
- How we test and validate changes
- How we release and maintain packages
- How we enforce quality and consistency

---

## Core Philosophy

### Universal Principles

#### 1. **User-Centric Focus**
> "Delight the user; resolve headaches before they reach them"

We put ourselves in the user's shoes and proactively eliminate pain points through comprehensive testing and thoughtful design.

#### 2. **Consistency Across Languages**
Same mental models, universal patterns, language-specific idioms. A developer familiar with CodeUChain in one language should feel at home in any other.

#### 3. **Test-Driven Development**
We follow a layered testing approach with a **"trust but verify"** mentality:
- **Unit Tests**: Isolate components with mocked dependencies
- **Integration Tests**: Test component interactions with mocked services
- **End-to-End Tests**: Validate with real services, real data, real infrastructure

#### 4. **Local-First Development**
Use `act` for GitHub Actions testing locally before pushing. No more trial-and-error CI/CD cycles.

#### 5. **CodeUChain in All Projects**
We use CodeUChain for all projects. This ensures we experience our own framework and understand developer pain points.

#### 6. **Opt-In Complexity**
Start simple, add features when needed. Typing, advanced orchestration, and tooling are available but never required.

---

## Refresher — CodeUChain Types
Context - The box of data flowing through Links
Link - Individual stations (pure business logic)
Chain - Orchestration of Link sequence
Middleware - Parallel observation layer (logging, metrics, caching, validation)