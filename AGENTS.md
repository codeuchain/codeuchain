# CodeUChain Agent Instructions

> **State**: Polyglot Monorepo (Go, Py, TS/JS, C#, Rust, Java, C++, COBOL).
> **Role**: Maintain consistency, quality, and user-centricity across all languages.

## 🧠 Core Mental Model
*   **State**: Immutable data container ("the box"). Thread-safe.
*   **Link**: Single-responsibility processing unit ("the station"). Input State → Output State.
*   **Chain**: Ordered sequence of Links ("the conveyor belt"). Orchestrates flow & errors.
*   **Hook**: Parallel observation layer (Logging, Metrics). *Cannot modify business logic.*

## 📜 Universal Workflow
1.  **Branch**: `feature/your-feature-name`
2.  **TDD Cycle**:
    *   🔴 **RED**: Write failing test first (verifying logic/interface).
    *   🟢 **GREEN**: Implement minimal code to pass.
    *   🔵 **REFACTOR**: Align with standards (Linting/Formatting).
3.  **Verify**:
    *   Run Unit Tests: `npm test`, `pytest`, `go test ./...`
    *   Run Workflows: `act -j [job_name]` (Local GitHub Actions).
4.  **Push**: PR with tests + docs + examples.

## 📏 Coding Standards
*   **Organization**: **1 Link = 1 File**. **1 Chain = 1 File**.
    *   ✅ `src/links/ValidateEmail.ts`
    *   ❌ `src/utils.ts` (containing links)
*   **Naming**: 
    *   Classes: `PascalCase`
    *   Functions: Language idiomatic (camelCase/snake_case).
*   **Link Design**: Single Purpose. No "god links".
*   **Docs**: Mandatory JSDoc/Docstring for public APIs.

## 🧪 Testing Strategy (Trust But Verify)
1.  **Unit**: Mock **everything**. Test logic isolation.
2.  **Integration**: Mock **external services**. Test component flow.
3.  **E2E**: Real services, real data. **MUST clean up resources** in `teardown/afterEach`.

## 💎 Typed Features (Opt-In)
*   **Goal**: Static safety, runtime flexibility.
*   **Pattern**: `Link[Input, Output]`
*   **State**: `State[T]`
    *   `insert(k, v)` -> `State[T]` (Type preserving)
    *   `insert_as(k, v)` -> `State[U]` (Type evolution/transformation)
*   **Rule**: Untyped code must continue to work. Zero runtime cost.

## 📂 Key Paths
*   **Impls**: `packages/[lang]/`
*   **CI**: `.github/workflows/`
*   **Scripts**: `scripts/` (Release automation)
*   **Standards**: `CODING_STANDARDS.md`
*   **Typed Plan**: `TYPED_FEATURES_IMPLEMENTATION_PLAN.md`

## ⚠️ Critical Rules
*   **NEVER** skip local testing (use `act`).
*   **NEVER** mix unrelated concerns in one file.
*   **ALWAYS** follow language idioms (Go conventions in Go, Pythonic in Python).
*   **ALWAYS** verify external side-effects in E2E tests.
