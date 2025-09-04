# CodeUChain Coding Standards

## Overview
This document establishes consistent coding standards across all languages in the CodeUChain monorepo. These standards ensure fair and consistent linting rules while respecting each language's idioms and best practices.

## Core Principles
- **Consistency**: Same concepts should be expressed similarly across languages
- **Readability**: Code should be easily readable by developers familiar with any language
- **Maintainability**: Standards should support long-term code maintenance
- **Performance**: Standards should not negatively impact performance
- **Language Idioms**: Respect each language's established conventions

## Global Standards

### 1. Line Length
- **Maximum**: 100 characters
- **Rationale**: Balances readability with modern wide-screen displays
- **Exception**: URLs, import statements, and long strings may exceed limit

### 2. Indentation
- **Style**: Spaces only (no tabs)
- **Width**: 4 spaces (2 for JavaScript/TypeScript, 4 for others)
- **Rationale**: Consistent visual hierarchy across languages

### 3. Naming Conventions
- **Functions/Methods**: `camelCase` (JavaScript, Java, C#) or `snake_case` (Python, Rust, Go)
- **Classes/Types**: `PascalCase` across all languages
- **Constants**: `UPPER_SNAKE_CASE` across all languages
- **Variables**: Language-specific conventions
- **Files**: `snake_case` or `kebab-case` depending on language conventions

### 4. Code Structure
- **Imports**: Grouped by type, sorted alphabetically
- **Functions**: Maximum 50 lines (exceptions for complex algorithms)
- **Classes**: Single responsibility principle
- **Files**: Related functionality grouped together

### 5. Documentation
- **Public APIs**: Full documentation with examples
- **Complex Logic**: Inline comments explaining business logic
- **File Headers**: Apache 2.0 license header

### 6. Error Handling
- **Explicit**: Prefer explicit error handling over silent failures
- **Meaningful**: Error messages should be descriptive
- **Recovery**: Where possible, provide recovery mechanisms

## Language-Specific Standards

### JavaScript/TypeScript
- **Style**: Airbnb JavaScript Style Guide (adapted)
- **Promises**: Async/await preferred over raw promises
- **Types**: Strict TypeScript usage
- **Modules**: ES6 modules preferred

### Python
- **Style**: PEP 8 with some adaptations
- **Type Hints**: Required for public APIs
- **Docstrings**: Google-style docstrings
- **Imports**: Absolute imports preferred

### Rust
- **Style**: Standard Rust formatting (`rustfmt`)
- **Error Handling**: `Result<T, E>` and `Option<T>` patterns
- **Ownership**: Explicit ownership management
- **Documentation**: Rustdoc comments for public APIs

### Java
- **Style**: Google Java Style Guide
- **Exception Handling**: Checked exceptions for recoverable errors
- **Null Safety**: Avoid null where possible
- **Documentation**: Javadoc for public APIs

### C#
- **Style**: Microsoft C# Coding Conventions
- **Exception Handling**: Specific exception types
- **Async**: Async/await patterns
- **Documentation**: XML documentation comments

### C++
- **Style**: Google C++ Style Guide
- **Memory**: RAII patterns, smart pointers
- **Exception Handling**: Exceptions for exceptional cases
- **Documentation**: Doxygen comments

### Go
- **Style**: Standard Go formatting (`gofmt`)
- **Error Handling**: Multiple return values pattern
- **Concurrency**: Goroutines and channels
- **Documentation**: Go doc comments

## Linting Rules Matrix

| Rule Category | JavaScript | Python | Rust | Java | C# | C++ | Go |
|---------------|------------|--------|------|------|----|-----|----|
| Line Length | 100 | 100 | 100 | 100 | 100 | 100 | 100 |
| Indentation | 2 spaces | 4 spaces | 4 spaces | 4 spaces | 4 spaces | 2 spaces | tabs |
| Naming | camelCase | snake_case | snake_case | camelCase | PascalCase | snake_case | camelCase |
| Documentation | JSDoc | docstrings | rustdoc | Javadoc | XML docs | Doxygen | godoc |
| Error Handling | try/catch | exceptions | Result/Option | checked | specific | exceptions | multiple return |

## Implementation Notes

### MegaLinter Configuration
- Use consistent rule sets across languages where possible
- Configure language-specific rules to match these standards
- Enable parallel processing for performance
- Set appropriate severity levels

### CI/CD Integration
- Lint checks must pass before merge
- Automated formatting on commit (where possible)
- Consistent reporting across all languages
- Performance monitoring of linting process

### Tool Selection
- Prefer fast, reliable tools
- Use language-native tools where available
- Ensure tools support the established standards
- Regular updates to tool versions

## Known Limitations

### MegaLinter Jest Configuration Issue
- **Issue**: MegaLinter does not properly recognize Jest globals (describe, test, expect, beforeEach, etc.) even when configured correctly
- **Impact**: JavaScript test files show false "no-undef" errors for Jest globals
- **Workaround**: Use direct ESLint execution for JavaScript linting: `npx eslint packages/javascript/ --config packages/javascript/.eslintrc.json`
- **Status**: Known limitation in MegaLinter - does not affect actual code quality
- **Resolution**: Consider using direct ESLint for JavaScript in CI/CD pipeline

## Maintenance
- Review standards annually
- Update based on language evolution
- Incorporate team feedback
- Document exceptions and rationale

## Exceptions
- Performance-critical code may have relaxed standards
- Generated code may not follow all standards
- Legacy code migration follows separate timeline
- Experimental features may have different standards