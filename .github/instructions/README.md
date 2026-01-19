---
applyTo: '**'
---

# CodeUChain Repository Instructions for GitHub Copilot

This repository contains the CodeUChain framework - a universal, cross-language pattern for building composable software. This document provides guidance for working with this codebase.

## 🎯 Repository Overview

CodeUChain is a **multi-language monorepo** implementing the same conceptual framework across multiple programming languages:

- **Languages**: Python, JavaScript/TypeScript, Go, Rust, C#, Java, C++, Dart, COBOL, Pseudocode
- **Structure**: `/packages/{language}/` contains each language implementation
- **Philosophy**: Universal concepts with language-idiomatic implementations

## 🏗️ Architecture Principles

### Core Concepts (Apply to All Languages)

1. **Context**: Immutable key-value data structure that flows through processing pipelines
2. **Link**: Individual processing unit with single responsibility (transforms Context)
3. **Chain**: Ordered sequence of Links in a pipeline
4. **Middleware**: Observes and enhances Chain execution (logging, metrics, auth, etc.)

### Implementation Requirements

- **Consistency**: Same conceptual model across all languages
- **Idiomatic Code**: Use each language's natural patterns and conventions
- **Backward Compatibility**: Never break existing untyped code
- **Zero Performance Impact**: Typed features should not affect runtime performance

## 📁 Repository Structure

```
codeuchain/
├── packages/           # Language implementations
│   ├── python/        # Python implementation (reference)
│   ├── javascript/    # JavaScript/TypeScript implementation
│   ├── go/           # Go implementation
│   ├── rust/         # Rust implementation
│   ├── csharp/       # C# implementation
│   ├── java/         # Java implementation
│   ├── cpp/          # C++ implementation
│   ├── dart/         # Dart implementation
│   ├── cobol/        # COBOL implementation
│   └── pseudo/       # Pseudocode examples
├── docs/             # Documentation and website
├── releases/         # Release artifacts
└── scripts/          # Build and utility scripts
```

## 🔧 Development Workflow

### Before Making Changes

1. **Understand the Language**: Each implementation has its own build system and conventions
2. **Check Coding Standards**: Review `/CODING_STANDARDS.md` for language-specific rules
3. **Review Existing Tests**: Understand how testing works in that language
4. **Run Existing Tests**: Ensure everything passes before making changes

### Making Changes

1. **Minimal Changes**: Make the smallest possible changes to achieve the goal
2. **Language Idioms**: Follow the natural patterns of the target language
3. **Test Coverage**: Add tests that match existing test patterns
4. **Documentation**: Update docs if API changes or new features are added
5. **Consistency**: Maintain conceptual consistency across language implementations

### Testing Strategy

Each language has its own testing approach:
- **Python**: pytest in `packages/python/tests/`
- **JavaScript**: Jest/Mocha in `packages/javascript/tests/`
- **Go**: Go test in `packages/go/`
- **Rust**: Cargo test in `packages/rust/`
- **C#**: xUnit/NUnit in `packages/csharp/tests/`
- **Java**: JUnit in `packages/java/src/test/`
- **C++**: Google Test in `packages/cpp/tests/`

### Build Systems

Each language uses its native build system:
- **Python**: pip/poetry
- **JavaScript**: npm/yarn
- **Go**: go build
- **Rust**: cargo
- **C#**: dotnet
- **Java**: Maven
- **C++**: CMake/Conan
- **Dart**: pub

## 🎨 Code Style

Follow these universal principles:

1. **Line Length**: 100 characters maximum
2. **Indentation**: Use language-specific conventions (see CODING_STANDARDS.md)
3. **Naming**: Use language conventions (camelCase vs snake_case)
4. **Documentation**: Document all public APIs
5. **Error Handling**: Use language-appropriate error patterns

## 🧪 Testing Guidelines

### Test Coverage Requirements

- ✅ Core functionality (Context, Link, Chain)
- ✅ Type evolution (if applicable)
- ✅ Generic interfaces (if applicable)
- ✅ Error handling
- ✅ Middleware integration
- ✅ Backward compatibility

### Test Patterns

Use the Python implementation as reference for test structure:
- Test files should mirror source structure
- Use descriptive test names
- Test both success and failure cases
- Test edge cases and boundary conditions

## 📚 Documentation

### When to Update Documentation

- Adding new features or APIs
- Changing existing behavior
- Adding new language implementations
- Updating typed features

### Documentation Locations

- **README.md**: High-level overview and quick start
- **docs/**: Language-specific documentation
- **packages/{language}/README.md**: Language implementation details
- **Code Comments**: Complex logic and business rules

## 🔐 Security

- Never commit secrets or credentials
- Follow secure coding practices for each language
- Validate inputs appropriately
- Use parameterized queries where applicable
- Keep dependencies updated

## 🚀 Performance

- Maintain zero-cost abstractions where possible
- Avoid unnecessary allocations
- Use appropriate data structures
- Profile before optimizing
- Document performance characteristics

## 🎯 Typed Features

CodeUChain implements **opt-in generics** across all languages. See `.github/instructions/typed_features_implementation.instructions.md` for comprehensive guidelines.

### Key Principles

1. **Opt-in**: Typing is optional, never required
2. **Zero Cost**: No runtime performance impact
3. **Type Evolution**: Clean transformation between types
4. **Gradual Adoption**: Add typing incrementally

### Generic Patterns

```python
# Python Reference Pattern
class Link[Input, Output]:
    async def call(self, ctx: Context[Input]) -> Context[Output]:
        pass

class Context[T]:
    def insert(self, key: str, value: Any) -> Context[T]
    def insert_as(self, key: str, value: Any) -> Context[Any]
```

Adapt this pattern to your target language while maintaining the same mental model.

## 🐛 Debugging

### Common Issues

1. **Build Failures**: Check language-specific build requirements
2. **Test Failures**: Ensure you're using the correct test runner
3. **Linting Errors**: Run language-specific linter (see CODING_STANDARDS.md)
4. **Type Errors**: Check typed features documentation

### Debug Tools

- Use language-specific debuggers
- Add logging with appropriate middleware
- Use chain visualization tools (if available)
- Check Context state at each Link

## 📦 Releases

- Releases are managed per language in `/releases/`
- Follow semantic versioning
- Update CHANGELOG for each release
- Test releases in isolation

## 🤝 Contributing

When contributing to CodeUChain:

1. **Start Small**: Begin with small, focused changes
2. **Follow Patterns**: Study existing code in that language
3. **Test Thoroughly**: Add comprehensive tests
4. **Document**: Update relevant documentation
5. **Ask Questions**: Reach out if unclear about patterns

## 🔍 Finding Your Way

### Working with Python
- Reference implementation
- Start here to understand core concepts
- Located in `packages/python/`
- Uses pytest for testing

### Working with JavaScript/TypeScript
- TypeScript provides strong typing
- Located in `packages/javascript/`
- Uses Jest for testing
- Supports both typed and untyped usage

### Working with Go
- Production-ready with 97.5% test coverage
- Located in `packages/go/`
- Uses Go 1.18+ generics
- Native Go testing

### Working with Rust
- Production-ready implementation
- Located in `packages/rust/`
- Uses async traits and Serde
- Cargo for building/testing

### Working with C#
- Strong static typing support
- Located in `packages/csharp/`
- Uses xUnit for testing
- .NET standard conventions

### Working with Other Languages
- Java, C++, Dart, COBOL implementations available
- Each follows language-specific conventions
- Check package README for details

## ⚠️ Important Notes

### DO
- ✅ Make minimal, surgical changes
- ✅ Follow language-specific conventions
- ✅ Test your changes thoroughly
- ✅ Update documentation when needed
- ✅ Maintain backward compatibility
- ✅ Use existing libraries and patterns

### DON'T
- ❌ Break existing untyped code
- ❌ Add unnecessary dependencies
- ❌ Ignore linting errors
- ❌ Skip testing
- ❌ Change working code unnecessarily
- ❌ Mix concerns across languages

## 📞 Getting Help

- **Typed Features**: See `.github/instructions/typed_features_implementation.instructions.md`
- **Coding Standards**: See `CODING_STANDARDS.md`
- **Implementation Plan**: See `TYPED_FEATURES_IMPLEMENTATION_PLAN.md`
- **Language Docs**: Check `packages/{language}/README.md`

---

**Remember**: CodeUChain's goal is universal understanding through consistent concepts, not identical syntax. Each language should feel natural while maintaining the same mental model.
