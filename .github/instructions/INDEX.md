# GitHub Copilot Instructions Index

This directory contains instructions for GitHub Copilot to assist with development in the CodeUChain repository.

## 📚 Available Instructions

### Repository-Wide Instructions

**[README.md](README.md)** - `applyTo: '**'`
- Primary repository-wide instructions
- Architecture principles and core concepts
- Repository structure and navigation
- Universal development workflow
- Multi-language repository guidelines

**[contributing.instructions.md](contributing.instructions.md)** - `applyTo: '**'`
- Contributing guidelines and workflow
- Pull request process
- Code review best practices
- Git workflow and branch management
- Release process

**[testing.instructions.md](testing.instructions.md)** - `applyTo: '**'`
- Testing patterns across all languages
- CI/CD integration guidelines
- Coverage goals and strategies
- Common test scenarios
- Debugging failed tests

**[typed_features_implementation.instructions.md](typed_features_implementation.instructions.md)** - `applyTo: '**'`
- Comprehensive typed features guidelines
- Generic patterns across languages
- Type evolution implementation
- Language-specific adaptations
- Testing and migration strategies

### Language-Specific Instructions

**[python.instructions.md](python.instructions.md)** - `applyTo: 'packages/python/**'`
- Python development (reference implementation)
- pytest testing patterns
- Type hints and mypy usage
- Python-specific idioms

**[javascript.instructions.md](javascript.instructions.md)** - `applyTo: 'packages/javascript/**'`
- JavaScript/TypeScript development
- Jest testing patterns
- ESLint and formatting
- Both typed and untyped usage

**[go.instructions.md](go.instructions.md)** - `applyTo: 'packages/go/**'`
- Go development (97.5% coverage, production-ready)
- Go 1.18+ generics usage
- Table-driven tests
- Go-specific idioms and patterns

**[rust.instructions.md](rust.instructions.md)** - `applyTo: 'packages/rust/**'`
- Rust development (production-ready)
- Ownership and borrowing
- async_trait patterns
- Cargo testing and benchmarks

**[csharp.instructions.md](csharp.instructions.md)** - `applyTo: 'packages/csharp/**'`
- C# development with .NET
- xUnit testing patterns
- Async/await best practices
- Nullable reference types

## 🎯 How to Use

### For Developers

These instructions are automatically used by GitHub Copilot when:
1. You're working in files that match the `applyTo` pattern
2. You request code suggestions or completions
3. You ask Copilot questions about the codebase

### For Copilot

The `applyTo` pattern determines which instructions are relevant:
- `'**'` - Applies to all files in the repository
- `'packages/python/**'` - Applies only to Python package files
- `'packages/go/**'` - Applies only to Go package files
- And so on for each language

## 📋 Instruction Categories

### Core Development
- Setup and installation
- Project structure
- Development environment
- Build and test commands

### Code Patterns
- Creating Links (simple and generic)
- Creating Chains
- Using Middleware
- Type evolution

### Best Practices
- Code style guidelines
- Error handling
- Performance considerations
- Language-specific idioms

### Testing
- Test structure and organization
- Common test scenarios
- Mocking and stubbing
- Coverage goals

### Debugging
- Debug commands and tools
- Common issues
- Troubleshooting tips

## 🔄 Updating Instructions

When updating these instructions:

1. **Maintain Format**: Keep YAML frontmatter with `applyTo` pattern
2. **Be Specific**: Provide concrete examples and commands
3. **Stay Current**: Update when patterns change
4. **Test Examples**: Ensure code examples work
5. **Link References**: Point to relevant documentation

## 📝 Frontmatter Format

Each instruction file must start with:

```yaml
---
applyTo: 'pattern/**'
---
```

Where `pattern` matches files this instruction applies to.

## 🎨 Content Guidelines

Good instructions should:
- ✅ Be specific and actionable
- ✅ Include working code examples
- ✅ Cover common scenarios
- ✅ Explain the "why" not just the "how"
- ✅ Reference relevant documentation
- ✅ Use clear, concise language

Avoid:
- ❌ Vague or general advice
- ❌ Outdated examples
- ❌ Overly complex explanations
- ❌ Contradicting other instructions
- ❌ Missing file paths or commands

## 🔗 Related Documentation

- **[CODING_STANDARDS.md](../../CODING_STANDARDS.md)** - Language-specific coding standards
- **[TYPED_FEATURES_IMPLEMENTATION_PLAN.md](../../TYPED_FEATURES_IMPLEMENTATION_PLAN.md)** - Implementation roadmap
- **[docs/TYPED_FEATURES_SPECIFICATION.md](../../docs/TYPED_FEATURES_SPECIFICATION.md)** - Technical specification
- **[README.md](../../README.md)** - Repository overview

## 🚀 Quick Reference

| Task | Instruction File |
|------|-----------------|
| General development | README.md |
| Python code | python.instructions.md |
| JavaScript/TypeScript code | javascript.instructions.md |
| Go code | go.instructions.md |
| Rust code | rust.instructions.md |
| C# code | csharp.instructions.md |
| Writing tests | testing.instructions.md |
| Contributing | contributing.instructions.md |
| Typed features | typed_features_implementation.instructions.md |

## 📞 Questions?

If you have questions about these instructions:
1. Read the relevant instruction file thoroughly
2. Check related documentation
3. Look at existing code examples
4. Ask in GitHub Discussions or Issues

---

**Note**: These instructions are designed to help GitHub Copilot provide better assistance. They complement but don't replace the main repository documentation.
