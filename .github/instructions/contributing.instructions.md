---
applyTo: '**'
---

# Contributing and Repository Practices

## 🤝 Contributing to CodeUChain

This document provides guidelines for contributing to the CodeUChain repository.

## Getting Started

### Before Contributing

1. **Understand the Framework**: Read the main README and understand core concepts
2. **Choose Your Language**: Pick the language implementation you want to work on
3. **Check Existing Issues**: Look for open issues or discussions
4. **Ask Questions**: Don't hesitate to ask for clarification

### Setting Up Your Environment

1. **Fork the Repository**: Create your own fork on GitHub
2. **Clone Locally**: `git clone https://github.com/YOUR_USERNAME/codeuchain.git`
3. **Create a Branch**: `git checkout -b feature/your-feature-name`
4. **Install Dependencies**: Follow language-specific setup instructions

## Contribution Guidelines

### Types of Contributions

✅ **We Welcome:**
- Bug fixes
- New features (after discussion)
- Performance improvements
- Documentation improvements
- Test coverage improvements
- Examples and tutorials
- Translations

### Code Contributions

#### 1. Make Minimal Changes
- Change only what's necessary to fix the issue or add the feature
- Don't refactor unrelated code
- Keep changes focused and atomic

#### 2. Follow Language Conventions
- Use language-specific coding standards (see CODING_STANDARDS.md)
- Match existing code style in that language
- Follow idiomatic patterns for the language

#### 3. Write Tests
- Add tests for new features
- Ensure existing tests pass
- Match existing test patterns
- Aim for good coverage (80%+ for new code)

#### 4. Document Your Changes
- Update relevant documentation
- Add XML/JSDoc/docstring comments for public APIs
- Include examples for new features
- Update README if needed

#### 5. Commit Messages
```
feat(python): add retry middleware

Add configurable retry middleware with exponential backoff.
Includes tests and documentation.

Fixes #123
```

**Format:**
- `type(scope): subject`
- Types: feat, fix, docs, style, refactor, test, chore
- Scope: language or component (python, go, docs, etc.)
- Subject: brief description

### Pull Request Process

#### Before Submitting

```bash
# 1. Run tests
cd packages/{language}
# Run language-specific test command

# 2. Run linter
# Run language-specific lint command

# 3. Update documentation
# Edit relevant docs files

# 4. Commit changes
git add .
git commit -m "feat(language): your change"

# 5. Push to your fork
git push origin feature/your-feature-name
```

#### Creating the PR

1. **Title**: Clear, descriptive title
   - Good: "Add retry middleware to Python implementation"
   - Bad: "Updates" or "Fix stuff"

2. **Description**: Use the PR template
   - What changes were made
   - Why the changes are needed
   - How to test the changes
   - Related issues

3. **Link Issues**: Reference related issues with `Fixes #123`

4. **Request Review**: Tag relevant maintainers

#### After Submitting

- **Respond to Feedback**: Address review comments promptly
- **Keep Updated**: Rebase on main if needed
- **Be Patient**: Maintainers review in their spare time
- **Be Open**: Be receptive to suggestions

## Code Review Guidelines

### For Contributors

When receiving feedback:
- **Be Respectful**: Assume good intentions
- **Ask Questions**: If feedback is unclear, ask
- **Explain Decisions**: Share your reasoning
- **Be Flexible**: Be open to alternative approaches

### For Reviewers

When reviewing code:
- **Be Kind**: Provide constructive feedback
- **Be Specific**: Point to exact lines/issues
- **Explain Why**: Help contributors learn
- **Approve Quickly**: Don't block unnecessarily

## Common Contribution Scenarios

### Adding a New Link Type

1. Create link class in appropriate directory
2. Implement Link interface for that language
3. Add comprehensive tests
4. Add example usage
5. Update documentation

```python
# Example structure
# 1. Implementation
class MyNewLink(Link):
    async def call(self, ctx: Context) -> Context:
        # Implementation
        pass

# 2. Tests
def test_my_new_link():
    # Test cases
    pass

# 3. Example
# examples/my_new_link_example.py

# 4. Documentation
# Update README or add docs
```

### Fixing a Bug

1. **Reproduce**: Write a failing test first
2. **Fix**: Make minimal change to fix
3. **Verify**: Ensure test now passes
4. **Check**: Ensure no regressions

```python
# 1. Failing test
def test_bug_reproduction():
    # Test that demonstrates the bug
    assert False, "Bug exists"

# 2. Fix the bug
# Make minimal code change

# 3. Test passes
def test_bug_reproduction():
    # Test now passes
    assert True
```

### Adding Documentation

1. **Identify Gap**: What's missing or unclear?
2. **Write Content**: Clear, concise documentation
3. **Add Examples**: Include code examples
4. **Test Examples**: Ensure examples work

### Improving Performance

1. **Measure First**: Profile to find bottlenecks
2. **Benchmark**: Write benchmarks before optimizing
3. **Optimize**: Make targeted improvements
4. **Verify**: Ensure benchmarks show improvement
5. **Document**: Explain what and why

## Multi-Language Considerations

### Maintaining Consistency

When implementing a feature across languages:

1. **Start with Python**: Use Python as reference
2. **Maintain Concepts**: Same mental model across languages
3. **Use Language Idioms**: Adapt to language patterns
4. **Test Equivalently**: Similar test coverage

### Cross-Language Changes

If your change affects multiple languages:

1. **Plan Carefully**: Understand impact across languages
2. **Coordinate**: Consider implementing in phases
3. **Document**: Explain language-specific differences
4. **Test Thoroughly**: Ensure all implementations work

## Git Workflow

### Branch Naming

```
feature/add-retry-middleware
fix/context-memory-leak
docs/improve-quickstart
refactor/simplify-chain-api
test/increase-coverage
```

### Keeping Your Branch Updated

```bash
# Fetch latest changes
git fetch upstream main

# Rebase your branch
git rebase upstream/main

# Force push to your fork (if needed)
git push --force-with-lease origin feature/your-feature
```

### Resolving Conflicts

```bash
# During rebase, if conflicts occur
# 1. Fix conflicts in files
# 2. Stage resolved files
git add .

# 3. Continue rebase
git rebase --continue

# If things go wrong
git rebase --abort
```

## CI/CD Integration

### Automated Checks

Pull requests automatically run:
- Linting checks
- Unit tests
- Integration tests (where applicable)
- Code coverage analysis

### Required Checks

Before merging, PRs must:
- ✅ Pass all tests
- ✅ Pass linting
- ✅ Have review approval
- ✅ Be up to date with main

### Handling CI Failures

1. **Check Logs**: Review failure details
2. **Reproduce Locally**: Run same commands locally
3. **Fix Issues**: Address the failures
4. **Push Fix**: Update your branch
5. **Wait for CI**: CI will run again automatically

## Release Process

### Version Numbers

CodeUChain uses semantic versioning:
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Checklist

1. Update version numbers
2. Update CHANGELOG
3. Run full test suite
4. Create release branch
5. Tag release
6. Build packages
7. Publish to package registries
8. Create GitHub release

## Getting Help

### Where to Ask

- **GitHub Issues**: Bug reports and feature requests
- **Discussions**: Questions and general discussion
- **Documentation**: Check docs first
- **Code**: Read existing implementations

### What to Include

When asking for help:
- **Context**: What are you trying to do?
- **Problem**: What's not working?
- **Attempts**: What have you tried?
- **Environment**: OS, language version, etc.
- **Code**: Minimal reproducible example

## Recognition

### Contributors

All contributors are recognized:
- Added to CONTRIBUTORS file
- Mentioned in release notes
- Credited in relevant documentation

### Maintainers

Consistent, high-quality contributors may be invited to become maintainers.

## Code of Conduct

### Our Standards

- **Be Respectful**: Treat everyone with respect
- **Be Inclusive**: Welcome diverse perspectives
- **Be Collaborative**: Work together constructively
- **Be Professional**: Keep discussions focused and civil

### Unacceptable Behavior

- Harassment or discrimination
- Trolling or insulting comments
- Personal or political attacks
- Publishing private information

### Enforcement

Violations may result in:
1. Warning
2. Temporary ban
3. Permanent ban

Report issues to maintainers privately.

## License

By contributing, you agree that your contributions will be licensed under the Apache 2.0 License.

## Questions?

- Read the documentation
- Check existing issues
- Ask in Discussions
- Contact maintainers

Thank you for contributing to CodeUChain! 🎉
