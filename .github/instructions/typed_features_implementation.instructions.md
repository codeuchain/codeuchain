---
applyTo: '**'
---
title: CodeUChain Typed Features Implementation Guidelines
description: Comprehensive guidelines for implementing opt-in generics and type evolution across all CodeUChain language implementations
version: 1.0
created: 2025-09-04
updated: 2025-09-04
---

# CodeUChain: Typed Features Implementation Guidelines

## 🎯 Core Philosophy

CodeUChain implements **opt-in generics** that provide static type safety while maintaining runtime flexibility. This document provides authoritative guidelines for implementing these features across all language implementations.

## 📋 Universal Requirements

### Core Concepts (Must Maintain)
1. **Same Mental Model**: `Link[Input, Output]` pattern across all languages
2. **Type Evolution**: Clean transformation between related types without casting
3. **Runtime Flexibility**: `Dict[str, Any]` behavior when typing is disabled
4. **Opt-in Philosophy**: Typing features are optional, never required
5. **Zero Performance Impact**: Typing should not affect runtime performance

### Implementation Principles
- **Language Idioms**: Use each language's natural patterns and conventions
- **Backward Compatibility**: Existing untyped code continues to work unchanged
- **Gradual Adoption**: Teams can adopt typing incrementally
- **Mixed Usage**: Typed and untyped components can coexist seamlessly

## 🎨 Generic Interface Patterns

### Link Interface (Universal)
```python
# Python Reference
class Link[Input, Output]:
    async def call(self, ctx: Context[Input]) -> Context[Output]:
        pass
```

**Universal Requirements:**
- Generic type parameters for Input/Output types
- Async execution pattern (or language equivalent)
- Context transformation capability
- Error handling support
- Optional: Middleware compatibility

### Context Interface (Universal)
```python
# Python Reference
class Context[T]:
    def insert(self, key: str, value: Any) -> Context[T]:  # Preserve type
        pass

    def insert_as(self, key: str, value: Any) -> Context[Any]:  # Type evolution
        pass
```

**Universal Requirements:**
- Generic type parameter for current data shape
- Immutable transformation methods
- Runtime Dict[str, Any] equivalent storage
- Type-safe access methods
- Optional: Mutable context for performance-critical sections

## 🔧 Language-Specific Implementation Guidelines

### C# Implementation
**Strengths**: Strong static typing, covariance, LINQ integration
**Key Patterns:**
```csharp
public interface ILink<TInput, TOutput>
{
    Task<Context<TOutput>> CallAsync(Context<TInput> context);
}

public class Context<out T> : IContext  // Covariant for flexibility
{
    public Context Insert(string key, object value) => this;
    public Context<U> InsertAs<U>(string key, object value) => new Context<U>(...);
}
```
**Guidelines:**
- Use `out T` for covariance where appropriate
- Leverage nullable reference types (`T?`)
- Maintain compatibility with existing `ILink` interface
- Use `dynamic` for runtime flexibility when needed

### JavaScript/TypeScript Implementation
**Strengths**: Structural typing, gradual adoption, runtime flexibility
**Key Patterns:**
```typescript
interface Link<TInput = any, TOutput = any> {
  call(ctx: Context<TInput>): Promise<Context<TOutput>>;
}

class Context<T = any> {
  insert(key: string, value: any): Context<T>;
  insertAs<U>(key: string, value: any): Context<U>;
}
```
**Guidelines:**
- Use structural typing over nominal typing
- Support both TypeScript and vanilla JavaScript usage
- Leverage `unknown` and conditional types appropriately
- Maintain runtime flexibility with `any` defaults

### Java Implementation
**Strengths**: Enterprise-grade type safety, annotations, tooling
**Key Patterns:**
```java
public interface Link<TInput, TOutput> {
    CompletableFuture<Context<TOutput>> call(Context<TInput> context);
}

public class Context<T> {
    public Context<T> insert(String key, Object value);
    public <U> Context<U> insertAs(String key, Object value);
}
```
**Guidelines:**
- Use wildcards (`? extends T`, `? super T`) appropriately
- Add `@Nullable` and other annotations
- Maintain type erasure compatibility
- Support both typed and raw usage patterns

### Go Implementation
**Strengths**: Interface-based typing, simplicity, performance
**Key Patterns:**
```go
type Link[TInput any, TOutput any] interface {
    Call(ctx Context[TInput]) (Context[TOutput], error)
}

type Context[T any] struct {
    Insert(key string, value any) Context[T]
    InsertAs[U any](key string, value any) Context[U]
}
```
**Guidelines:**
- Use Go 1.18+ generics consistently
- Maintain interface compatibility
- Leverage `any` for flexibility
- Follow Go naming conventions

### Rust Implementation
**Strengths**: Memory safety, ownership system, performance
**Key Patterns:**
```rust
#[async_trait]
pub trait Link<Input, Output>: Send + Sync {
    async fn call(&self, ctx: Context<Input>) -> Result<Context<Output>, Error>;
}

pub struct Context<T = serde_json::Value> {
    pub fn insert(self, key: String, value: serde_json::Value) -> Self;
    pub fn insert_as<U>(self, key: String, value: serde_json::Value) -> Context<U>;
}
```
**Guidelines:**
- Respect ownership and borrowing rules
- Use Serde for runtime flexibility
- Implement proper error handling
- Follow Rust async trait patterns

## 🧪 Testing Strategy

### Universal Test Patterns

#### Type Evolution Test
```python
# Python Reference - Adapt to target language
def test_type_evolution():
    input_ctx = Context[InputData]({"numbers": [1, 2, 3]})
    output_ctx = input_ctx.insert_as("result", 6.0)

    assert output_ctx.get("result") == 6.0
    assert output_ctx.get("numbers") == [1, 2, 3]
```

#### Generic Link Test
```python
# Python Reference - Adapt to target language
def test_generic_link():
    link = SumLink()
    input_ctx = Context[InputData]({"numbers": [1, 2, 3]})

    result_ctx = await link.call(input_ctx)

    assert result_ctx.get("result") == 6.0
    assert result_ctx.get("numbers") == [1, 2, 3]
```

#### Runtime Compatibility Test
```python
# Ensure untyped usage still works identically
def test_runtime_compatibility():
    untyped_ctx = Context({"numbers": [1, 2, 3]})
    result = untyped_ctx.insert("result", 6.0)

    assert result.get("result") == 6.0
    assert result.get("numbers") == [1, 2, 3]
```

### Test Coverage Requirements
- ✅ Basic type evolution functionality
- ✅ Generic link interfaces
- ✅ Chain composition with generics
- ✅ Runtime compatibility (untyped usage)
- ✅ Error handling in typed contexts
- ✅ Mixed typed/untyped component usage

## 📊 Performance Requirements

### Runtime Performance
- **Zero Cost**: Typing should not impact runtime performance
- **Same Storage**: Use equivalent of `Dict[str, Any]` for all implementations
- **Same Execution**: Identical execution paths for typed vs untyped code

### Compile-Time Performance
- **Incremental**: Type checking should be fast and incremental
- **Optional**: No compilation penalty when typing is disabled
- **Helpful**: Clear, actionable error messages

## 🔄 Migration Strategy

### Backward Compatibility
- **Existing Code**: All existing untyped code continues to work
- **Gradual Adoption**: Teams can adopt typing incrementally
- **Mixed Usage**: Typed and untyped components can coexist

### Implementation Phases
1. **Phase 1**: Add generic interfaces alongside existing ones
2. **Phase 2**: Implement type evolution methods
3. **Phase 3**: Add comprehensive examples and tests
4. **Phase 4**: Update documentation and tooling

## 🎯 Success Criteria

### Functional Completeness
- ✅ Generic `Link[Input, Output]` interfaces implemented
- ✅ Generic `Context[T]` with type evolution implemented
- ✅ TypedDict/struct equivalents for data shapes
- ✅ Clean `insert_as()` method implemented
- ✅ Comprehensive test coverage achieved

### Developer Experience
- ✅ Clear, actionable error messages
- ✅ Helpful IDE integration
- ✅ Comprehensive documentation
- ✅ Working examples for all patterns

### Runtime Compatibility
- ✅ Zero performance impact verified
- ✅ Identical runtime behavior confirmed
- ✅ Full backward compatibility maintained
- ✅ Mixed typed/untyped usage supported

## 📚 Reference Materials

### Primary References
- **Python Implementation**: `packages/python/codeuchain/core/`
- **Test Suite**: `packages/python/tests/test_typed.py`
- **Examples**: `packages/python/examples/`
- **Specification**: `docs/TYPED_FEATURES_SPECIFICATION.md`

### Implementation Plan
- **Detailed Plan**: `TYPED_FEATURES_IMPLEMENTATION_PLAN.md`
- **Language Priorities**: C# → JavaScript → Java → Go → Rust
- **Timeline**: Q1-Q2 2025 rollout

## 🤝 Implementation Guidelines

### When Implementing in a New Language:
1. **Study Python Reference**: Understand the patterns and philosophy
2. **Adapt to Language Idioms**: Use language-specific best practices
3. **Maintain Universal Patterns**: Same mental model across languages
4. **Comprehensive Testing**: Both typed and untyped test coverage
5. **Documentation**: Clear examples and migration guides

### Code Review Checklist:
- [ ] Generic interfaces follow universal patterns
- [ ] Type evolution works without explicit casting
- [ ] Runtime compatibility maintained
- [ ] Comprehensive test coverage
- [ ] Documentation updated
- [ ] Performance requirements met

### Common Pitfalls to Avoid:
- ❌ Breaking existing untyped code
- ❌ Adding performance overhead
- ❌ Complex type system that confuses developers
- ❌ Inconsistent naming conventions
- ❌ Missing error handling in typed contexts

## 🚀 Best Practices

### Type System Design
- **Simple Over Complex**: Prefer simple, understandable type patterns
- **Flexible Defaults**: Use `any` equivalents for maximum flexibility
- **Clear Error Messages**: Provide helpful type error feedback
- **Gradual Adoption**: Make it easy to start with typing

### Runtime Compatibility
- **Same Storage**: Use equivalent runtime representations
- **Same Behavior**: Typed and untyped should behave identically
- **Zero Cost**: No performance penalty for typing
- **Mixed Usage**: Allow typed and untyped components together

### Developer Experience
- **Clear Examples**: Provide comprehensive working examples
- **Migration Path**: Easy transition from untyped to typed
- **Helpful Errors**: Actionable error messages and suggestions
- **IDE Support**: Full IntelliSense and refactoring support

## 📞 Getting Help

When implementing typed features:

1. **Reference Python**: Use Python implementation as the authoritative reference
2. **Check Specification**: Consult `docs/TYPED_FEATURES_SPECIFICATION.md`
3. **Review Tests**: Study `packages/python/tests/test_typed.py` patterns
4. **Ask Questions**: Reach out to maintainers for clarification

**Remember**: The goal is universal understanding through consistent concepts, not identical syntax. Each language should feel natural while maintaining the same mental model.

---

*These guidelines ensure CodeUChain's typed features provide production-grade type safety while maintaining the framework's core philosophy of universal accessibility and runtime flexibility.*
--></content>
<parameter name="filePath">/Users/jwink/Documents/github/codeuchain/.github/instructions/typed_features_implementation.instructions.md