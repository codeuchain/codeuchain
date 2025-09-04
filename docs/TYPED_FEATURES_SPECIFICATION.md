# CodeUChain: Advanced Typing Features

## 🎯 Overview

CodeUChain introduces **revolutionary opt-in generics** that provide static type safety while maintaining runtime flexibility. This document serves as the authoritative specification for implementing these features across all language implementations.

## 📋 Core Philosophy

### Dual Approach Design
CodeUChain supports two complementary approaches:

1. **Untyped (Default)**: Maximum runtime flexibility with `Dict[str, Any]` behavior
2. **Typed (Opt-in)**: Static type checking with compile-time guarantees

### Key Principles
- **Opt-in**: Typing features are optional, never required
- **Runtime Compatible**: Same `Dict[str, Any]` behavior regardless of typing
- **Universal Patterns**: Same mental model across all languages
- **Clean Evolution**: Type-safe transformation without explicit casting

## 🎨 Universal Patterns

### Generic Link Interface

```python
# Python Reference
class Link[Input, Output]:
    async def call(self, ctx: Context[Input]) -> Context[Output]:
        # Process context and return evolved type
        pass
```

**Universal Requirements:**
- Generic type parameters for Input/Output types
- Async execution pattern (or language equivalent)
- Context transformation capability
- Error handling support
- Optional: Middleware compatibility

### Generic Context Interface

```python
# Python Reference
class Context[T]:
    # Current: Preserve type
    def insert(self, key: str, value: Any) -> Context[T]:
        pass

    # New: Type evolution
    def insert_as(self, key: str, value: Any) -> Context[Any]:
        pass
```

**Universal Requirements:**
- Generic type parameter for current data shape
- Immutable transformation methods
- Runtime Dict[str, Any] equivalent storage
- Type-safe access methods
- Optional: Mutable context for performance-critical sections

### Type Evolution Pattern

```python
# Python Reference
class InputData(TypedDict):
    numbers: List[int]

class OutputData(InputData):
    result: float

class Processor(Link[InputData, OutputData]):
    async def call(self, ctx: Context[InputData]) -> Context[OutputData]:
        numbers = ctx.get("numbers") or []
        result = sum(numbers)
        # Clean evolution - no casting required!
        return ctx.insert_as("result", float(result))
```

**Universal Requirements:**
- TypedDict or equivalent for data shapes
- Inheritance for type evolution
- Clean transformation methods
- Compile-time type checking

## 🔧 Implementation Specifications

### Language-Specific Adaptations

#### C# Implementation
```csharp
// Generic interfaces
public interface ILink<TInput, TOutput>
{
    Task<Context<TOutput>> CallAsync(Context<TInput> context);
}

// Covariant context
public class Context<out T> : IContext  // Covariant for flexibility
{
    public Context Insert(string key, object value) => this;
    public Context<U> InsertAs<U>(string key, object value) => new Context<U>(...);
}

// TypedDict equivalent
public record InputData
{
    public required List<int> Numbers { get; init; }
    public required string Operation { get; init; }
}

public record OutputData : InputData
{
    public required float Result { get; init; }
}
```

#### JavaScript/TypeScript Implementation
```typescript
// Generic interfaces
interface Link<TInput = any, TOutput = any> {
  call(ctx: Context<TInput>): Promise<Context<TOutput>>;
}

// Structural typing
class Context<T = any> {
  insert(key: string, value: any): Context<T>;
  insertAs<U>(key: string, value: any): Context<U>;
}

// TypedDict equivalent
interface InputData {
  numbers: number[];
  operation: string;
}

interface OutputData extends InputData {
  result: number;
}
```

#### Java Implementation
```java
// Generic interfaces
public interface Link<TInput, TOutput> {
    CompletableFuture<Context<TOutput>> call(Context<TInput> context);
}

// Wildcard generics
public class Context<T> {
    public Context<T> insert(String key, Object value);
    public <U> Context<U> insertAs(String key, Object value);
}

// Record types (Java 14+)
public record InputData(List<Integer> numbers, String operation) {}
public record OutputData(List<Integer> numbers, String operation, Double result) {}
```

#### Go Implementation
```go
// Generic interfaces (Go 1.18+)
type Link[TInput any, TOutput any] interface {
    Call(ctx Context[TInput]) (Context[TOutput], error)
}

// Type evolution
type Context[T any] struct {
    Insert(key string, value any) Context[T]
    InsertAs[U any](key string, value any) Context[U]
}

// Struct types
type InputData struct {
    Numbers   []int  `json:"numbers"`
    Operation string `json:"operation"`
}

type OutputData struct {
    Numbers   []int   `json:"numbers"`
    Operation string  `json:"operation"`
    Result    float64 `json:"result"`
}
```

#### Rust Implementation
```rust
// Generic traits
#[async_trait]
pub trait Link<Input, Output>: Send + Sync {
    async fn call(&self, ctx: Context<Input>) -> Result<Context<Output>, Error>;
}

// Type evolution with ownership
pub struct Context<T = serde_json::Value> {
    data: HashMap<String, serde_json::Value>,
}

impl<T> Context<T> {
    pub fn insert(self, key: String, value: serde_json::Value) -> Self {
        // Implementation
    }

    pub fn insert_as<U>(self, key: String, value: serde_json::Value) -> Context<U> {
        // Implementation
    }
}

// Struct types with Serde
#[derive(Serialize, Deserialize)]
pub struct InputData {
    pub numbers: Vec<i32>,
    pub operation: String,
}

#[derive(Serialize, Deserialize)]
pub struct OutputData {
    pub numbers: Vec<i32>,
    pub operation: String,
    pub result: f64,
}
```

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

## 📊 Performance Considerations

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

## 🎯 Success Metrics

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

## 📚 Reference Implementation

The Python implementation serves as the reference for all other languages:

- **Source**: `packages/python/codeuchain/core/`
- **Tests**: `packages/python/tests/test_typed.py`
- **Examples**: `packages/python/examples/`
- **Documentation**: `packages/python/README.md`

## 🤝 Contributing

When implementing typed features in a new language:

1. **Study Python Reference**: Understand the patterns and philosophy
2. **Adapt to Language Idioms**: Use language-specific best practices
3. **Maintain Universal Patterns**: Same mental model across languages
4. **Comprehensive Testing**: Both typed and untyped test coverage
5. **Documentation**: Clear examples and migration guides

**Remember**: The goal is universal understanding through consistent concepts, not identical syntax.</content>
<parameter name="filePath">/Users/jwink/Documents/github/codeuchain/docs/TYPED_FEATURES_SPECIFICATION.md