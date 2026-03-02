# CodeUChain C++ Typed Features Implementation

This implementation provides opt-in generics for CodeUChain's C++ version, extending the existing `State` class with type-safe operations while maintaining runtime flexibility.

## Overview

The typed features follow the universal CodeUChain guidelines:
- **Opt-in generics**: Type safety when you want it, runtime flexibility when you don't
- **Same mental model**: `Link<Input, Output>` pattern across all implementations
- **Type evolution**: Clean transformation between related types without casting
- **Zero performance impact**: Typing should not affect runtime performance

## Key Components

### 1. TypedState<T>
Generic state that maintains type information at compile time:

```cpp
// Create typed state
auto ctx = make_typed_state<std::string>({});

// Type-safe operations
auto ctx2 = ctx.insert("name", std::string("Alice"));
auto ctx3 = ctx2.insert("age", 30);

// Type-safe retrieval
auto name = ctx3.get_typed<std::string>("name");  // std::optional<std::string>
auto age = ctx3.get_typed<int>("age");            // std::optional<int>
```

### 2. Type Evolution
Clean transformation between types using `insert_as()`:

```cpp
// Type evolution
auto ctx4 = ctx3.insert_as<double>("score", 95.5);  // Changes state type to double
```

### 3. Link<Input, Output>
Generic link interface for type-safe data transformation:

```cpp
class UppercaseLink : public Link<std::string, std::string> {
public:
    std::string call(const std::string& input) override {
        // Transform input to uppercase
        std::string result = input;
        for (char& c : result) {
            c = std::toupper(c);
        }
        return result;
    }
};
```

## Usage Examples

### Basic Typed Operations
```cpp
#include "typed_state.hpp"

using namespace codeuchain;

// Create and use typed state
auto ctx = make_typed_state<std::string>({});
auto ctx2 = ctx.insert("name", std::string("Alice"));
auto ctx3 = ctx2.insert("age", 30);

// Type-safe retrieval
auto name = ctx3.get_typed<std::string>("name");
if (name) {
    std::cout << "Name: " << *name << std::endl;
}
```

### Type Evolution
```cpp
// Start with string state
auto ctx = make_typed_state<std::string>({});
auto ctx2 = ctx.insert("data", std::string("hello"));

// Evolve to different type
auto ctx3 = ctx2.insert_as<int>("count", 42);
auto ctx4 = ctx3.insert_as<double>("score", 95.5);
```

### Runtime Flexibility
```cpp
// Access underlying state for runtime operations
auto base_ctx = ctx.to_state();
auto runtime_value = base_ctx.get("any_key");
```

## Type Safety Features

- **Compile-time type checking**: Catch type errors at compile time
- **Optional types**: Use `std::optional<T>` for safe retrieval
- **Type evolution**: Clean transitions between state types
- **Runtime fallback**: Access underlying `State` for dynamic operations

## Building and Running

### Prerequisites
- C++17 or later
- CMake 3.10 or later

### Build Examples
```bash
# Build the typed state example
g++ -std=c++17 -Iinclude examples/typed_state_example.cpp src/typed_state.cpp src/state.cpp -o typed_example

# Build the typed link example
g++ -std=c++17 -Iinclude examples/typed_link_example.cpp src/typed_state.cpp src/state.cpp -o link_example
```

### Run Examples
```bash
./typed_example
./link_example
```

## Integration with Existing Code

The typed features extend rather than replace the existing `State` class:

```cpp
// Existing code continues to work
State ctx;
ctx = ctx.insert("key", DataValue("value"));

// New typed features
TypedState<std::string> typed_ctx(ctx);
auto typed_result = typed_ctx.insert("typed_key", std::string("typed_value"));
```

## Architecture Notes

### Design Principles
1. **Opt-in**: Typing features are optional, never required
2. **Zero Cost**: No runtime performance impact when typing is disabled
3. **Same Storage**: Uses equivalent runtime representations
4. **Type Evolution**: Clean transformation without explicit casting

### Type System
- Uses C++ templates for compile-time type safety
- Maintains runtime flexibility through base `State` compatibility
- Provides type-safe wrappers around runtime data

### Memory Management
- Uses `std::shared_ptr` for reference counting
- Immutable by default (following CodeUChain principles)
- Optional mutable operations for performance-critical code

## Future Enhancements

- [ ] Additional type specializations
- [ ] Chain integration with typed states
- [ ] Hook support for typed operations
- [ ] Performance optimizations
- [ ] Extended type evolution patterns

## Related Documentation

- [Universal Foundation](../MODULINK_UNIVERSAL_FOUNDATION.md)
- [Type Progress Instructions](../../packages/cpp/include/codeuchain/type-progress.instructions.md)
- [State API](state.hpp)