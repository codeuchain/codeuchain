# CodeUChain - C++ Implementation

[![C++](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![CMake](https://img.shields.io/badge/CMake-3.20+-green)](https://cmake.org/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

> **Universal Language Learning Framework** - Same concepts, C++ syntax. AI agents and developers work seamlessly across C#, JavaScript, Python, Java, Go, Rust, and C++.

## 🌟 Overview

The C++ implementation of CodeUChain brings the universal patterns to modern C++20 development. Leveraging coroutines, smart pointers, and RAII principles, this implementation provides the same core concepts (Chain, Link, Context, Middleware) with C++-appropriate syntax and performance optimizations.

### 🎯 Key Features

- **Modern C++20**: Full coroutine support for async processing
- **Memory Safe**: RAII and smart pointers throughout
- **Performance Optimized**: Zero-cost abstractions and efficient data structures
- **Universal Patterns**: Same concepts as all other language implementations
- **CMake Build System**: Industry-standard build configuration
- **Comprehensive Testing**: Full unit test coverage

## 📁 Project Structure

```
packages/cpp/
├── CMakeLists.txt              # Main build configuration
├── cmake/
│   └── codeuchain-config.cmake.in  # CMake package config
├── include/codeuchain/         # Public headers
│   ├── codeuchain.hpp         # Main include file
│   ├── context.hpp            # Context class
│   ├── link.hpp               # Link interface
│   ├── middleware.hpp         # Middleware interface
│   ├── chain.hpp              # Chain class
│   └── error_handling.hpp     # Error utilities
├── src/                       # Implementation files
│   ├── core/                  # Core implementations
│   └── utils/                 # Utility implementations
├── examples/                  # Example programs
│   ├── CMakeLists.txt
│   └── simple_math.cpp        # Basic arithmetic example
└── tests/                     # Unit tests
    ├── CMakeLists.txt
    └── unit_tests.cpp         # Comprehensive test suite
```

## 🚀 Quick Start

### Prerequisites

- **C++20 Compiler**: GCC 10+, Clang 11+, or MSVC 2019+
- **CMake**: Version 3.20 or higher
- **Git**: For cloning the repository

### Build Instructions

```bash
# Clone the repository
git clone https://github.com/codeuchain/codeuchain.git
cd codeuchain/packages/cpp

# Create build directory
mkdir build && cd build

# Configure with CMake
cmake -DCMAKE_BUILD_TYPE=Release ..

# Build the library
make -j$(nproc)

# Run tests
ctest

# Run example
./examples/simple_math
```

### Using CodeUChain in Your Project

#### CMake Integration

```cmake
# Find CodeUChain
find_package(codeuchain REQUIRED)

# Link to your target
target_link_libraries(your_target PRIVATE codeuchain)
```

#### Manual Integration

```cpp
#include "codeuchain/codeuchain.hpp"

// Your code here
```

## 🎨 Core Components

### Context

The immutable data container that flows through chains:

```cpp
#include "codeuchain/context.hpp"

### Context

The immutable data container that flows through chains:

```cpp
#include "codeuchain/context.hpp"

// Create empty context
codeuchain::Context ctx;

// Insert data (returns new context)
ctx = ctx.insert("key", 42);
ctx = ctx.insert("name", std::string("example"));

// Get data
auto value = ctx.get("key");
if (value) {
    int num = std::get<int>(*value);
}
```

#### Performance Optimization: Mutable Operations

For performance-critical scenarios where you need to make many modifications to the same context within a single link, CodeUChain provides mutable operations:

```cpp
// High-frequency mutations (performance optimization)
codeuchain::Context ctx;
for (int i = 0; i < 1000; ++i) {
    ctx.insert_mut("key" + std::to_string(i), i);  // Modifies in-place
    ctx.update_mut("key500", 9999);               // Modifies in-place
}
```

**⚠️ Important:** Mutable operations break immutability guarantees. Use only when:
- Performance is critical
- You're making many modifications within a single link
- You understand the implications for debugging and testing
- Thread safety is not a concern (single-threaded context)

**✅ Recommended:** Use immutable operations (`insert()`, `update()`, etc.) for most cases to maintain predictability and thread safety.

// Update data (returns new context)
ctx = ctx.update("key", 100);
```

### Link

Individual processing units that transform data:

```cpp
#include "codeuchain/link.hpp"

class MyProcessor : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        // Process the context
        auto input = context.get("input");
        if (input) {
            int value = std::get<int>(*input);
            context = context.insert("output", value * 2);
        }
        co_return {context};
    }

    std::string name() const override { return "my_processor"; }
    std::string description() const override { return "Doubles input values"; }
};
```

### Chain

Orchestrates link execution with middleware support:

```cpp
#include "codeuchain/chain.hpp"

// Create chain
codeuchain::Chain chain;

// Add links
chain.add_link("processor", std::make_shared<MyProcessor>());

// Add middleware
chain.use_middleware(std::make_shared<LoggingMiddleware>());

// Execute
codeuchain::Context initial_ctx;
initial_ctx = initial_ctx.insert("input", 5);

auto future = chain.run(initial_ctx);
auto result = future.get();
```

### Middleware

Cross-cutting concerns that intercept chain execution:

```cpp
#include "codeuchain/middleware.hpp"

class LoggingMiddleware : public codeuchain::IMiddleware {
public:
    std::coroutine_handle<> before(std::shared_ptr<codeuchain::ILink> link,
                                  const codeuchain::Context& context) override {
        if (link) {
            std::cout << "[BEFORE] " << link->name() << std::endl;
        }
        return nullptr;
    }

    std::coroutine_handle<> after(std::shared_ptr<codeuchain::ILink> link,
                                 const codeuchain::Context& context) override {
        if (link) {
            std::cout << "[AFTER] " << link->name() << std::endl;
        }
        return nullptr;
    }

    std::string name() const override { return "logging"; }
    std::string description() const override { return "Logs execution flow"; }
};
```

## 🧪 Testing

Run the comprehensive test suite:

```bash
cd build
ctest --verbose
```

Or run tests individually:

```bash
./tests/unit_tests
```

## 📚 Examples

### Simple Math Chain

The `simple_math.cpp` example demonstrates:

- Creating custom links for arithmetic operations
- Building a chain with multiple processing steps
- Adding middleware for logging
- Executing the chain and retrieving results

```bash
cd build
./examples/simple_math
```

Expected output:
```
CodeUChain C++ - Simple Math Example
====================================
[BEFORE] Chain execution started
[BEFORE] Executing link: add
AddLink: 5 + 3 = 8
[AFTER] Link completed: add
[BEFORE] Executing link: multiply
MultiplyLink: 8 * 2 = 16
[AFTER] Link completed: multiply
[AFTER] Chain execution completed

Final Results:
Addition result: 8
Final result: 16

Same pattern works in ALL languages!
```

## 🔧 Development

### Building with Debug Symbols

```bash
cmake -DCMAKE_BUILD_TYPE=Debug ..
make -j$(nproc)
```

### Code Coverage (GCC/Clang)

```bash
cmake -DCMAKE_BUILD_TYPE=Debug -DCODE_COVERAGE=ON ..
make -j$(nproc)
make coverage
```

### Static Analysis

```bash
# Using clang-tidy
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
clang-tidy src/**/*.cpp -p build
```

## 🤝 Contributing

We welcome contributions! See the main [CodeUChain README](../../README.md) for contribution guidelines.

### C++ Specific Guidelines

- **C++20 Features**: Use modern C++20 features (coroutines, concepts, modules when appropriate)
- **RAII**: Follow RAII principles for resource management
- **Smart Pointers**: Use `std::unique_ptr` and `std::shared_ptr` appropriately
- **Const Correctness**: Maintain const correctness throughout
- **Exception Safety**: Ensure exception safety in all operations
- **Performance**: Optimize for performance while maintaining safety

## 📄 License

This project is licensed under the Apache License 2.0 - see the [LICENSE](../../LICENSE) file for details.

## 🙏 Acknowledgments

- **C++ Standards Committee** for modern C++ features
- **CMake Community** for the excellent build system
- **Open Source Community** for libraries and tools

---

**CodeUChain C++**: Where universal patterns meet modern C++ performance 🌟

*Same concepts, C++ excellence*