# CodeUChain - C++ Implementation

[![C++](https://img.shields.io/badge/C%2B%2B-20-blue)](https://en.cppreference.com/)
[![CMake](https://img.shields.io/badge/CMake-3.20+-green)](https://cmake.org/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

> **Universal Language Learning Framework** - Same concepts, C++ syntax. AI agents and developers work seamlessly across C#, JavaScript, Python, Java, Go, Rust, and C++.

## 🌟 Overview

The C++ implementation of CodeUChain brings the universal patterns to modern C++20 development. Leveraging coroutines, smart pointers, and RAII principles, this implementation provides the same core concepts (Chain, Link, State, Hook) with C++-appropriate syntax and performance optimizations.

### 🎯 Key Features

- **Modern C++20**: Full coroutine support for async processing
- **Memory Safe**: RAII and smart pointers throughout
- **Performance Optimized**: Zero-cost abstractions and efficient data structures
- **Universal Patterns**: Same concepts as all other language implementations
- **Typed Features**: Opt-in generics for compile-time type safety
- **Branching Support**: Advanced conditional branching with return-to-main functionality
- **Timing Hook**: Built-in performance profiling for optimization
- **CMake Build System**: Industry-standard build configuration
- **Comprehensive Testing**: Full unit test coverage

## 🔷 Typed Features (NEW!)

CodeUChain C++ now includes opt-in generic features that provide compile-time type safety while maintaining runtime flexibility. These features follow the universal CodeUChain type evolution guidelines.

### Why Typed Features?

- **Compile-time Safety**: Catch type errors at compile time
- **Zero Runtime Cost**: Typing doesn't affect performance
- **Opt-in Design**: Use when you want it, runtime flexibility when you need it
- **Clean Evolution**: `insert_as()` method for type transformations
- **Universal Consistency**: Same mental model across all implementations

### Quick Typed Example

```cpp
#include "codeuchain/typed_state.hpp"

// Type-safe operations
auto ctx = codeuchain::make_typed_state<std::string>({});
auto ctx2 = ctx.insert("name", std::string("Alice"));
auto ctx3 = ctx2.insert("age", 30);

// Type-safe retrieval
auto name = ctx3.get_typed<std::string>("name");  // Compile-time checked
auto age = ctx3.get_typed<int>("age");            // Compile-time checked

// Type evolution
auto ctx4 = ctx3.insert_as<double>("score", 95.5);  // Clean type change

// Runtime flexibility
auto base_ctx = ctx4.to_state();
```

## ⚡ TL;DR (Performance & When to Optimize)

Most users should start with the standard dynamic `Chain` abstraction for clarity, observability, and flexibility.

Optimize ONLY if profiling shows a micro-scale hot path where per-link work is trivial (nanoseconds to low microseconds) and chain overhead dominates.

Decision artifacts:
- Optimization Decision Guide (practical ladder): `../cpp_opt/OPTIMIZATION_DECISION_GUIDE.md`
- Deep empirical analysis (overhead sources + roadmap): `CHAIN_PERFORMANCE_OPTIMIZATION.md`
- Hot key slot empirical addendum (value caching impact): Section 14 of `CHAIN_PERFORMANCE_OPTIMIZATION.md`

Quick ladder:
1. Dynamic Chain (default)
2. StaticChain (remove virtual dispatch) 
3. StaticChain + mut ops (remove immutable copy churn)
4. Slot caching / value hoisting (remove repeated lookup/variant cost)
5. HybridState + interning (planned) (remove alloc + hash overhead)
6. Direct fused function (only if extreme constraints)

Heuristic: If chain structural overhead < 15% of total useful link work, leave it alone.

### ⏱ Reusable Per-Link Timing (TimingHook)

For quick, ad-hoc measurement of real chain behavior (including your own links' logic), enable the built-in `TimingHook`.

Why it exists:
* Complements synthetic microbenchmarks by measuring your actual link mix
* Zero changes to link code – pure hook drop-in
* Human-readable units + raw nanoseconds (same formatter as benchmark harness)

Usage:
```cpp
#include "codeuchain/chain.hpp"
#include "codeuchain/timing_hook.hpp"

codeuchain::Chain chain;
// add links ...
auto timing = std::make_shared<codeuchain::TimingHook>(/*per_invocation=*/true);
chain.use_hook(timing);

auto fut = chain.run(codeuchain::State{});
auto out = fut.get();
timing->report(std::cout); // prints per-link totals + averages + chain total
```

CLI (benchmark harness):
```bash
./examples/benchmark_chain --mode async --timing-mw --iters 5000
```

Design notes:
* `per_invocation=true` stores each call to compute an average; set `false` to aggregate only (lower memory).
* Uses steady_clock wall time – sufficient for relative comparisons; for instruction-level analysis still use external profilers.
* Report distinguishes total chain wall time vs sum of links (hook cost / scheduler gaps become visible if they diverge).

When to use:
* Validating that a suspected hot link actually dominates chain time
* Comparing impact of refactoring a single link
* Establishing baseline before adopting advanced optimizations (StaticChain, slot caching, etc.)

When not to use:
* Ultra high-frequency microbench (prefer dedicated harness where timer noise can be amplified via batching)
* Multi-thread contention analysis (extend hook or integrate with external tracing)

Future extensions (roadmap alignment): statistical summarization (median/p95), optional JSON export, integration with forthcoming instrumentation counters (lookup counts, variant constructions) for a unified performance report.


## 📁 Project Structure

```
packages/cpp/
│   ├── codeuchain.hpp         # Main include file
│   ├── state.hpp            # State class
│   ├── link.hpp               # Link interface
│   ├── hook.hpp         # Hook interface
│   ├── chain.hpp              # Chain class with branching support
│   ├── error_handling.hpp     # Error utilities
│   ├── typed_state.hpp      # Typed features (NEW!)
│   ├── timing_hook.hpp  # Performance profiling hook
│   └── TYPED_FEATURES_README.md # Typed features documentation
├── src/                       # Implementation files
│   ├── core/                  # Core implementations
│   │   ├── chain.cpp          # Chain with advanced branching
│   │   ├── state.cpp        # State implementation
│   │   ├── link.cpp           # Link interface
│   │   └── hook.cpp     # Hook system
│   ├── utils/                 # Utility implementations
│   └── typed_state.cpp      # Typed features implementation
├── examples/                  # Example programs
│   ├── CMakeLists.txt
│   ├── simple_math.cpp        # Basic arithmetic example
│   ├── typed_state_example.cpp    # Typed state demo (NEW!)
│   ├── typed_link_example.cpp       # Typed link demo (NEW!)
│   ├── business_workflow.cpp        # Real-world workflow with timing
│   └── benchmark_chain.cpp          # Performance benchmarking
├── tests/                     # Unit tests
│   ├── CMakeLists.txt
│   ├── unit_tests.cpp         # Comprehensive test suite
│   └── test_typed_state.cpp # Typed features tests (NEW!)
└── build/                     # Build artifacts (generated)
```

## � Installation via Conan (Recommended)

CodeUChain is available via [Conan Center](https://conan.io/center/) for easy integration into your C++ projects.

### Quick Install

```bash
# Install CodeUChain
conan install codeuchain/1.0.0@

# For development with examples/tests
conan install codeuchain/1.0.0@ -o build_examples=True -o build_tests=True
```

### CMake Integration

```cmake
# In your CMakeLists.txt
find_package(codeuchain REQUIRED)
target_link_libraries(your_target codeuchain::codeuchain)
```

### Conanfile.txt Example

```ini
[requires]
codeuchain/1.0.0

[generators]
CMakeDeps
CMakeToolchain

[layout]
cmake_layout
```

### Building with Conan

```bash
# Configure
cmake --preset conan-release

# Build
cmake --build build/Release
```

## 📦 Direct Download (Alternative)

If you prefer a minimal, release-only archive (no monorepo contents), we publish a clean package under `releases/codeuchain-cpp-v1.0.0` in this repository.

Quick download and extract:

```bash
curl -L https://github.com/codeuchain/codeuchain/raw/main/releases/codeuchain-cpp-v1.0.0.tar.gz | tar xz
cd codeuchain-cpp-v1.0.0
./build.sh
./examples/simple_math
```

The release archive contains only the package sources, examples, `conanfile.py`, and build helpers (no other repo files).
## �🚀 Quick Start

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

### State

The immutable data container that flows through chains:

```cpp
#include "codeuchain/state.hpp"

## 🎨 Core Components

### State

The immutable data container that flows through chains:

```cpp
#include "codeuchain/state.hpp"

// Create empty state
codeuchain::State ctx;

// Insert data (returns new state)
ctx = ctx.insert("key", 42);
ctx = ctx.insert("name", std::string("example"));

// Get data
auto value = ctx.get("key");
if (value) {
    int num = std::get<int>(*value);
}
```

#### Performance Optimization: Mutable Operations

For performance-critical scenarios where you need to make many modifications to the same state within a single link, CodeUChain provides mutable operations:

```cpp
// High-frequency mutations (performance optimization)
codeuchain::State ctx;
for (int i = 0; i < 1000; ++i) {
    ctx.insert_mut("key" + std::to_string(i), i);  // Modifies in-place
    ctx.update_mut("key500", 9999);               // Modifies in-place
}
```

**⚠️ Important:** Mutable operations break immutability guarantees. Use only when:
- Performance is critical
- You're making many modifications within a single link
- You understand the implications for debugging and testing
- Thread safety is not a concern (single-threaded state)

**✅ Recommended:** Use immutable operations (`insert()`, `update()`, etc.) for most cases to maintain predictability and thread safety.

### Typed State (NEW!)

Opt-in generics for compile-time type safety while maintaining runtime flexibility:

```cpp
#include "codeuchain/typed_state.hpp"

// Type-safe state operations
auto ctx = codeuchain::make_typed_state<std::string>({});
auto ctx2 = ctx.insert("name", std::string("Alice"));
auto ctx3 = ctx2.insert("age", 30);

// Type-safe retrieval
auto name = ctx3.get_typed<std::string>("name");  // std::optional<std::string>
auto age = ctx3.get_typed<int>("age");            // std::optional<int>

// Type evolution without casting
auto ctx4 = ctx3.insert_as<double>("score", 95.5);

// Runtime flexibility when needed
auto base_ctx = ctx4.to_state();
auto runtime_value = base_ctx.get("any_key");
```

**Key Benefits:**
- **Compile-time type safety** when you want it
- **Runtime flexibility** when you need it
- **Zero performance impact** - typing doesn't affect runtime
- **Clean type evolution** with `insert_as()`
- **Full backward compatibility** with existing State

## 🌿 Advanced Branching (NEW!)

CodeUChain C++ now supports sophisticated conditional branching with return-to-main functionality, perfect for complex workflows like API request processing with database queries.

### Branch Types

- **Conditional Branch**: Execute alternative path based on conditions
- **Branch with Return**: Execute branch then return to main execution path
- **Branch Termination**: Execute branch and stop (no return)

### Quick Branching Example

```cpp
#include "codeuchain/chain.hpp"

// Create main processing chain
codeuchain::Chain chain;
chain.add_link("validate_request", std::make_shared<ValidateLink>());
chain.add_link("process_response", std::make_shared<ResponseLink>());
chain.add_link("send_response", std::make_shared<SendLink>());

// Add database query branch
chain.add_link("query_database", std::make_shared<DatabaseLink>());
chain.add_link("store_results", std::make_shared<StoreLink>());

// Branch from validation to database if needed, then return to response processing
auto needs_db = [](const codeuchain::State& ctx) -> bool {
    auto needs_query = ctx.get("needs_database");
    return needs_query && std::holds_alternative<bool>(*needs_query) && 
           std::get<bool>(*needs_query);
};
chain.connect_branch("validate_request", "query_database", "process_response", needs_db);

// Execute
codeuchain::State ctx;
ctx = ctx.insert("needs_database", true);
auto result = chain.run(ctx).get();

// Execution path: validate_request → query_database → store_results → process_response → send_response
```

### Branch Scenarios

| Scenario | Method | Description |
|----------|--------|-------------|
| **API with DB Query** | `connect_branch(source, branch, return_target, condition)` | Validate request → Query DB → Return to process response |
| **Error Handling** | `connect_branch(source, error_handler, "", condition)` | On error, handle and terminate |
| **Conditional Processing** | `connect(source, target, condition)` | Simple conditional jump (existing) |

### Performance Benefits

- **Zero Overhead**: Branch conditions evaluated only when reached
- **Memory Efficient**: No additional allocations for branching logic
- **Coroutine Optimized**: Branches work seamlessly with async execution

### Link

Individual processing units that transform data:

```cpp
#include "codeuchain/link.hpp"

class MyProcessor : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::State state) override {
        // Process the state
        auto input = state.get("input");
        if (input) {
            int value = std::get<int>(*input);
            state = state.insert("output", value * 2);
        }
        co_return {state};
    }

    std::string name() const override { return "my_processor"; }
    std::string description() const override { return "Doubles input values"; }
};
```

### Typed Link (NEW!)

Generic link interface for type-safe data transformation:

```cpp
#include "codeuchain/typed_state.hpp"

// Type-safe link
class UppercaseLink : public codeuchain::Link<std::string, std::string> {
public:
    std::string call(const std::string& input) override {
        std::string result = input;
        for (char& c : result) {
            c = std::toupper(c);
        }
        return result;
    }
};

// Usage
auto link = std::make_unique<UppercaseLink>();
std::string result = link->call("hello world");  // "HELLO WORLD"
```

### Chain

Orchestrates link execution with hook support:

```cpp
#include "codeuchain/chain.hpp"

// Create chain
codeuchain::Chain chain;

// Add links
chain.add_link("processor", std::make_shared<MyProcessor>());

// Add hook
chain.use_hook(std::make_shared<LoggingHook>());

// Execute
codeuchain::State initial_ctx;
initial_ctx = initial_ctx.insert("input", 5);

auto future = chain.run(initial_ctx);
auto result = future.get();
```

### Hook

Cross-cutting concerns that intercept chain execution:

```cpp
#include "codeuchain/hook.hpp"

class LoggingHook : public codeuchain::IHook {
public:
    std::coroutine_handle<> before(std::shared_ptr<codeuchain::ILink> link,
                                  const codeuchain::State& state) override {
        if (link) {
            std::cout << "[BEFORE] " << link->name() << std::endl;
        }
        return nullptr;
    }

    std::coroutine_handle<> after(std::shared_ptr<codeuchain::ILink> link,
                                 const codeuchain::State& state) override {
        if (link) {
            std::cout << "[AFTER] " << link->name() << std::endl;
        }
        return nullptr;
    }

    std::string name() const override { return "logging"; }
    std::string description() const override { return "Logs execution flow"; }
};
```
```

### Link

Individual processing units that transform data:

```cpp
#include "codeuchain/link.hpp"

class MyProcessor : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::State state) override {
        // Process the state
        auto input = state.get("input");
        if (input) {
            int value = std::get<int>(*input);
            state = state.insert("output", value * 2);
        }
        co_return {state};
    }

    std::string name() const override { return "my_processor"; }
    std::string description() const override { return "Doubles input values"; }
};
```

### Chain

Orchestrates link execution with hook support:

```cpp
#include "codeuchain/chain.hpp"

// Create chain
codeuchain::Chain chain;

// Add links
chain.add_link("processor", std::make_shared<MyProcessor>());

// Add hook
chain.use_hook(std::make_shared<LoggingHook>());

// Execute
codeuchain::State initial_ctx;
initial_ctx = initial_ctx.insert("input", 5);

auto future = chain.run(initial_ctx);
auto result = future.get();
```

### Hook

Cross-cutting concerns that intercept chain execution:

```cpp
#include "codeuchain/hook.hpp"

class LoggingHook : public codeuchain::IHook {
public:
    std::coroutine_handle<> before(std::shared_ptr<codeuchain::ILink> link,
                                  const codeuchain::State& state) override {
        if (link) {
            std::cout << "[BEFORE] " << link->name() << std::endl;
        }
        return nullptr;
    }

    std::coroutine_handle<> after(std::shared_ptr<codeuchain::ILink> link,
                                 const codeuchain::State& state) override {
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
./tests/unit_tests              # Core functionality tests
./tests/test_typed_state      # Typed features tests (NEW!)
```

### Test Coverage

- **Core Tests**: State, Link, Chain, and Hook functionality
- **Typed Tests**: Type safety, evolution, and compatibility
- **Integration Tests**: Full chain execution with hook
- **Performance Tests**: Benchmarking for optimization validation

## 📚 Examples

### Simple Math Chain

The `simple_math.cpp` example demonstrates:

- Creating custom links for arithmetic operations
- Building a chain with multiple processing steps
- Adding hook for logging
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

### Typed State Example (NEW!)

The `typed_state_example.cpp` demonstrates the new typed features:

- Type-safe state operations with compile-time guarantees
- Type evolution using `insert_as()` method
- Runtime flexibility when needed
- Type safety validation

```bash
cd build
./examples/typed_state_example
```

Expected output:
```
CodeUChain Typed State Example
=================================

1. Creating typed state...
2. Type-safe insert operations...
3. Type-safe retrieval...
Name: Alice
Age: 30
Active: Yes
4. Type evolution with insert_as()...
Score: 95.5
5. Runtime flexibility...
Runtime name: Alice
6. Type safety demonstration...
Type safety: Cannot get string as double (expected)

Example completed successfully!
```

### Typed Link Example (NEW!)

The `typed_link_example.cpp` demonstrates generic link interfaces:

- Type-safe link implementations with `Link<Input, Output>`
- Compile-time type checking for data transformation
- Runtime compatibility with existing chains
- Error handling for type mismatches

```bash
cd build
./examples/typed_link_example
```

Expected output:
```
CodeUChain Typed Link Example
============================

1. Typed Link calls:
Input: hello world
After uppercase: HELLO WORLD
Final result: HELLO WORLD (length: 11)

2. Runtime Link calls:
Runtime result: TEST STRING (length: 11)

3. Type safety:
Type safety: Wrong input type handled gracefully

Link example completed successfully!
```

### Business Workflow Example (NEW!)

The `business_workflow.cpp` demonstrates a realistic multi-stage order processing pipeline with TimingHook:

- Simulated order validation, customer enrichment, pricing, discounts, persistence, and event publishing
- Each link performs meaningful work and mutates state
- TimingHook measures per-link performance
- Shows how to profile real-world chains

```bash
cd build
```bash
cd build
./examples/business_workflow --runs 3 --per-invocation --format csv --unit ms --decimals 3
```

Expected output (CSV format):
```
Runs: 3 per-invocation: on
Final order summary:
  total: 24.275
  order_id: 1002
  loyalty_tier: gold
Link,Total,Avg/Call,Calls
ValidateInput,0.011 ms (11250.00 ns),0.004 ms (3750.00 ns),3
ApplyDiscounts,0.016 ms (15791.00 ns),0.005 ms (5263.67 ns),3
EnrichCustomer,0.013 ms (12583.00 ns),0.004 ms (4194.33 ns),3
PriceCalculation,0.021 ms (20625.00 ns),0.007 ms (6875.00 ns),3
PersistOrder,0.041 ms (40958.00 ns),0.014 ms (13652.67 ns),3
PublishEvent,0.022 ms (22126.00 ns),0.007 ms (7375.33 ns),3
[Chain Total],0.076 ms (76000.00 ns),,
```

### Formatting Options

The TimingHook supports extensive customization of output format:

| Option | Values | Description |
|--------|--------|-------------|
| `--format` | `tabular`, `csv` | Output format (default: tabular) |
| `--unit` | `auto`, `ns`, `us`, `ms` | Time unit (default: auto) |
| `--decimals` | `N` | Decimal places (default: 2) |
| `--no-raw-ns` | | Hide raw nanoseconds in parentheses |
| `--no-calls` | | Hide call count column |
| `--no-avg` | | Hide average per call column |
| `--no-total` | | Hide total time column |

Examples:
```bash
# CSV format with milliseconds, 3 decimals
./examples/business_workflow --format csv --unit ms --decimals 3

# Nanoseconds only, no decimals, hide raw ns
./examples/business_workflow --unit ns --decimals 0 --no-raw-ns

# Microseconds, hide call counts
./examples/business_workflow --unit us --no-calls
```
```

Expected output:
```
Runs: 3 per-invocation: on

Final order summary:
  total: 24.275
  order_id: 1002
  loyalty_tier: gold

== TimingHook Report ==
Link                    Total             Avg/Call      Calls
----------------------------------------------------------------------
ValidateInput           3.62 ms (3620000.00 ns) 1.21 ms (1206667.00 ns) 3
EnrichCustomer          6.01 ms (6010000.00 ns) 2.00 ms (2003333.00 ns) 3
PriceCalculation        7.52 ms (7520000.00 ns) 2.51 ms (2506667.00 ns) 3
ApplyDiscounts          5.41 ms (5410000.00 ns) 1.80 ms (1803333.00 ns) 3
PersistOrder            9.63 ms (9630000.00 ns) 3.21 ms (3210000.00 ns) 3
PublishEvent            6.32 ms (6320000.00 ns) 2.11 ms (2106667.00 ns) 3
----------------------------------------------------------------------
[Chain Total]           2.45 µs (2450.00 ns)
```

## � Benchmarking (NEW!)

The C++ implementation includes a dedicated micro-benchmark harness to empirically quantify the computational cost of CodeUChain primitives versus direct/manual equivalents.

### Covered Benchmarks

| Category | Framework Operation | Control Baseline | Notes |
|----------|---------------------|------------------|-------|
| Immutable State | `State.insert()` | Manual fresh `std::unordered_map` copy + insert | Measures persistent-style insert cost |
| Mutable State | `State.insert_mut()` | Direct `unordered_map` mutation | Shows optimization path |
| Typed Features | `TypedState.insert() / get_typed()` | Untyped `State.insert()/get()` | Overhead of type-safety wrapper |
| Type Evolution | `insert_as()` | (No direct control) | Absolute per-op cost only |
| Chain Dispatch | 3-link sync or async chain | Direct nested functions (`double -> add_ten -> square`) | Virtual + coroutine + state overhead |
| Scaling | Chain lengths 1,2,4,8 | (Absolute) | Per-link growth characteristics |

### Building & Running

```bash
cd packages/cpp
mkdir -p build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . --target benchmark_chain -j$(nproc)

# Run with defaults (sync mode)
./examples/benchmark_chain

# Increase iterations & repeats, both sync and async
./examples/benchmark_chain --iters 100000 --repeat 7 --mode both

# Amplify extremely small operations with batching
./examples/benchmark_chain --iters 40000 --batch 4 --repeat 5

# Disable scaling section for faster runs
./examples/benchmark_chain --no-scale
```

### CLI Options

| Flag | Default | Description |
|------|---------|-------------|
| `--iters N` | 20000 | Loop iterations per benchmark group |
| `--repeat R` | 5 | Median-of-R timing stabilization |
| `--mode sync|async|both` | sync | Include sync, async, or both chain modes |
| `--batch B` | 1 | Perform B operations per loop body to amplify timing |
| `--no-scale` | (off) | Skip chain length scaling section |
| `--help` | | Show usage |

### Allocation Tracking (Optional)

The harness can globally count allocations to help identify unexpected heap churn:

```bash
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-DCODEUCHAIN_BENCH_TRACK_ALLOC" ..
cmake --build . --target benchmark_chain -j$(nproc)
./examples/benchmark_chain --iters 60000 --repeat 7
```

Output will include allocation call counts and total allocated bytes. (This is a coarse tool: it overrides global `new/delete`.)

### Interpreting Results

1. Always use `Release` builds (`-O2`/`-O3`). Debug builds exaggerate framework overhead.
2. When baseline per-op time falls below ~1ns the benchmark suppresses the relative overhead percentage (sub-nanosecond noise floor). Increase `--iters` and/or `--batch` for higher signal.
3. Sync chain dispatch shows deterministic per-link scaling; async mode includes `std::future` + coroutine state overhead and is expected to be higher.
4. Typed feature overhead should remain modest (generally low double-digit ns or a small percentage over untyped ops, depending on compiler and CPU).
5. Use median-of-repeats to reduce tail effects from frequency scaling, state switches, and interrupt jitter.

### Example (Truncated) Output

```
CodeUChain Benchmark
 iterations        : 20000
 median repeats    : 5
 mode              : both
 batch factor      : 1 (each loop performs this many ops)
 scaling section   : on

== State Insert (Immutable) ==
State.insert() vs manual copy        total(ms): 8.627   per-op(ns): 431.34  overhead(%): 436.29
== State Mutable Insert ==
State.insert_mut()                   total(ms): 3.473   per-op(ns): 173.67  overhead(%): 79.04
== Typed vs Untyped State ==
TypedState insert/get                total(ms): 7.225   per-op(ns): 361.23  overhead(%): 21.85
== Type Evolution (insert_as) ==
TypedState insert_as()               total(ms): 13.024  per-op(ns): 651.23  overhead(%): 0.00
== Chain vs Direct Function Pipeline ==
Chain sync (3 links)                   total(ms): 22.719  per-op(ns): 1135.96 overhead(%): 15.42
Chain async (3 links)                  total(ms): 158.197 per-op(ns): 15819.7 overhead(%): 1294.3
...
```

### Common Questions

**Q: Why is immutable insert so much slower than direct mutation?**  
Because each immutable insert simulates a persistent structure by creating a new map. Real workloads typically amortize this by batching or using mutable paths inside a single link when safe.

**Q: Why suppress overhead when baseline < 1ns?**  
At that scale results are dominated by timing noise and loop/carried dependencies. Percentages become misleading.

**Q: Async chain seems much slower—does that matter?**  
Async cost reflects coroutine frame + future orchestration. Use async only when you need concurrency or natural suspension points; sync mode keeps overhead minimal.

**Q: How do I compare across machines?**  
Fix `--iters`, `--repeat`, and record CPU model, compiler, and flags. Compare percentage deltas, not absolute nanoseconds.

---

For deeper performance investigations consider: perf (Linux), Instruments (macOS), VTune (Intel), or `-finstrument-functions` sampling. The benchmark harness is a starting point, not a full profiler.

### Benchmarking Addendum: Linear Nested Evaluation (NEW)

The benchmark harness now also reports a Linear Nested Evaluation baseline using a compile-time recursive template (`nested_eval<N>`). This path:

* Applies the exact same logical sequence (double → add_ten → square) as the 3-link chain
* Uses only fully inlinable static calls (no virtual dispatch)
* Avoids state construction/copy and heap allocation
* Often optimizes below the timer’s resolution (<1ns); overhead % is therefore suppressed

Interpretation guidelines:
1. Treat nested eval as a theoretical lower bound (floor) on transformation cost.
2. Compare Chain vs Direct function pipeline to assess real abstraction overhead.
3. Use `--batch B` to amplify operations if you need visibility into sub-nanosecond regions.
4. For future deeper analysis, a planned enhancement (`--nested-mode noinline`) can create a measurable upper bound for raw call stacking.

Table row legend (if present in your build output):
| Row | Meaning |
|-----|---------|
| Nested eval (3 levels) | Pure template recursion baseline |
| Chain sync vs nested (Δ%) | Relative difference between structured chain and theoretical floor |
| Nested eval length N | Scaling of recursive depth (1,2,4,8) |

This addition strengthens comparative analysis by separating unavoidable structural costs (state, dispatch, coroutine/future) from the irreducible compute floor.


## �🔧 Development

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

## 📋 Changelog

### v1.0.0 (Latest)
- ✅ **Advanced Branching**: `connect_branch()` with return-to-main functionality
- ✅ **Performance Profiling**: Built-in TimingHook for C++ developers
- ✅ **Typed Features**: Opt-in generics with compile-time type safety
- ✅ **Business Workflow Example**: Real-world order processing with timing
- ✅ **Comprehensive Testing**: 100% test coverage including branching scenarios
- ✅ **Production Ready**: Memory-safe, coroutine-optimized, CMake-based build

### Key Features for C++ Developers
- **Zero-Cost Timing**: Profile your chains without code changes
- **Branching Support**: Handle complex workflows like API + database processing
- **Type Safety**: Optional compile-time guarantees with runtime flexibility
- **Performance Optimized**: Smart pointers, RAII, and efficient data structures
- **Modern C++20**: Full coroutine support with async execution

---