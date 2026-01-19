# CodeUChain C++ - Installation Guide

This guide covers all the ways to install and use CodeUChain C++ in your projects.

## Table of Contents

- [Installation Options](#installation-options)
- [Option 1: Conan Package Manager (Recommended)](#option-1-conan-package-manager-recommended)
- [Option 2: Direct Download from GitHub Releases](#option-2-direct-download-from-github-releases)
- [Option 3: Build from Source (Development)](#option-3-build-from-source-development)
- [Using CodeUChain in Your Project](#using-codeuchain-in-your-project)
- [Troubleshooting](#troubleshooting)

## Installation Options

Choose the installation method that best fits your workflow:

| Method | Best For | Pros | Cons |
|--------|----------|------|------|
| **Conan** | Production projects | Dependency management, easy updates | Requires Conan setup |
| **Direct Download** | Quick start, scripts | Simple, standalone | Manual updates |
| **Build from Source** | Development, contributions | Full control, latest changes | More complex setup |

## Prerequisites

All installation methods require:
- **C++20 compatible compiler**: GCC 10+, Clang 11+, or MSVC 2019+
- **CMake**: Version 3.20 or higher

## Option 1: Conan Package Manager (Recommended)

### Install Conan

```bash
pip install conan
```

### Configure Conan Profile

```bash
conan profile detect --force
```

### Install CodeUChain

**Basic installation:**
```bash
conan install --requires=codeuchain/1.0.0@ --build=missing
```

**With examples and tests:**
```bash
conan install --requires=codeuchain/1.0.0@ \
  -o codeuchain/*:build_examples=True \
  -o codeuchain/*:build_tests=True \
  --build=missing
```

### Using in Your Project

Create a `conanfile.txt`:

```ini
[requires]
codeuchain/1.0.0

[generators]
CMakeDeps
CMakeToolchain

[layout]
cmake_layout
```

Then in your `CMakeLists.txt`:

```cmake
cmake_minimum_required(VERSION 3.20)
project(my_project)

find_package(codeuchain REQUIRED)

add_executable(my_app main.cpp)
target_link_libraries(my_app PRIVATE codeuchain::codeuchain)
```

Build your project:

```bash
# Install dependencies
conan install . --build=missing

# Configure and build
cmake --preset conan-release
cmake --build build/Release
```

## Option 2: Direct Download from GitHub Releases

### Download and Extract

**Using curl (Linux/macOS):**
```bash
curl -L https://github.com/codeuchain/codeuchain/releases/download/v1.0.0/codeuchain-cpp-v1.0.0.tar.gz | tar xz
cd codeuchain-cpp-v1.0.0
```

**Using wget (Linux):**
```bash
wget https://github.com/codeuchain/codeuchain/releases/download/v1.0.0/codeuchain-cpp-v1.0.0.tar.gz
tar -xzf codeuchain-cpp-v1.0.0.tar.gz
cd codeuchain-cpp-v1.0.0
```

**Manual download (Windows/all platforms):**
1. Visit: https://github.com/codeuchain/codeuchain/releases/tag/v1.0.0
2. Download `codeuchain-cpp-v1.0.0.zip` or `codeuchain-cpp-v1.0.0.tar.gz`
3. Extract the archive
4. Navigate to the extracted directory

### Build the Package

**Quick build (using provided script):**
```bash
./build.sh
```

**Manual CMake build:**
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . -j$(nproc)
```

### Run Examples and Tests

```bash
# Run an example
./build/examples/simple_math
./build/examples/typed_context_example
./build/examples/business_workflow

# Run tests
cd build
ctest --output-on-failure
```

### Install System-Wide (Optional)

```bash
cd build
sudo cmake --install .
```

This installs headers to `/usr/local/include/codeuchain/` and libraries to `/usr/local/lib/`.

## Option 3: Build from Source (Development)

### Clone the Repository

```bash
git clone https://github.com/codeuchain/codeuchain.git
cd codeuchain/packages/cpp
```

### Build

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug
cmake --build . -j$(nproc)
```

### Run Tests

```bash
ctest --output-on-failure
```

### Build with Different Options

```bash
# Build without examples
cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_EXAMPLES=OFF

# Build without tests
cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTS=OFF

# Release with optimizations
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-O3 -march=native"
```

## Using CodeUChain in Your Project

### CMake Integration (Installed Package)

If you've installed CodeUChain system-wide or via Conan:

```cmake
find_package(codeuchain REQUIRED)

add_executable(my_app main.cpp)
target_link_libraries(my_app PRIVATE codeuchain::codeuchain)
```

### CMake Integration (Direct Source)

If you've extracted the standalone package or want to include it directly:

```cmake
# Add CodeUChain as a subdirectory
add_subdirectory(path/to/codeuchain-cpp-v1.0.0)

add_executable(my_app main.cpp)
target_link_libraries(my_app PRIVATE codeuchain)
```

### Simple Example Code

```cpp
#include "codeuchain/codeuchain.hpp"
#include <iostream>

// Create a custom link
class MyProcessor : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        std::cout << "Processing..." << std::endl;
        context = context.insert("result", 42);
        co_return {context};
    }

    std::string name() const override { return "my_processor"; }
    std::string description() const override { return "Example processor"; }
};

int main() {
    // Create a chain
    codeuchain::Chain chain;
    chain.add_link("processor", std::make_shared<MyProcessor>());

    // Run the chain
    codeuchain::Context ctx;
    auto future = chain.run(ctx);
    auto result = future.get();

    // Get the result
    if (auto value = result.get("result")) {
        std::cout << "Result: " << std::get<int>(*value) << std::endl;
    }

    return 0;
}
```

## Build Options Reference

| Option | Default | Description |
|--------|---------|-------------|
| `BUILD_EXAMPLES` | `ON` | Build example programs |
| `BUILD_TESTS` | `ON` | Build unit tests |
| `CMAKE_BUILD_TYPE` | `Debug` | Build type (Debug/Release/RelWithDebInfo) |
| `CMAKE_INSTALL_PREFIX` | `/usr/local` | Installation directory |

## Compiler Requirements

### GCC
- **Minimum version**: 10
- **Recommended**: 11 or later
- **Enable C++20**: `-std=c++20` (handled by CMake)

### Clang
- **Minimum version**: 11
- **Recommended**: 13 or later
- **May need**: `libc++` for full C++20 support

### MSVC
- **Minimum version**: 2019 (16.8)
- **Recommended**: 2022 or later
- **Enable C++20**: `/std:c++20` (handled by CMake)

## Troubleshooting

### Issue: "C++20 features not supported"

**Solution:** Update your compiler to a supported version or ensure CMake is detecting the correct compiler:

```bash
cmake .. -DCMAKE_CXX_COMPILER=g++-11
```

### Issue: "conan: command not found"

**Solution:** Install Conan via pip:

```bash
pip install conan
```

### Issue: Build errors with older CMake

**Solution:** Update CMake to 3.20 or later:

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install cmake

# macOS
brew upgrade cmake

# Or download from https://cmake.org/download/
```

### Issue: "Cannot find codeuchain package"

**Solution (Conan):** Ensure you've added the Conan Center remote:

```bash
conan remote add conancenter https://center.conan.io
```

**Solution (CMake):** Set the installation path:

```bash
cmake .. -DCMAKE_PREFIX_PATH=/path/to/installation
```

### Issue: Examples won't build

**Solution:** Ensure examples are enabled:

```bash
cmake .. -DBUILD_EXAMPLES=ON
```

### Issue: Tests fail to run

**Solution:** Build and run tests explicitly:

```bash
cmake .. -DBUILD_TESTS=ON
cmake --build .
ctest --output-on-failure
```

## Getting Help

- **Documentation**: https://github.com/codeuchain/codeuchain/tree/main/packages/cpp
- **Issues**: https://github.com/codeuchain/codeuchain/issues
- **Examples**: See the `examples/` directory in the package

## Next Steps

After installation, check out:

1. **Examples Directory**: See practical usage patterns
   - `examples/simple_math.cpp` - Basic chain usage
   - `examples/typed_context_example.cpp` - Type-safe operations
   - `examples/business_workflow.cpp` - Real-world scenarios

2. **Documentation**: Read the full README for advanced features
   - Branching and conditional execution
   - Middleware system
   - Performance optimization

3. **API Reference**: Explore the header files in `include/codeuchain/`
   - `chain.hpp` - Chain orchestration
   - `context.hpp` - Data container
   - `link.hpp` - Processing units
   - `typed_context.hpp` - Type-safe features

## License

CodeUChain is licensed under the Apache License 2.0. See LICENSE file for details.
