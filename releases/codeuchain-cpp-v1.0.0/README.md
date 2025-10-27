# CodeUChain C++ - Standalone Release Package

This is the standalone release package for CodeUChain C++ implementation.

## Quick Start

### Option 1: Build from Source (Direct Download)

```bash
# Extract the archive (if you haven't already)
tar -xzf codeuchain-cpp-v1.0.0.tar.gz
cd codeuchain-cpp-v1.0.0

# Build the library and examples
./build.sh

# Run an example
./build/examples/simple_math
```

### Option 2: Install via Conan

```bash
# Add CodeUChain package
conan install codeuchain/1.0.0@

# Or with examples and tests
conan install codeuchain/1.0.0@ -o build_examples=True -o build_tests=True
```

## What's Included

- `include/` - Public header files
- `src/` - Library implementation
- `examples/` - Example programs demonstrating usage
- `tests/` - Unit tests
- `cmake/` - CMake configuration files
- `conanfile.py` - Conan package recipe
- `CMakeLists.txt` - Standalone CMake build configuration
- `build.sh` - Quick build helper script

## Requirements

- C++20 compatible compiler (GCC 10+, Clang 11+, MSVC 2019+)
- CMake 3.20 or higher
- (Optional) Conan 2.0+ for package management

## Manual CMake Build

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . -j$(nproc)
```

### Build Options

- `BUILD_EXAMPLES` - Build example programs (default: ON)
- `BUILD_TESTS` - Build unit tests (default: ON)

Example:
```bash
cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_EXAMPLES=OFF -DBUILD_TESTS=OFF
```

## Using in Your Project

### CMake

```cmake
find_package(codeuchain REQUIRED)
target_link_libraries(your_target PRIVATE codeuchain::codeuchain)
```

### Conan

```ini
[requires]
codeuchain/1.0.0

[generators]
CMakeDeps
CMakeToolchain
```

## Documentation

For comprehensive documentation, examples, and tutorials, visit:
https://github.com/codeuchain/codeuchain/tree/main/packages/cpp

## License

Apache License 2.0 - See LICENSE file for details
