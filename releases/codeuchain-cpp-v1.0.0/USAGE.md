# CodeUChain C++ - Quick Usage Guide

## Build and Run

```bash
# Build everything
./build.sh

# Run an example
./build/examples/simple_math
./build/examples/typed_context_example
./build/examples/business_workflow

# Run tests
cd build && ctest
```

## Install with Conan

```bash
conan install codeuchain/1.0.0@
```

## Use in Your CMake Project

```cmake
find_package(codeuchain REQUIRED)
add_executable(myapp main.cpp)
target_link_libraries(myapp PRIVATE codeuchain::codeuchain)
```

For more details, see README.md
