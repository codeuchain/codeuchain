#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VERSION="${1:-v1.0.0}"
SRC="$ROOT/packages/cpp"
DEST="$ROOT/releases/codeuchain-cpp-$VERSION"

echo "Packaging CodeUChain C++ $VERSION..."

# Clean and create destination directory
rm -rf "$DEST"
mkdir -p "$DEST"

# Copy source files (exclude build artifacts and unnecessary files)
rsync -av \
  --exclude 'build' \
  --exclude 'build_*' \
  --exclude '.git' \
  --exclude '*.o' \
  --exclude '*.so' \
  --exclude '*.dll' \
  --exclude '*.dylib' \
  --exclude '*.a' \
  --exclude '*.exe' \
  --exclude '.DS_Store' \
  --exclude '._*' \         # macOS metadata files created by Finder
  --exclude '__pycache__' \
  --exclude '*.pyc' \
  --exclude '.pytest_cache' \
  --exclude 'test_consumer' \
  --exclude '*.backup' \
  --prune-empty-dirs \
  "$SRC/include" \
  "$SRC/src" \
  "$SRC/examples" \
  "$SRC/tests" \
  "$SRC/cmake" \
  "$SRC/conanfile.py" \
  "$SRC/conanprofile" \
  "$SRC/LICENSE" \
  "$DEST/"

# Create standalone CMakeLists.txt
cat > "$DEST/CMakeLists.txt" <<'EOF'
cmake_minimum_required(VERSION 3.20)
project(codeuchain VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(BUILD_EXAMPLES "Build examples" ON)
option(BUILD_TESTS "Build tests" ON)

# Library sources
file(GLOB_RECURSE CODEUCHAIN_SOURCES "src/*.cpp")
file(GLOB_RECURSE CODEUCHAIN_HEADERS "include/*.hpp")

# Create library
add_library(codeuchain ${CODEUCHAIN_SOURCES})
target_include_directories(codeuchain PUBLIC 
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_compile_features(codeuchain PUBLIC cxx_std_20)

# Add compiler warnings
if(MSVC)
    target_compile_options(codeuchain PRIVATE /W4)
else()
    target_compile_options(codeuchain PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Installation rules
install(TARGETS codeuchain
    EXPORT codeuchain-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.hpp"
)

install(EXPORT codeuchain-targets
    FILE codeuchain-config.cmake
    NAMESPACE codeuchain::
    DESTINATION lib/cmake/codeuchain
)
EOF

# Create standalone build.sh script
cat > "$DEST/build.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

echo "Building CodeUChain C++ release package..."
ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$ROOT_DIR/build"

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . --config Release -j$(nproc 2>/dev/null || echo 4)

echo ""
echo "Build complete!"
echo "  Library: $BUILD_DIR/libcodeuchain.a"
echo "  Examples: $BUILD_DIR/examples/"
echo "  Tests: $BUILD_DIR/tests/"
echo ""
echo "To run an example:"
echo "  ./build/examples/simple_math"
echo ""
echo "To run tests:"
echo "  cd build && ctest"
EOF
chmod +x "$DEST/build.sh"

# Create README.md
cat > "$DEST/README.md" <<'EOF'
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
EOF

# Create USAGE.md
cat > "$DEST/USAGE.md" <<'EOF'
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
EOF

echo "✅ Package created at: $DEST"
echo ""
echo "Contents:"
ls -lh "$DEST"
echo ""

# Create archives
echo "Creating archives..."
cd "$ROOT/releases"
# Exclude macOS metadata files from both archives
tar --exclude='._*' -czf "codeuchain-cpp-$VERSION.tar.gz" "codeuchain-cpp-$VERSION"
# Zip uses different pattern syntax - need full path with wildcard
zip -r "codeuchain-cpp-$VERSION.zip" "codeuchain-cpp-$VERSION" -x "*/._*"

echo "✅ Archives created:"
ls -lh "$ROOT/releases/codeuchain-cpp-$VERSION".{tar.gz,zip}
