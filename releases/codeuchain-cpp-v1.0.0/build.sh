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
