#!/usr/bin/env bash
set -euo pipefail

echo "Building CodeUChain C++ release package"
ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$ROOT_DIR/build"

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build . --config Release

echo "Build complete. Binaries (examples) are in: $BUILD_DIR/examples" 
