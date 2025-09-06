#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
RELEASE_DIR="$ROOT/releases/codeuchain-cpp-v1.0.0"
OUT_DIR="$ROOT/releases"
VERSION="v1.0.0"

echo "Creating archives for CodeUChain C++ $VERSION..."

cd "$OUT_DIR"
tar -czf "codeuchain-cpp-$VERSION.tar.gz" -C "$RELEASE_DIR" .
zip -r "codeuchain-cpp-$VERSION.zip" -j "$RELEASE_DIR"/* || true

echo "Created: $OUT_DIR/codeuchain-cpp-$VERSION.tar.gz"
echo "Created: $OUT_DIR/codeuchain-cpp-$VERSION.zip"
