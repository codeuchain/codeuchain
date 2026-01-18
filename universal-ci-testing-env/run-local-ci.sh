#!/bin/bash
# Wrapper to spin up the Universal CI environment using 'act'
# This ensures that your local test runs EXACTLY like the server.

# Get the directory where this script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$DIR/.."

echo "🚀 Spinning up Universal CI Environment..."

# Check for act
if ! command -v act &> /dev/null; then
    echo "❌ 'act' is not installed."
    echo "   Please install it: brew install act (macOS) or see https://github.com/nektos/act"
    exit 1
fi

# Check for .actrc
if [ ! -f "$ROOT_DIR/.actrc" ]; then
    echo "⚠️  .actrc not found at root. Creating default configuration..."
    echo "-P ubuntu-latest=ghcr.io/catthehacker/ubuntu:full-latest" > "$ROOT_DIR/.actrc"
    echo "--container-architecture linux/amd64" >> "$ROOT_DIR/.actrc"
    echo "--rm" >> "$ROOT_DIR/.actrc"
fi

# Run it
echo "🎬 Action! Running 'universal-ci' workflow..."
cd "$ROOT_DIR"
act -j universal-ci -W .github/workflows/universal-ci.yml

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ Local CI Passed!"
else
    echo "❌ Local CI Failed."
fi

exit $EXIT_CODE
