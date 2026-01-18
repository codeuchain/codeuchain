#!/bin/bash
# Run the test suite for Universal CI

echo "🧪 Running Universal CI Test Suite..."

# Check if we're in the right directory
if [ ! -f "verify.py" ]; then
    echo "❌ Error: verify.py not found. Run this script from universal-ci-testing-env/"
    exit 1
fi

# Install test dependencies
echo "📦 Installing test dependencies..."
pip3 install -r tests/requirements.txt

# Run tests
echo "🚀 Running tests..."
python3 -m pytest tests/ -v

# Run a quick integration test
echo "🔗 Running integration test..."
python3 verify.py --config tests/test-config.json

echo "✅ Test suite completed!"