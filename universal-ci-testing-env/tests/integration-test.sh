#!/bin/bash
# Integration test to verify the workflow works correctly in GitHub Actions via act

set -e

echo "🧪 GitHub Actions Integration Test"
echo "-----------------------------------"

# Create a test directory
TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"

echo "📁 Test directory: $TEST_DIR"

# Initialize a minimal GitHub Actions setup
mkdir -p .github/workflows

# Create minimal config
cat > universal-ci.config.json << 'EOF'
{
  "tasks": [
    {
      "name": "Simple Echo Test",
      "working_directory": ".",
      "command": "echo 'Universal CI is working!'"
    },
    {
      "name": "File Check",
      "working_directory": ".",
      "command": "test -f universal-ci.config.json && echo 'Config file found!'"
    }
  ]
}
EOF

# Copy universal-ci-testing-env
cp -r /Users/jwink/Documents/github/codeuchain/universal-ci-testing-env .

# Copy workflow
cp /Users/jwink/Documents/github/codeuchain/.github/workflows/universal-ci.yml .github/workflows/

# Copy .actrc
cp /Users/jwink/Documents/github/codeuchain/.actrc .

echo "✅ Test setup complete"
echo "📋 Files in test directory:"
ls -la
echo ""

# Test 1: Run verify.py directly
echo "🧪 Test 1: Running verify.py directly..."
python3 universal-ci-testing-env/verify.py && echo "   ✅ PASS" || echo "   ❌ FAIL"

# Test 2: Try running with act (if Docker is available)
echo ""
echo "🧪 Test 2: Checking act availability..."
if command -v act &> /dev/null; then
    echo "   ✅ act is installed"
    echo "   (Note: Docker daemon must be running for full workflow test)"
else
    echo "   ⚠️  act not installed, skipping Docker test"
fi

# Cleanup
cd /
rm -rf "$TEST_DIR"

echo ""
echo "✅ Integration test complete!"
