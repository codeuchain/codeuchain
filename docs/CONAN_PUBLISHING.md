# Conan Center Publishing Guide

This document describes how to publish the CodeUChain C++ package to Conan Center.

## Overview

CodeUChain C++ is available through Conan Center Index, the central repository for Conan packages. This guide covers:

- Prerequisites and setup
- Publishing process (automated and manual)
- Troubleshooting
- Maintenance

## Prerequisites

### 1. Conan Center Account

Create an account at [Conan Center](https://conan.io/center/).

### 2. GitHub Secrets Configuration

Add the following secrets to your GitHub repository (Settings → Secrets and variables → Actions):

| Secret Name | Description | How to Get |
|-------------|-------------|------------|
| `CONAN_LOGIN_USERNAME` | Your Conan Center username | Your login username |
| `CONAN_PASSWORD` | Your Conan Center password or API token | Account settings → API tokens (recommended) |

**⚠️ Security Note:** Use API tokens instead of passwords for better security. Generate one at: https://conan.io/center/user/settings

### 3. Local Development Setup

Install Conan locally for testing:

```bash
pip install conan>=2.0
conan profile detect --force
```

## Publishing Methods

### Method 1: Automated via GitHub Actions (Recommended)

The repository includes a GitHub Actions workflow that automatically publishes to Conan Center on releases.

#### Automatic on Release

When you create a new GitHub release, the workflow automatically:
1. Creates the Conan package
2. Tests it locally
3. Uploads to Conan Center
4. Validates the published package

#### Manual Trigger

You can also manually trigger the workflow:

1. Go to: Actions → "Publish to Conan Center"
2. Click "Run workflow"
3. Enter the version (e.g., `1.0.0`)
4. Choose whether to do a dry run (test only, no upload)
5. Click "Run workflow"

**Dry Run Mode**: Test package creation without uploading:
- Check the "Dry run" option
- Workflow will create and validate the package locally
- No upload will occur
- Useful for testing before actual release

### Method 2: Manual Publishing

For development or one-off publishes, you can publish manually from your local machine.

#### Step 1: Navigate to Package Directory

```bash
cd packages/cpp
```

#### Step 2: Create Package Locally

```bash
conan create . --build=missing
```

This creates the package in your local Conan cache and runs all tests.

#### Step 3: Login to Conan Center

```bash
conan remote add conancenter https://center.conan.io
conan remote login conancenter <username>
```

Enter your password or API token when prompted.

#### Step 4: Upload Package

```bash
conan upload "codeuchain/1.0.0" -r conancenter --confirm
```

#### Step 5: Verify Upload

```bash
# Clear local cache
conan remove "codeuchain/1.0.0" -c

# Install from Conan Center
conan install --requires=codeuchain/1.0.0 --build=missing
```

## Package Configuration

The package is configured in `packages/cpp/conanfile.py`:

```python
class CodeUChainConan(ConanFile):
    name = "codeuchain"
    version = "1.0.0"
    license = "Apache-2.0"
    author = "CodeUChain Team"
    url = "https://github.com/codeuchain/codeuchain"
    description = "Universal Chain Processing Framework - C++ Implementation"
```

### Build Options

Users can customize the package with these options:

| Option | Default | Description |
|--------|---------|-------------|
| `shared` | `False` | Build as shared library |
| `fPIC` | `True` | Position-independent code (Linux/Mac) |
| `build_examples` | `False` | Build example programs |
| `build_tests` | `False` | Build unit tests |

Example usage:
```bash
conan install --requires=codeuchain/1.0.0 \
  -o codeuchain/*:build_examples=True \
  -o codeuchain/*:build_tests=True
```

## Version Updates

When releasing a new version:

### 1. Update Version in conanfile.py

```python
class CodeUChainConan(ConanFile):
    name = "codeuchain"
    version = "1.0.1"  # ← Update this
    # ...
```

### 2. Update Version in CMakeLists.txt

```cmake
project(codeuchain VERSION 1.0.1 LANGUAGES CXX)
```

### 3. Update Packaging Script

Edit `scripts/package_cpp_release.sh` if the default version changed.

### 4. Create GitHub Release

Create a new release with tag `v1.0.1`. The CI workflow will automatically publish to Conan Center.

### 5. Verify Publication

After the workflow completes:

```bash
# Check the package is available
conan search codeuchain/1.0.1 -r conancenter

# Test installation
conan install --requires=codeuchain/1.0.1 --build=missing
```

## Testing Before Release

Always test the package locally before publishing:

### 1. Create Package

```bash
cd packages/cpp
conan create . --build=missing
```

### 2. Test in Consumer Project

Create a test project in `packages/cpp/test_consumer/`:

```cmake
# CMakeLists.txt
cmake_minimum_required(VERSION 3.20)
project(test_consumer)

find_package(codeuchain REQUIRED)

add_executable(test_app main.cpp)
target_link_libraries(test_app codeuchain::codeuchain)
```

```cpp
// main.cpp
#include "codeuchain/codeuchain.hpp"
int main() {
    codeuchain::Context ctx;
    return 0;
}
```

Build and test:

```bash
cd test_consumer
conan install . --build=missing
cmake --preset conan-release
cmake --build build/Release
./build/Release/test_app
```

### 3. Test Different Configurations

```bash
# Test with examples
conan create . -o build_examples=True --build=missing

# Test with tests
conan create . -o build_tests=True --build=missing

# Test shared library
conan create . -o shared=True --build=missing
```

## Troubleshooting

### Issue: "Authentication failed"

**Cause:** Invalid credentials or missing secrets

**Solution:**
1. Verify secrets are set in GitHub repository settings
2. For local publishing, ensure you've logged in:
   ```bash
   conan remote login conancenter <username>
   ```
3. Use API token instead of password for better reliability

### Issue: "Package already exists"

**Cause:** Version already published to Conan Center

**Solution:**
1. If you need to update: bump the version number
2. Conan Center doesn't allow overwriting existing versions
3. For fixes to same version, contact Conan Center support

### Issue: "Build failed"

**Cause:** Package doesn't build on CI

**Solution:**
1. Test locally first with `conan create . --build=missing`
2. Check CI logs for specific errors
3. Ensure all dependencies are properly declared in `conanfile.py`
4. Verify CMake configuration works standalone

### Issue: "Recipe validation failed"

**Cause:** Conan Center has strict requirements

**Solution:**
1. Review [Conan Center guidelines](https://github.com/conan-io/conan-center-index/blob/master/docs/adding_packages/README.md)
2. Ensure `conanfile.py` follows best practices
3. Check that all required attributes are set
4. Verify license identifier is correct (SPDX format)

### Issue: Workflow fails with "Secrets not found"

**Cause:** Required GitHub secrets are not configured

**Solution:**
1. Add `CONAN_LOGIN_USERNAME` and `CONAN_PASSWORD` secrets
2. Go to: Repository Settings → Secrets and variables → Actions
3. Click "New repository secret"
4. Add both secrets with your Conan Center credentials

## Maintenance

### Regular Tasks

1. **Monitor Package Health**: Check Conan Center for package status
2. **Update Dependencies**: Keep package dependencies current
3. **Test New Versions**: Always test before publishing
4. **Respond to Issues**: Monitor GitHub issues related to Conan

### Best Practices

1. **Semantic Versioning**: Follow [semver](https://semver.org/) for version numbers
2. **Changelog**: Maintain CHANGELOG.md with version updates
3. **Testing**: Test on multiple platforms (CI does this automatically)
4. **Documentation**: Keep installation guides up to date

### Conan Center Index Pull Request

For official Conan Center Index inclusion, you may need to submit a PR to:
https://github.com/conan-io/conan-center-index

This makes your package officially listed and maintained by Conan Center. The process involves:

1. Fork the conan-center-index repository
2. Add your recipe following their structure
3. Submit PR with required documentation
4. Address review comments
5. Wait for approval and merge

**Note:** The current workflow publishes to your own Conan remote. For official CCI listing, follow their contribution guide.

## Resources

- **Conan Documentation**: https://docs.conan.io/
- **Conan Center Index**: https://github.com/conan-io/conan-center-index
- **Recipe Guidelines**: https://github.com/conan-io/conan-center-index/blob/master/docs/recipe_templates/
- **Support**: https://github.com/conan-io/conan/issues

## Security Notes

- **Never commit credentials** to the repository
- **Use API tokens** instead of passwords
- **Rotate tokens** periodically
- **Limit token permissions** to what's necessary
- **Review secret access** in GitHub audit logs

## Contact

For questions or issues with Conan publishing:
- Open an issue: https://github.com/codeuchain/codeuchain/issues
- Tag with: `conan` and `packaging`
