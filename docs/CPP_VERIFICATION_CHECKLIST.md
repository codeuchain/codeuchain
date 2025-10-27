# C++ Standalone Package - Verification Checklist

This checklist helps verify that all components of the C++ standalone package implementation are working correctly.

## Pre-Deployment Checklist

### 1. Repository Configuration

- [ ] GitHub secrets configured (if using Conan):
  - [ ] `CONAN_LOGIN_USERNAME` set
  - [ ] `CONAN_PASSWORD` or API token set
  - [ ] Secrets are accessible to workflows

### 2. Package Structure Verification

- [x] `releases/codeuchain-cpp-v1.0.0/` directory exists
- [x] Contains all required files:
  - [x] CMakeLists.txt (standalone, no repo paths)
  - [x] LICENSE
  - [x] README.md
  - [x] USAGE.md
  - [x] build.sh (executable)
  - [x] conanfile.py
  - [x] include/ directory
  - [x] src/ directory
  - [x] examples/ directory
  - [x] tests/ directory

### 3. Archive Verification

- [x] `codeuchain-cpp-v1.0.0.tar.gz` created
- [x] `codeuchain-cpp-v1.0.0.zip` created
- [x] Archives are reasonably sized (~32KB tar.gz, ~76KB zip)
- [x] Archives contain no build artifacts:
  - [x] No .o files
  - [x] No .so/.dll/.dylib files
  - [x] No .a/.lib files
  - [x] No .exe files
  - [x] No build/ directories
  - [x] No ._* (macOS metadata) files

### 4. Standalone Build Test

- [x] Extract archive to clean location: ✅ Tested in /tmp
- [x] CMake configures successfully: ✅ Passed
- [x] Library builds: ✅ libcodeuchain.a created
- [x] Examples build: ✅ All examples compiled
- [x] Tests build: ✅ All tests compiled
- [x] Examples run correctly: ✅ simple_math works
- [x] Tests pass: ✅ 2/2 tests passed

### 5. Documentation Verification

- [x] `packages/cpp/README.md` updated:
  - [x] Correct download URL (uses releases/download, not raw)
  - [x] Clear installation instructions
  - [x] Both Conan and direct download methods documented
- [x] `packages/cpp/INSTALLATION.md` created:
  - [x] Comprehensive guide for all installation methods
  - [x] Troubleshooting section
  - [x] Examples and usage
- [x] `docs/CONAN_PUBLISHING.md` created:
  - [x] Clear setup instructions
  - [x] Secrets documentation
  - [x] Manual and automated workflows
- [x] `docs/CPP_PACKAGING_SUMMARY.md` created:
  - [x] Complete implementation summary
  - [x] What changed and why
  - [x] Next steps

### 6. CI/CD Workflows

- [x] `.github/workflows/package-cpp-release.yml`:
  - [x] Uses packaging script
  - [x] Validates clean archives
  - [x] Tests standalone build
  - [x] Uploads to releases
  - [x] Proper permissions set
- [x] `.github/workflows/publish_release_assets.yml`:
  - [x] Regenerates clean packages
  - [x] Verifies build before upload
  - [x] Proper permissions set
- [x] `.github/workflows/conan-center-publish.yml`:
  - [x] Dry-run mode available
  - [x] Credential validation
  - [x] Error handling
  - [x] Proper permissions set

### 7. Security

- [x] CodeQL scan passed: ✅ 0 alerts
- [x] No secrets in code
- [x] Workflow permissions properly scoped
- [x] No credentials exposed

### 8. Packaging Script

- [x] `scripts/package_cpp_release.sh` exists
- [x] Script is executable
- [x] Excludes build artifacts
- [x] Excludes macOS metadata
- [x] Creates both tar.gz and zip
- [x] Validates output
- [x] Documented with comments

## Post-Release Verification

Complete these checks after creating a GitHub release:

### Release Assets

- [ ] Navigate to GitHub release page
- [ ] Verify artifacts are attached:
  - [ ] codeuchain-cpp-v1.0.0.tar.gz
  - [ ] codeuchain-cpp-v1.0.0.zip
- [ ] Download and verify file sizes are reasonable
- [ ] Check archive contents (no build artifacts)

### Installation Testing

#### Direct Download Method

- [ ] Download tar.gz from GitHub releases:
  ```bash
  curl -L https://github.com/codeuchain/codeuchain/releases/download/v1.0.0/codeuchain-cpp-v1.0.0.tar.gz | tar xz
  cd codeuchain-cpp-v1.0.0
  ```
- [ ] Build successfully:
  ```bash
  ./build.sh
  ```
- [ ] Run example:
  ```bash
  ./build/examples/simple_math
  ```
- [ ] Run tests:
  ```bash
  cd build && ctest
  ```

#### Conan Method (if published)

- [ ] Install from Conan Center:
  ```bash
  conan install --requires=codeuchain/1.0.0 --build=missing
  ```
- [ ] Create test project
- [ ] Verify package works in project
- [ ] Test with different options (shared, examples, tests)

### Documentation Verification

- [ ] Check all links in README.md work
- [ ] Verify installation instructions are accurate
- [ ] Test commands from documentation
- [ ] Check that examples match actual examples in package

### CI/CD Verification

- [ ] Check that workflows ran successfully
- [ ] Review workflow logs for any warnings
- [ ] Verify artifacts were uploaded correctly
- [ ] Confirm validation jobs passed

## Troubleshooting

If any check fails, refer to:
- `docs/CPP_PACKAGING_SUMMARY.md` - Implementation details
- `packages/cpp/INSTALLATION.md` - Installation help
- `docs/CONAN_PUBLISHING.md` - Conan-specific issues

## Sign-Off

**Completed by:** _________________  
**Date:** _________________  
**Version tested:** _________________  
**Notes:**

---

### Quick Test Command Summary

```bash
# Test standalone package
cd /tmp
curl -L https://github.com/codeuchain/codeuchain/releases/download/v1.0.0/codeuchain-cpp-v1.0.0.tar.gz | tar xz
cd codeuchain-cpp-v1.0.0
./build.sh
./build/examples/simple_math
cd build && ctest

# Test Conan (if published)
conan install --requires=codeuchain/1.0.0 --build=missing
```

### Expected Results

- ✅ Build completes without errors
- ✅ Example runs and produces output
- ✅ Tests pass: "100% tests passed, 0 tests failed out of 2"
- ✅ Archives are small (~32-76 KB)
- ✅ No build artifacts in extracted package

## Implementation Status

- ✅ **Pre-deployment checks:** All passed
- ✅ **Code changes:** Committed and pushed
- ✅ **Documentation:** Complete
- ✅ **Security:** Verified
- ✅ **Testing:** Local validation passed
- ⏳ **Post-release checks:** Pending actual release

**Status:** Ready for production release! 🚀
