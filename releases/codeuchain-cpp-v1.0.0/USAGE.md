CodeUChain C++ - Release Package
================================

This archive contains only the C++ package source required to build CodeUChain C++ library and examples.

Files included:
- `src/` - implementation source files
- `include/` - public headers (if present)
- `examples/` - example programs
- `CMakeLists.txt` - top-level CMake for building the package
- `conanfile.py` - Conan recipe if you use Conan

Quick build (Unix/macOS):

```bash
mkdir build && cd build
cmake ..
cmake --build . --config Release
```

If you prefer Conan to manage dependencies, run:

```bash
conan create .. --name codeuchain --version 1.0.0 --build=missing
```

Enjoy!
