from conan import ConanFile
from conan.tools.cmake import CMakeToolchain, CMakeDeps, CMake, cmake_layout
from conan.tools.files import copy
import os


class CodeUChainConan(ConanFile):
    name = "codeuchain"
    version = "1.0.0"
    license = "Apache-2.0"
    author = "CodeUChain Team"
    url = "https://github.com/codeuchain/codeuchain"
    description = "Universal Chain Processing Framework - C++ Implementation"
    topics = ("cpp", "chain", "processing", "coroutines", "async")
    settings = "os", "compiler", "build_type", "arch"
    options = {
        "shared": [True, False],
        "fPIC": [True, False],
        "build_examples": [True, False],
        "build_tests": [True, False]
    }
    default_options = {
        "shared": False,
        "fPIC": True,
        "build_examples": False,
        "build_tests": False
    }
    exports_sources = "CMakeLists.txt", "cmake/*", "include/*", "src/*", "examples/*", "tests/*"

    def config_options(self):
        if self.settings.os == "Windows":
            del self.options.fPIC

    def configure(self):
        if self.options.shared:
            self.options.rm_safe("fPIC")

    def layout(self):
        cmake_layout(self)

    def generate(self):
        tc = CMakeToolchain(self)
        tc.variables["BUILD_EXAMPLES"] = self.options.build_examples
        tc.variables["BUILD_TESTS"] = self.options.build_tests
        tc.generate()

        deps = CMakeDeps(self)
        deps.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def package(self):
        copy(self, "LICENSE", self.source_folder, self.package_folder)
        copy(self, "*.hpp", os.path.join(self.source_folder, "include"), os.path.join(self.package_folder, "include"))
        copy(self, "*.h", os.path.join(self.source_folder, "include"), os.path.join(self.package_folder, "include"))
        copy(self, "*.lib", self.build_folder, os.path.join(self.package_folder, "lib"), keep_path=False)
        copy(self, "*.dll", self.build_folder, os.path.join(self.package_folder, "bin"), keep_path=False)
        copy(self, "*.dylib*", self.build_folder, os.path.join(self.package_folder, "lib"), keep_path=False)
        copy(self, "*.so*", self.build_folder, os.path.join(self.package_folder, "lib"), keep_path=False)
        copy(self, "*.a", self.build_folder, os.path.join(self.package_folder, "lib"), keep_path=False)

    def package_info(self):
        self.cpp_info.libs = ["codeuchain"]
        self.cpp_info.includedirs = ["include"]
        if self.settings.os in ["Linux", "FreeBSD"]:
            self.cpp_info.system_libs = ["pthread"]

    def package_id(self):
        # Make package ID independent of build options that don't affect the binary
        self.info.options.build_examples = "Any"
        self.info.options.build_tests = "Any"