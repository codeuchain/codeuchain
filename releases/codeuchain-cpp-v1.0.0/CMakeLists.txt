cmake_minimum_required(VERSION 3.20)
project(codeuchain_cpp_release VERSION 1.0.0 LANGUAGES CXX)

# This CMake is intentionally small and delegates to the original package sources.
add_subdirectory(../../packages/cpp/src ${CMAKE_BINARY_DIR}/codeuchain_src EXCLUDE_FROM_ALL)
add_subdirectory(../../packages/cpp/examples ${CMAKE_BINARY_DIR}/examples EXCLUDE_FROM_ALL)
