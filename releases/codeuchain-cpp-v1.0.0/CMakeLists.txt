cmake_minimum_required(VERSION 3.20)
project(codeuchain VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(BUILD_EXAMPLES "Build examples" ON)
option(BUILD_TESTS "Build tests" ON)

# Library sources
file(GLOB_RECURSE CODEUCHAIN_SOURCES "src/*.cpp")
file(GLOB_RECURSE CODEUCHAIN_HEADERS "include/*.hpp")

# Create library
add_library(codeuchain ${CODEUCHAIN_SOURCES})
target_include_directories(codeuchain PUBLIC 
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_compile_features(codeuchain PUBLIC cxx_std_20)

# Add compiler warnings
if(MSVC)
    target_compile_options(codeuchain PRIVATE /W4)
else()
    target_compile_options(codeuchain PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Installation rules
install(TARGETS codeuchain
    EXPORT codeuchain-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.hpp"
)

install(EXPORT codeuchain-targets
    FILE codeuchain-config.cmake
    NAMESPACE codeuchain::
    DESTINATION lib/cmake/codeuchain
)
