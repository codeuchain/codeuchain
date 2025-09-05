cmake_minimum_required(VERSION 3.20)
project(codeuchain VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Find required packages
find_package(Threads REQUIRED)

# Create library
add_library(codeuchain
    src/core/context.cpp
    src/core/link.cpp
    src/core/chain.cpp
    src/core/middleware.cpp
    src/core/timing_middleware.cpp
    src/utils/error_handling.cpp
    src/typed_context.cpp
)

# Include directories
target_include_directories(codeuchain
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
)

# Link libraries
target_link_libraries(codeuchain
    PUBLIC
        Threads::Threads
)

# Set compile options
target_compile_options(codeuchain PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Werror
)

# Install library
install(TARGETS codeuchain
    EXPORT codeuchain-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

# Install headers
install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.hpp"
)

# Export targets
install(EXPORT codeuchain-targets
    FILE codeuchain-targets.cmake
    NAMESPACE codeuchain::
    DESTINATION lib/cmake/codeuchain
)

# Create and install config file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/codeuchain-config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/codeuchain-config.cmake
    INSTALL_DESTINATION lib/cmake/codeuchain
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/codeuchain-config.cmake
    DESTINATION lib/cmake/codeuchain
)

# Examples
option(BUILD_EXAMPLES "Build examples" ON)
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Tests
option(BUILD_TESTS "Build tests" ON)
if(BUILD_TESTS AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tests")
    enable_testing()
    add_subdirectory(tests)
endif()