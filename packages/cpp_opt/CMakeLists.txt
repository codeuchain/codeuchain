cmake_minimum_required(VERSION 3.20)
project(codeuchain_opt LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Assume parent workspace has already built core codeuchain library when top-level configured.
add_library(codeuchain_opt INTERFACE)
target_include_directories(codeuchain_opt INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../cpp/include>
)

# Try to locate core codeuchain library in sibling build (if not provided by parent)
if(NOT TARGET codeuchain)
    # Allow user to specify path: -DCODEUCHAIN_LIB_DIR=
    set(CODEUCHAIN_LIB_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../cpp/build" CACHE PATH "Location of built codeuchain library")
    find_library(CODEUCHAIN_LIB NAMES codeuchain PATHS ${CODEUCHAIN_LIB_DIR} NO_DEFAULT_PATH)
    if(NOT CODEUCHAIN_LIB)
        message(FATAL_ERROR "Could not find core codeuchain library. Build packages/cpp first or set CODEUCHAIN_LIB_DIR.")
    endif()
    add_library(codeuchain UNKNOWN IMPORTED)
    set_target_properties(codeuchain PROPERTIES IMPORTED_LOCATION ${CODEUCHAIN_LIB})
    target_include_directories(codeuchain INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/../cpp/include)
endif()

# Example executable demonstrating StaticChain vs dynamic Chain
add_executable(static_chain_demo examples/static_chain_demo.cpp)
target_link_libraries(static_chain_demo PRIVATE codeuchain codeuchain_opt)
target_compile_options(static_chain_demo PRIVATE -Wall -Wextra -Wpedantic)
