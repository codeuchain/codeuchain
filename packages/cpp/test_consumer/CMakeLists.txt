cmake_minimum_required(VERSION 3.20)
project(test_consumer CXX)

find_package(codeuchain REQUIRED)

add_executable(test_consumer main.cpp)
target_link_libraries(test_consumer codeuchain::codeuchain)
target_compile_features(test_consumer PRIVATE cxx_std_20)