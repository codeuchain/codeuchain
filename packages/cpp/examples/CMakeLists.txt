add_executable(simple_math simple_math.cpp)
target_link_libraries(simple_math PRIVATE codeuchain)
target_compile_options(simple_math PRIVATE -Wall -Wextra)