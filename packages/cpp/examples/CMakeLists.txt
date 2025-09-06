add_executable(simple_math simple_math.cpp)
target_link_libraries(simple_math PRIVATE codeuchain)
target_compile_options(simple_math PRIVATE -Wall -Wextra)

# Typed features examples
add_executable(typed_context_example typed_context_example.cpp)
target_link_libraries(typed_context_example PRIVATE codeuchain)
target_compile_options(typed_context_example PRIVATE -Wall -Wextra)

add_executable(typed_link_example typed_link_example.cpp)
target_link_libraries(typed_link_example PRIVATE codeuchain)
target_compile_options(typed_link_example PRIVATE -Wall -Wextra)

# Benchmark executable
add_executable(benchmark_chain benchmark_chain.cpp)
target_link_libraries(benchmark_chain PRIVATE codeuchain)
target_compile_options(benchmark_chain PRIVATE -Wall -Wextra -O3)

# Business workflow example
add_executable(business_workflow business_workflow.cpp)
target_link_libraries(business_workflow PRIVATE codeuchain)
target_compile_options(business_workflow PRIVATE -Wall -Wextra -O3)