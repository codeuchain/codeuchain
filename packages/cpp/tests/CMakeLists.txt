add_executable(unit_tests unit_tests.cpp)
target_link_libraries(unit_tests PRIVATE codeuchain)
target_compile_options(unit_tests PRIVATE -Wall -Wextra)

add_executable(test_typed_context test_typed_context.cpp)
target_link_libraries(test_typed_context PRIVATE codeuchain)
target_compile_options(test_typed_context PRIVATE -Wall -Wextra)

add_test(NAME unit_tests COMMAND unit_tests)
add_test(NAME typed_context_tests COMMAND test_typed_context)