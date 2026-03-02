add_executable(unit_tests unit_tests.cpp)
target_link_libraries(unit_tests PRIVATE codeuchain)
target_compile_options(unit_tests PRIVATE -Wall -Wextra)

add_executable(test_typed_state test_typed_state.cpp)
target_link_libraries(test_typed_state PRIVATE codeuchain)
target_compile_options(test_typed_state PRIVATE -Wall -Wextra)

add_test(NAME unit_tests COMMAND unit_tests)
add_test(NAME typed_state_tests COMMAND test_typed_state)