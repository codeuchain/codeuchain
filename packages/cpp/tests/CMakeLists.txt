add_executable(unit_tests unit_tests.cpp)
target_link_libraries(unit_tests PRIVATE codeuchain)
target_compile_options(unit_tests PRIVATE -Wall -Wextra)

add_test(NAME unit_tests COMMAND unit_tests)