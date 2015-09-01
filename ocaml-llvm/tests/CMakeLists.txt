find_package(Boost 1.55 COMPONENTS unit_test_framework REQUIRED)
if (NOT Boost_FOUND)
    message(FATAL_ERROR "Couldn't find Boost unit test framework")
endif ()

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

include_directories(../include)

# Generate main() for tests
#add_definitions(-DBOOST_TEST_DYN_LINK)

add_executable(ocaml-llvm-dummy_test dummy_test.cpp)
target_link_libraries(ocaml-llvm-dummy_test ${Boost_LIBRARIES})
add_test(NAME test_dummy COMMAND ocaml-llvm-dummy_test)

add_executable(ocaml-llvm-ocamllexer_test ocamllexer_test.cpp)
target_link_libraries(ocaml-llvm-ocamllexer_test ${Boost_LIBRARIES})
add_test(NAME test_ocamllexer COMMAND ocaml-llvm-ocamllexer_test)
