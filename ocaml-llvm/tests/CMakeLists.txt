find_package(Boost 1.55 COMPONENTS system filesystem unit_test_framework
        REQUIRED)
if (NOT Boost_FOUND)
    message(FATAL_ERROR "Couldn't find Boost unit test framework")
endif ()

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

include_directories(../include)

if(NOT WIN32)
# FIXME: Boost Spirit lex::lexertl::token gives this warning
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")
# FIXME: Boost Spirit Karma gives this warning
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")
endif()

# Generate main() for tests
#add_definitions(-DBOOST_TEST_DYN_LINK)

set(OCAML_TEST_CASE_DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ocaml-test-data)
add_definitions(-DOCAML_TEST_CASE_DATA_DIR="${OCAML_TEST_CASE_DATA_DIR}")

add_executable(ocaml-llvm-dummy_test dummy_test.cpp)
target_link_libraries(ocaml-llvm-dummy_test ${Boost_LIBRARIES})
add_test(NAME test_dummy COMMAND ocaml-llvm-dummy_test)

add_executable(ocaml-llvm-ocamllexer_test ocamllexer_test.cpp)
target_link_libraries(ocaml-llvm-ocamllexer_test ${Boost_LIBRARIES})
add_test(NAME test_ocamllexer COMMAND ocaml-llvm-ocamllexer_test)

add_executable(ocaml-llvm-ocamlgrammar_test ocamlgrammar_test.cpp)
target_link_libraries(ocaml-llvm-ocamlgrammar_test ${Boost_LIBRARIES})
add_test(NAME test_ocamlgrammar COMMAND ocaml-llvm-ocamlgrammar_test)
