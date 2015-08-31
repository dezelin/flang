find_package(Boost 1.57 COMPONENTS unit_test_framework REQUIRED)
if (NOT Boost_FOUND)
    message(FATAL_ERROR "Couldn't find Boost unit test framework")
endif ()

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

# Generate main() for tests
#add_definitions(-DBOOST_TEST_DYN_LINK)

add_executable(dummy_test dummy_test.cpp)
target_link_libraries(dummy_test ${Boost_LIBRARIES})
add_test(NAME test_dummy COMMAND dummy_test)
