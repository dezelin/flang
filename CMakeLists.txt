cmake_minimum_required(VERSION 3.3)
project(flang)

if (CMAKE_BINARY_DIR STREQUAL ${CMAKE_SOURCE_DIR})
    message(FATAL_ERROR "type: git clean -df && mkdir build && cd build && cmake .. && make")
endif ()

set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wpedantic -Wall -Wextra -Wno-deprecated-declarations")

# Set LLVM directory from environment
set(LLVM_DIR $ENV{LLVM_DIR})
message(STATUS "llvm library prefix: " ${LLVM_DIR})

# Set Boost directory from environment
set(BOOST_ROOT $ENV{BOOST_DIR})
message(STATUS "boost library prefix: " ${BOOST_ROOT})

# Enable verbose compiler output for Debug builds
#if(CMAKE_BUILD_TYPE MATCHES Debug)
#    set(CMAKE_VERBOSE_MAKEFILE 1)
#else()
#    set(CMAKE_VERBOSE_MAKEFILE 0)
#endif()

add_subdirectory(flang-llvm)
add_subdirectory(flang-runtime-llvm)
add_subdirectory(ocaml-llvm)

message(STATUS "Build type: " ${CMAKE_BUILD_TYPE})
message(STATUS "Compiler cxx debug flags:" ${CMAKE_CXX_FLAGS_DEBUG})
message(STATUS "Compiler cxx release flags:" ${CMAKE_CXX_FLAGS_RELEASE})
message(STATUS "Compiler cxx min size flags:" ${CMAKE_CXX_FLAGS_MINSIZEREL})
message(STATUS "Compiler cxx flags:" ${CMAKE_CXX_FLAGS})
