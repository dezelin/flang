cmake_minimum_required(VERSION 3.3)
project(flang)

if (CMAKE_BINARY_DIR STREQUAL ${CMAKE_SOURCE_DIR})
    message(FATAL_ERROR "type: git clean -df && mkdir build && cd build && cmake .. && make")
endif ()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wpedantic -Wall -Wextra")

# Enable verbose compiler output for Debug builds
#if(CMAKE_BUILD_TYPE MATCHES Debug)
#    set(CMAKE_VERBOSE_MAKEFILE 1)
#else()
#    set(CMAKE_VERBOSE_MAKEFILE 0)
#endif()

add_subdirectory(flang-backend-llvm)
