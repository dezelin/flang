cmake_minimum_required(VERSION 3.3)
project(flang-llvm)

find_package(LLVM 3.6.0 REQUIRED CONFIG)
if (NOT LLVM_FOUND)
    message(FATAL_ERROR "Couldn't find LLVM librarires")
endif ()

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

set(SOURCE_FILES src/dummy.cpp)
add_library(flang-llvm SHARED ${SOURCE_FILES})

enable_testing()
add_subdirectory(tests)
