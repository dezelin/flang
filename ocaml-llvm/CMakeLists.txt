cmake_minimum_required(VERSION 3.3)
project(ocaml-llvm)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sequence-point")

find_package(LLVM REQUIRED)
if (NOT LLVM_FOUND)
    message(FATAL_ERROR "Couldn't find LLVM librarires")
endif ()

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

find_package(Boost 1.55 REQUIRED)
if (NOT Boost_FOUND)
    message(FATAL_ERROR "Couldn't find Boost Spirit Parser framework")
endif ()

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

include_directories(include)

set(HEADER_FILES include/ocamllexer.h include/ocamlgrammar.h)
set(SOURCE_FILES src/dummy.cpp)
add_library(ocaml-llvm SHARED ${SOURCE_FILES})

enable_testing()
add_subdirectory(tests)
