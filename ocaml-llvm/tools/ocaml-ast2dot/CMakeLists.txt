cmake_minimum_required(VERSION 3.3)
project(ocaml-ast2dot)

find_package(Boost 1.55 REQUIRED COMPONENTS program_options)
if(NOT Boost_FOUND)
    message(FATAL_ERROR "Couldn't find Boost libraries")
endif()

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

# Includes ocaml-llvm library for lexer, grammar and ast
include_directories(../../include)

if(NOT WIN32)
    # FIXME: Boost Spirit lex::lexertl::token gives this warning
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")
    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        # using regular Clang or AppleClang
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-parameter")
    else()
        # FIXME: Boost Spirit Karma gives this warning with GCC
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-local-typedefs")
    endif()
else()
	# Function call with parameters that may be unsafe 
	# - this call relies on the caller to check that the passed values are correct.
	add_definitions(-D_SCL_SECURE_NO_WARNINGS)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4244 /wd4018")
endif()

include_directories(include)

set(HEADER_FILES
    include/Graph.h
    include/GraphGenerator.h
    include/Options.h
    include/Parser.h
    include/Translator.h
)

set(SOURCE_FILES
    src/Graph.cpp
    src/GraphGenerator.cpp
    src/Options.cpp
    src/Parser.cpp
    src/Translator.cpp
    src/main.cpp
)

add_executable(${PROJECT_NAME} ${SOURCE_FILES} ${HEADER_FILES})
if(NOT WIN32)
    target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES})
endif()

