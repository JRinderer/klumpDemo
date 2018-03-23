cmake_minimum_required(VERSION 3.8)
project(klumpDemo)

set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES main.cpp error.cpp error.h scanner.cpp scanner.h tables.cpp tables.h assembler.cpp assembler.h)
add_executable(klumpDemo ${SOURCE_FILES})