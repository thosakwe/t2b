cmake_minimum_required(VERSION 3.9)
project(t2b)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/bin)

add_executable(t2b main.cc)
install(TARGETS t2b DESTINATION bin)