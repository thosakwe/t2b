cmake_minimum_required(VERSION 3.9)
project(t2b)

set(CMAKE_CXX_STANDARD 11)

add_executable(t2b main.cc t2b.cc t2b.hpp)
install(TARGETS t2b DESTINATION bin)