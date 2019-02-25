cmake_minimum_required(VERSION 3.9)
project(t2b)

set(CMAKE_CXX_STANDARD 11)
set(T2B_SOURCES t2b.cc t2b.hpp)

add_library(t2b SHARED ${T2B_SOURCES})
add_library(t2b_static STATIC ${T2B_SOURCES})
add_executable(t2b_exe main.cc)
set_target_properties(t2b_static t2b_exe PROPERTIES OUTPUT_NAME t2b)
target_link_libraries(t2b_exe t2b)
install(TARGETS t2b t2b_exe
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)