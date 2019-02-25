cmake_minimum_required(VERSION 3.9)
project(t2b)

set(CMAKE_CXX_STANDARD 11)
set(T2B_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(T2B_SOURCES src/t2b.cc include/t2b/t2b.hpp src/ast.cpp include/t2b/ast.hpp src/value.cpp src/value.hpp)

add_library(t2b SHARED ${T2B_SOURCES})
add_library(t2b_static STATIC ${T2B_SOURCES})
add_executable(t2b_exe src/main.cc)
set_target_properties(t2b_static t2b_exe PROPERTIES OUTPUT_NAME t2b)
target_include_directories(t2b PUBLIC ${T2B_INCLUDE_DIRS})
target_include_directories(t2b_exe PUBLIC ${T2B_INCLUDE_DIRS})
target_include_directories(t2b_static PUBLIC ${T2B_INCLUDE_DIRS})
target_link_libraries(t2b_exe t2b)
install(TARGETS t2b t2b_exe
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
install(DIRECTORY include DESTINATION include)