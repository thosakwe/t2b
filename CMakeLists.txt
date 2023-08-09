cmake_minimum_required(VERSION 3.9)
project(t2b)

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)
FLEX_TARGET(t2b_scanner
        ${CMAKE_CURRENT_SOURCE_DIR}/src/t2b_scanner.l ${CMAKE_CURRENT_BINARY_DIR}/t2b_scanner.cpp
        DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/t2b_scanner.hpp)
BISON_TARGET(t2b_parser
        ${CMAKE_CURRENT_SOURCE_DIR}/src/t2b_parser.y ${CMAKE_CURRENT_BINARY_DIR}/t2b_parser.cpp
        DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/t2b_parser.hpp)
ADD_FLEX_BISON_DEPENDENCY(t2b_scanner t2b_parser)

set(CMAKE_CXX_STANDARD 11)
set(T2B_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(T2B_SOURCES ${FLEX_t2b_scanner_OUTPUTS} ${BISON_t2b_parser_OUTPUTS}
        src/t2b.cc include/t2b/t2b.hpp src/ast.cpp
        include/t2b/ast.hpp src/value.cpp include/t2b/value.hpp
        src/visitor.cpp include/t2b/visitor.hpp)

add_library(t2b SHARED ${T2B_SOURCES})
add_library(t2b_static STATIC ${T2B_SOURCES})
add_executable(t2b_exe src/main.cc)
set_target_properties(t2b_static t2b_exe PROPERTIES OUTPUT_NAME t2b)
target_include_directories(t2b PUBLIC ${T2B_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(t2b_static PUBLIC ${T2B_INCLUDE_DIRS} ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(t2b_exe PUBLIC ${T2B_INCLUDE_DIRS})
target_link_libraries(t2b_exe t2b)
install(TARGETS t2b t2b_exe
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
install(DIRECTORY include DESTINATION include)