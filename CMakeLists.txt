cmake_minimum_required(VERSION 3.11)
project(t2b)
include(FetchContent)

set(CMAKE_CXX_STANDARD 17)
set(THIRD_PARTY_DIR ${PROJECT_SOURCE_DIR}/third_party)

# Get ANTLR CMake sources.
FetchContent_Declare(antlr4
  GIT_REPOSITORY "https://github.com/antlr/antlr4.git"
  GIT_TAG 4.9.2
  GIT_SHALLOW ON
)
FetchContent_MakeAvailable(antlr4)
list(APPEND CMAKE_MODULE_PATH ${antlr4_SOURCE_DIR}/runtime/Cpp/cmake)

add_subdirectory(src)

# install(TARGETS t2b t2b_exe
#         RUNTIME DESTINATION bin
#         LIBRARY DESTINATION lib
#         ARCHIVE DESTINATION lib)
# install(DIRECTORY include DESTINATION include)
