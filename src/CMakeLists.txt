include(ExternalAntlr4Cpp)
include_directories(${ANTLR4_INCLUDE_DIRS})

set(ANTLR_EXECUTABLE ${THIRD_PARTY_DIR}/antlr-4.9.2-complete.jar)
set(ANTLR4_ZIP_REPOSITORY ${THIRD_PARTY_DIR}/antlr-4.9.2-master.zip)
find_package(ANTLR REQUIRED)

antlr_target(T2BGrammar T2B.g4
  LEXER
  PACKAGE t2b
  PARSER
  VISITOR
)

add_executable(t2b
  ${ANTLR_T2BGrammar_CXX_OUTPUTS}
  interpreter.cc interpreter.h
  main.cc
  scope.cc scope.h
  type.cc type.h
  value.cc value.h
)

target_compile_definitions(t2b PUBLIC
  -DANTLR4CPP_STATIC
)

target_include_directories(t2b PUBLIC
  ${ANTLR4_INCLUDE_DIRS}
  ${ANTLR_T2BGrammar_OUTPUT_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(t2b antlr4_static)

message(STATUS ${ANTLR4_INCLUDE_DIRS})
