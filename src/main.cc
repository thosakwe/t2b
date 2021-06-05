#include <antlr4-runtime.h>

#include <iostream>
#include <memory>

#include "T2BLexer.h"
#include "T2BParser.h"
#include "interpreter.h"
#include "state.h"

int main() {
  auto scope = std::make_shared<t2b::Scope>();

  while (true) {
    std::string line;
    std::cout << "t2b> ";
    getline(std::cin, line);

    antlr4::ANTLRInputStream inputStream(line);
    t2b::T2BLexer lexer(&inputStream);
    antlr4::CommonTokenStream tokens(&lexer);
    t2b::T2BParser parser(&tokens);

    auto expr = parser.expr();
    t2b::Interpreter interpreter(scope);

    antlrcpp::Any result = expr->accept(&interpreter);
    if (result.is<std::shared_ptr<t2b::State>>()) {
      auto state = result.as<std::shared_ptr<t2b::State>>();
      switch (state->type) {
        case t2b::State::Type::kDefault:
        case t2b::State::Type::kReturned: {
          if (state->return_value) {
            state->return_value->Write(std::cout);
            std::cout << std::endl;
          }
        } break;
        case t2b::State::Type::kErrored: {
          std::cout << "error: " << state->error_message << std::endl;
        } break;
      }
    }
  }

  return 0;
}
