#include "interpreter.h"

#include <T2BVisitor.h>
#include <antlr4-runtime.h>

#include <memory>
#include <stack>
#include <string>

#include "scope.h"
#include "state.h"
#include "value.h"

namespace t2b {
Interpreter::Interpreter(std::shared_ptr<Scope> initial_scope) {
  scope_stack_.push(initial_scope);
}

std::shared_ptr<Scope> Interpreter::getCurrentScope() {
  if (scope_stack_.empty()) {
    return nullptr;
  } else {
    return scope_stack_.top();
  }
}

antlrcpp::Any Interpreter::visitIdExpr(T2BParser::IdExprContext *context) {
  auto value = getCurrentScope()->Resolve(context->ID()->getText());
  if (!value) {
    auto state = std::make_shared<State>();
    state->type = State::Type::kErrored;
    state->error_message = "No symbol \"" + context->ID()->getText() +
                           "\" exists in this context.";
    return state;
  }

  auto state = std::make_shared<State>();
  state->type = State::Type::kDefault;
  state->return_value = value;
  return state;
}

antlrcpp::Any Interpreter::visitTrueExpr(T2BParser::TrueExprContext *context) {
  auto value = std::make_shared<BoolValue>(true);
  auto state = std::make_shared<State>();
  state->type = State::Type::kDefault;
  state->return_value = value;
  return state;
}

antlrcpp::Any Interpreter::visitFalseExpr(
    T2BParser::FalseExprContext *context) {
  auto value = std::make_shared<BoolValue>(false);
  auto state = std::make_shared<State>();
  state->type = State::Type::kDefault;
  state->return_value = value;
  return state;
}

antlrcpp::Any Interpreter::visitNumExpr(T2BParser::NumExprContext *context) {
  auto value = std::make_shared<NumValue>(std::stod(context->NUM()->getText()));
  auto state = std::make_shared<State>();
  state->type = State::Type::kDefault;
  state->return_value = value;
  return state;
}

antlrcpp::Any Interpreter::visitParenExpr(
    T2BParser::ParenExprContext *context) {
  return context->expr()->accept(this);
}

antlrcpp::Any Interpreter::visitIdPattern(
    T2BParser::IdPatternContext *context) {
  return antlrcpp::Any();
}

antlrcpp::Any Interpreter::visitIgnorePattern(
    T2BParser::IgnorePatternContext *context) {
  return antlrcpp::Any();
}

antlrcpp::Any Interpreter::visitCallExpr(T2BParser::CallExprContext *context) {
  auto target = context->target->accept(this).as<std::shared_ptr<State>>();
  if (target->type != State::Type::kDefault) {
    return target;
  }

  auto arg = context->arg->accept(this).as<std::shared_ptr<State>>();
  if (arg->type != State::Type::kDefault) {
    return arg;
  }

  auto value = target->return_value->Invoke(arg->return_value);
  auto state = std::make_shared<State>();
  state->type = State::Type::kDefault;
  state->return_value = value;
  return state;
}

}  // namespace t2b
