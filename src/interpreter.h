#ifndef T2B_INTERPRETER_H_
#define T2B_INTERPRETER_H_

#include <T2BVisitor.h>
#include <antlr4-runtime.h>

#include <memory>
#include <stack>

#include "scope.h"
#include "value.h"

namespace t2b {
class Interpreter : public T2BVisitor {
 public:
  explicit Interpreter(std::shared_ptr<Scope> initial_scope);

  antlrcpp::Any visitIdExpr(T2BParser::IdExprContext *context) override;

  antlrcpp::Any visitTrueExpr(T2BParser::TrueExprContext *context) override;

  antlrcpp::Any visitParenExpr(T2BParser::ParenExprContext *context) override;

  antlrcpp::Any visitIdPattern(T2BParser::IdPatternContext *context) override;

  antlrcpp::Any visitIgnorePattern(
      T2BParser::IgnorePatternContext *context) override;

  antlrcpp::Any visitNumExpr(T2BParser::NumExprContext *context) override;

  antlrcpp::Any visitCallExpr(T2BParser::CallExprContext *context) override;

  antlrcpp::Any visitFalseExpr(T2BParser::FalseExprContext *context) override;

 private:
  std::shared_ptr<Scope> getCurrentScope();

  std::stack<std::shared_ptr<Scope>> scope_stack_;
};
}  // namespace t2b

#endif  // T2B_INTERPRETER_H_
