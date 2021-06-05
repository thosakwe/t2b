#include "value.h"

#include <memory>
#include <ostream>

namespace t2b {

std::shared_ptr<Value> Value::Invoke(std::shared_ptr<Value> arg) { return arg; }

BoolValue::BoolValue(bool value) : value_(value) {}

void BoolValue::Write(std::ostream& out) const {
  out << (value_ ? "true" : "false");
}

NumValue::NumValue(double value) : value_(value) {}

void NumValue::Write(std::ostream& out) const {
  out << value_;
}

}  // namespace t2b
