#ifndef T2B_VALUE_H_
#define T2B_VALUE_H_

#include <memory>
#include <ostream>

namespace t2b {
class ValueVisitor;

class Value {
 public:
  virtual std::shared_ptr<Value> Invoke(std::shared_ptr<Value> arg);
  // virtual void Accept(ValueVisitor& visitor) = 0;
  virtual void Write(std::ostream& out) const = 0;
};

class BoolValue : public Value {
 public:
  explicit BoolValue(bool value);
  // void Accept(ValueVisitor& visitor) override;
  void Write(std::ostream& out) const override;

 private:
  bool value_;
};

class NumValue : public Value {
 public:
  explicit NumValue(double value);
  // void Accept(ValueVisitor& visitor) override;
  void Write(std::ostream& out) const override;

 private:
  double value_;
};

}  // namespace t2b

#endif  // T2B_VALUE_H_
