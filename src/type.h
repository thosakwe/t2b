#ifndef T2B_TYPE_H_
#define T2B_TYPE_H_

#include <memory>
#include <string>

namespace t2b {
  class TypeVisitor;

  class Type {
    public:
      virtual void Accept(TypeVisitor& visitor) = 0;
      virtual const std::string GetName() const = 0;
  };
}

#endif  // T2B_TYPE_H_
