#ifndef T2B_STATE_H_
#define T2B_STATE_H_

#include <memory>
#include <string>

#include "scope.h"
#include "value.h"

namespace t2b {
class State {
 public:
  enum class Type {
    kDefault,
    kReturned,
    kErrored,
  };

  Type type;
  std::string error_message;
  std::shared_ptr<Value> return_value;
  std::shared_ptr<Scope> scope;
};
}  // namespace t2b

#endif  // T2B_STATE_H_
