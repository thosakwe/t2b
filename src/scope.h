#ifndef T2B_SCOPE_H_
#define T2B_SCOPE_H_

#include <memory>
#include <unordered_map>

#include "value.h"

namespace t2b {
class Scope : public std::enable_shared_from_this<Scope> {
 public:
  Scope() = default;

  std::shared_ptr<Scope> CreateChild() const;

  void Register(const std::string& key, const std::shared_ptr<Value>& value);

  std::shared_ptr<Value> Resolve(const std::string& key) const;

 private:
  explicit Scope(std::shared_ptr<const Scope> parent);
  std::shared_ptr<const Scope> parent_;
  std::unordered_map<std::string, std::shared_ptr<Value>> values_;
};

}  // namespace t2b

#endif  // T2B_SCOPE_H_
