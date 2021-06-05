#include "scope.h"

#include <memory>
#include <unordered_map>

#include "value.h"

namespace t2b {

std::shared_ptr<Scope> Scope::CreateChild() const {
  auto child = new Scope(shared_from_this());
  return std::shared_ptr<Scope>(child);
}

void Scope::Register(const std::string& key,
                     const std::shared_ptr<Value>& value) {
  values_[key] = value;
}

std::shared_ptr<Value> Scope::Resolve(const std::string& key) const {
  auto it = values_.find(key);
  if (it != values_.end()) {
    return it->second;
  } else if (parent_) {
    return parent_->Resolve(key);
  } else {
    return nullptr;
  }
}

Scope::Scope(std::shared_ptr<const Scope> parent) : parent_(parent) {}
}  // namespace t2b
