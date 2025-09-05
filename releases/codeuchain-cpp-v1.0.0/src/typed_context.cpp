#include "codeuchain/typed_context.hpp"
#include <iostream>
#include <string>

namespace codeuchain {

// ===== EXPLICIT TEMPLATE INSTANTIATIONS =====
// These ensure the templates are compiled for common types

template class TypedContext<std::monostate>;  // ContextAny
template class TypedContext<std::string>;     // ContextString
template class TypedContext<int>;             // ContextInt
template class TypedContext<double>;          // ContextDouble
template class TypedContext<bool>;            // ContextBool

} // namespace codeuchain