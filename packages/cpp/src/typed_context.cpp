#include "codeuchain/typed_state.hpp"
#include <iostream>
#include <string>

namespace codeuchain {

// ===== EXPLICIT TEMPLATE INSTANTIATIONS =====
// These ensure the templates are compiled for common types

template class TypedState<std::monostate>;  // StateAny
template class TypedState<std::string>;     // StateString
template class TypedState<int>;             // StateInt
template class TypedState<double>;          // StateDouble
template class TypedState<bool>;            // StateBool

} // namespace codeuchain