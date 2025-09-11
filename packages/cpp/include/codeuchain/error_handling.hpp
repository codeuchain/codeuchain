#pragma once

#include <string>

/*!
Error Handling: Comprehensive Error Management

Handle errors gracefully and provide meaningful feedback.
*/

namespace codeuchain {

void log_error(const std::string& message);
void log_warning(const std::string& message);
void log_info(const std::string& message);

} // namespace codeuchain