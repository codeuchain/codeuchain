#pragma once

#include <string>

/*!
Error Handling: Loving Error Management

With agape harmony, we handle errors gracefully and provide meaningful feedback.
*/

namespace codeuchain {

void log_error(const std::string& message);
void log_warning(const std::string& message);
void log_info(const std::string& message);

} // namespace codeuchain