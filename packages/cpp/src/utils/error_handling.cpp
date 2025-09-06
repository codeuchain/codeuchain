#include "codeuchain/error_handling.hpp"
#include <iostream>
#include <string>

namespace codeuchain {

void log_error(const std::string& message) {
    std::cerr << "[ERROR] " << message << std::endl;
}

void log_warning(const std::string& message) {
    std::cout << "[WARNING] " << message << std::endl;
}

void log_info(const std::string& message) {
    std::cout << "[INFO] " << message << std::endl;
}

} // namespace codeuchain