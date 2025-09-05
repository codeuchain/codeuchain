#include "codeuchain/typed_context.hpp"
#include <iostream>
#include <string>
#include <memory>

using namespace codeuchain;

/*!
 * @brief Example Link implementation using typed contexts
 */

// Example Link: String to Uppercase
class UppercaseLink : public Link<std::string, std::string> {
public:
    std::string call(const std::string& input) override {
        std::string result = input;
        for (char& c : result) {
            c = std::toupper(c);
        }
        return result;
    }

    DataValue call_runtime(const DataValue& input) override {
        if (std::holds_alternative<std::string>(input)) {
            return DataValue(call(std::get<std::string>(input)));
        }
        return DataValue(); // Empty on type mismatch
    }
};

// Example Link: Add Length
class AddLengthLink : public Link<std::string, std::string> {
public:
    std::string call(const std::string& input) override {
        return input + " (length: " + std::to_string(input.length()) + ")";
    }

    DataValue call_runtime(const DataValue& input) override {
        if (std::holds_alternative<std::string>(input)) {
            return DataValue(call(std::get<std::string>(input)));
        }
        return DataValue();
    }
};

int main() {
    std::cout << "CodeUChain Typed Link Example" << std::endl;
    std::cout << "============================" << std::endl;

    // Create links
    auto uppercase_link = std::make_unique<UppercaseLink>();
    auto length_link = std::make_unique<AddLengthLink>();

    // Test typed interface
    std::cout << "\n1. Typed Link calls:" << std::endl;
    std::string input = "hello world";
    std::string step1 = uppercase_link->call(input);
    std::string result = length_link->call(step1);

    std::cout << "Input: " << input << std::endl;
    std::cout << "After uppercase: " << step1 << std::endl;
    std::cout << "Final result: " << result << std::endl;

    // Test runtime interface
    std::cout << "\n2. Runtime Link calls:" << std::endl;
    DataValue runtime_input = std::string("test string");
    DataValue runtime_step1 = uppercase_link->call_runtime(runtime_input);
    DataValue runtime_result = length_link->call_runtime(runtime_step1);

    if (std::holds_alternative<std::string>(runtime_result)) {
        std::cout << "Runtime result: " << std::get<std::string>(runtime_result) << std::endl;
    }

    // Demonstrate type safety
    std::cout << "\n3. Type safety:" << std::endl;
    DataValue wrong_type = 42;  // int instead of string
    DataValue wrong_result = uppercase_link->call_runtime(wrong_type);

    if (std::holds_alternative<std::monostate>(wrong_result)) {
        std::cout << "Type safety: Wrong input type handled gracefully" << std::endl;
    }

    std::cout << "\nLink example completed successfully!" << std::endl;
    return 0;
}