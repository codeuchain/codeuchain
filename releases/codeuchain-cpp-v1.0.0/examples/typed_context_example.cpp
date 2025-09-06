#include "codeuchain/typed_context.hpp"
#include <iostream>
#include <string>
#include <vector>

using namespace codeuchain;

/*!
 * @brief Simple example demonstrating typed context usage
 */

int main() {
    std::cout << "CodeUChain Typed Context Example" << std::endl;
    std::cout << "=================================" << std::endl;

    // 1. Create typed context
    std::cout << "\n1. Creating typed context..." << std::endl;
    std::unordered_map<std::string, DataValue> empty_data;
    auto ctx = make_typed_context<std::string>(empty_data);

    // 2. Type-safe operations
    std::cout << "2. Type-safe insert operations..." << std::endl;
    auto ctx2 = ctx.insert("name", std::string("Alice"));
    auto ctx3 = ctx2.insert("age", 30);
    auto ctx4 = ctx3.insert("active", true);

    // 3. Type-safe retrieval
    std::cout << "3. Type-safe retrieval..." << std::endl;
    auto name = ctx4.get_typed<std::string>("name");
    auto age = ctx4.get_typed<int>("age");
    auto active = ctx4.get_typed<bool>("active");

    if (name) std::cout << "Name: " << *name << std::endl;
    if (age) std::cout << "Age: " << *age << std::endl;
    if (active) std::cout << "Active: " << (*active ? "Yes" : "No") << std::endl;

    // 4. Type evolution with insert_as()
    std::cout << "4. Type evolution with insert_as()..." << std::endl;
    auto ctx5 = ctx4.insert_as<double>("score", 95.5);

    auto score = ctx5.get_typed<double>("score");
    if (score) std::cout << "Score: " << *score << std::endl;

    // 5. Runtime flexibility
    std::cout << "5. Runtime flexibility..." << std::endl;
    auto base_ctx = ctx5.to_context();
    auto runtime_name = base_ctx.get("name");

    if (runtime_name && std::holds_alternative<std::string>(*runtime_name)) {
        std::cout << "Runtime name: " << std::get<std::string>(*runtime_name) << std::endl;
    }

    // 6. Demonstrate type safety
    std::cout << "6. Type safety demonstration..." << std::endl;
    auto wrong_type = ctx4.get_typed<double>("name");  // Try to get string as double
    if (!wrong_type) {
        std::cout << "Type safety: Cannot get string as double (expected)" << std::endl;
    }

    std::cout << "\nExample completed successfully!" << std::endl;
    return 0;
}