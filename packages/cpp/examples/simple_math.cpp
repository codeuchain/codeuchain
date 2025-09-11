#include "codeuchain/codeuchain.hpp"
#include <iostream>
#include <memory>

/*!
Simple Math Example: Demonstrating Universal Patterns in C++

This example shows how the same concepts work across all languages.
This example performs basic arithmetic operations using the universal CodeUChain pattern.
*/

// Simplified Link implementation without coroutines for now
class AddLink : public codeuchain::ILink {
public:
    // Simplified synchronous call for demonstration
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        auto a_opt = context.get("a");
        auto b_opt = context.get("b");

        if (a_opt && b_opt) {
            auto a = std::get<int>(*a_opt);
            auto b = std::get<int>(*b_opt);
            auto result = a + b;

            context = context.insert("result", result);
            std::cout << "AddLink: " << a << " + " << b << " = " << result << std::endl;
        }

        // For now, return synchronously
        co_return {context};
    }

    std::string name() const override { return "add"; }
    std::string description() const override { return "Adds two numbers"; }
};

// Simplified Multiply Link
class MultiplyLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        auto result_opt = context.get("result");
        auto multiplier_opt = context.get("multiplier");

        if (result_opt && multiplier_opt) {
            auto result = std::get<int>(*result_opt);
            auto multiplier = std::get<int>(*multiplier_opt);
            auto final_result = result * multiplier;

            context = context.insert("final_result", final_result);
            std::cout << "MultiplyLink: " << result << " * " << multiplier << " = " << final_result << std::endl;
        }

        co_return {context};
    }

    std::string name() const override { return "multiply"; }
    std::string description() const override { return "Multiplies result by multiplier"; }
};

// Simplified Logging Middleware
class LoggingMiddleware : public codeuchain::IMiddleware {
public:
    std::coroutine_handle<> before(std::shared_ptr<codeuchain::ILink> link, const codeuchain::Context& context) override {
        if (link) {
            std::cout << "[BEFORE] Executing link: " << link->name() << std::endl;
        } else {
            std::cout << "[BEFORE] Chain execution started" << std::endl;
        }
        return nullptr;
    }

    std::coroutine_handle<> after(std::shared_ptr<codeuchain::ILink> link, const codeuchain::Context& context) override {
        if (link) {
            std::cout << "[AFTER] Link completed: " << link->name() << std::endl;
        } else {
            std::cout << "[AFTER] Chain execution completed" << std::endl;
        }
        return nullptr;
    }

    std::string name() const override { return "logging"; }
    std::string description() const override { return "Logs execution flow"; }
};

int main() {
    std::cout << "CodeUChain C++ - Simple Math Example" << std::endl;
    std::cout << "====================================" << std::endl;

    // Create chain
    codeuchain::Chain chain;

    // Add links
    chain.add_link("add", std::make_shared<AddLink>());
    chain.add_link("multiply", std::make_shared<MultiplyLink>());

    // Add middleware
    chain.use_middleware(std::make_shared<LoggingMiddleware>());

    // Create initial context
    codeuchain::Context initial_context;
    initial_context = initial_context.insert("a", 5);
    initial_context = initial_context.insert("b", 3);
    initial_context = initial_context.insert("multiplier", 2);

    // Display initial context
    std::cout << "\nInitial Context:" << std::endl;
    for (const auto& key : initial_context.keys()) {
        if (auto value = initial_context.get(key)) {
            if (auto* int_val = std::get_if<int>(&*value)) {
                std::cout << key << ": " << *int_val << std::endl;
            }
        }
    }

    // Demonstrate mutable operations (for performance-critical scenarios)
    std::cout << "\nDemonstrating Mutable Operations (Performance Optimization):" << std::endl;
    codeuchain::Context mutable_ctx = initial_context;
    mutable_ctx.insert_mut("computed", 42);
    mutable_ctx.update_mut("a", 100);  // Modify existing value

    std::cout << "After mutable operations:" << std::endl;
    if (auto computed = mutable_ctx.get("computed")) {
        std::cout << "computed: " << std::get<int>(*computed) << std::endl;
    }
    if (auto a_val = mutable_ctx.get("a")) {
        std::cout << "a: " << std::get<int>(*a_val) << std::endl;
    }

    std::cout << "\nSame pattern works in ALL languages!" << std::endl;
    std::cout << "Note: Full async execution with coroutines coming soon!" << std::endl;
    std::cout << "Note: Mutable methods available for performance-critical scenarios!" << std::endl;

    return 0;
}