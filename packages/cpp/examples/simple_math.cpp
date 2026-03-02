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
    codeuchain::LinkAwaitable call(codeuchain::State state) override {
        auto a_opt = state.get("a");
        auto b_opt = state.get("b");

        if (a_opt && b_opt) {
            auto a = std::get<int>(*a_opt);
            auto b = std::get<int>(*b_opt);
            auto result = a + b;

            state = state.insert("result", result);
            std::cout << "AddLink: " << a << " + " << b << " = " << result << std::endl;
        }

        // For now, return synchronously
        co_return {state};
    }

    std::string name() const override { return "add"; }
    std::string description() const override { return "Adds two numbers"; }
};

// Simplified Multiply Link
class MultiplyLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::State state) override {
        auto result_opt = state.get("result");
        auto multiplier_opt = state.get("multiplier");

        if (result_opt && multiplier_opt) {
            auto result = std::get<int>(*result_opt);
            auto multiplier = std::get<int>(*multiplier_opt);
            auto final_result = result * multiplier;

            state = state.insert("final_result", final_result);
            std::cout << "MultiplyLink: " << result << " * " << multiplier << " = " << final_result << std::endl;
        }

        co_return {state};
    }

    std::string name() const override { return "multiply"; }
    std::string description() const override { return "Multiplies result by multiplier"; }
};

// Simplified Logging Hook
class LoggingHook : public codeuchain::IHook {
public:
    std::coroutine_handle<> before(std::shared_ptr<codeuchain::ILink> link, const codeuchain::State& state) override {
        if (link) {
            std::cout << "[BEFORE] Executing link: " << link->name() << std::endl;
        } else {
            std::cout << "[BEFORE] Chain execution started" << std::endl;
        }
        return nullptr;
    }

    std::coroutine_handle<> after(std::shared_ptr<codeuchain::ILink> link, const codeuchain::State& state) override {
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

    // Add hook
    chain.use_hook(std::make_shared<LoggingHook>());

    // Create initial state
    codeuchain::State initial_state;
    initial_state = initial_state.insert("a", 5);
    initial_state = initial_state.insert("b", 3);
    initial_state = initial_state.insert("multiplier", 2);

    // Display initial state
    std::cout << "\nInitial State:" << std::endl;
    for (const auto& key : initial_state.keys()) {
        if (auto value = initial_state.get(key)) {
            if (auto* int_val = std::get_if<int>(&*value)) {
                std::cout << key << ": " << *int_val << std::endl;
            }
        }
    }

    // Demonstrate mutable operations (for performance-critical scenarios)
    std::cout << "\nDemonstrating Mutable Operations (Performance Optimization):" << std::endl;
    codeuchain::State mutable_ctx = initial_state;
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