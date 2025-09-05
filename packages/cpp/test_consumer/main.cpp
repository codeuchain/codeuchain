#include <codeuchain/codeuchain.hpp>
#include <iostream>
#include <memory>

class SimpleLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        std::cout << "Hello from Conan-installed CodeUChain!" << std::endl;
        context = context.insert("message", std::string("Conan test successful"));
        co_return {context};
    }

    std::string name() const override { return "simple"; }
    std::string description() const override { return "Simple test link"; }
};

int main() {
    std::cout << "Testing CodeUChain via Conan..." << std::endl;

    codeuchain::Chain chain;
    chain.add_link("test", std::make_shared<SimpleLink>());

    codeuchain::Context ctx;
    auto result = chain.run(ctx).get();

    if (auto msg = result.get("message")) {
        if (auto* str = std::get_if<std::string>(&*msg)) {
            std::cout << "Result: " << *str << std::endl;
            std::cout << "✅ Conan package test successful!" << std::endl;
            return 0;
        }
    }

    std::cout << "❌ Test failed" << std::endl;
    return 1;
}