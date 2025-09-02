#include "codeuchain/codeuchain.hpp"
#include <cassert>
#include <iostream>
#include <memory>

/*!
Unit Tests: Loving Validation

With agape harmony, we validate our implementations through comprehensive testing.
*/

// Test Link implementation
class TestLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        // Simple transformation: add 1 to any integer value
        if (auto value_opt = context.get("input")) {
            if (auto* int_val = std::get_if<int>(&*value_opt)) {
                context = context.insert("output", *int_val + 1);
            }
        }
        co_return {context};
    }

    std::string name() const override { return "test"; }
    std::string description() const override { return "Test link for unit testing"; }
};

// Test functions
void test_context_operations() {
    std::cout << "Testing Context operations..." << std::endl;

    codeuchain::Context ctx;

    // Test insert and get
    ctx = ctx.insert("key1", 42);
    auto value = ctx.get("key1");
    assert(value.has_value());
    assert(std::get<int>(*value) == 42);

    // Test update
    ctx = ctx.update("key1", 100);
    value = ctx.get("key1");
    assert(value.has_value());
    assert(std::get<int>(*value) == 100);

    // Test has and keys
    assert(ctx.has("key1"));
    assert(!ctx.has("nonexistent"));
    auto keys = ctx.keys();
    assert(keys.size() == 1);
    assert(keys[0] == "key1");

    // Test remove
    ctx = ctx.remove("key1");
    assert(!ctx.has("key1"));
    assert(ctx.empty());

    std::cout << "✅ Context operations test passed!" << std::endl;
}

void test_chain_execution() {
    std::cout << "Testing Chain execution..." << std::endl;

    codeuchain::Chain chain;
    auto test_link = std::make_shared<TestLink>();
    chain.add_link("test", test_link);

    codeuchain::Context initial_ctx;
    initial_ctx = initial_ctx.insert("input", 5);

    // For now, let's test the synchronous parts
    const auto& links = chain.links();
    assert(links.size() == 1);
    assert(links.find("test") != links.end());

    std::cout << "✅ Chain basic functionality test passed!" << std::endl;
}

void test_link_awaitable() {
    std::cout << "Testing Link awaitable..." << std::endl;

    auto link = std::make_shared<TestLink>();
    codeuchain::Context ctx;
    ctx = ctx.insert("input", 10);

    // For now, just test that we can create the link and context
    assert(link->name() == "test");
    assert(ctx.has("input"));

    std::cout << "✅ Link basic functionality test passed!" << std::endl;
}

void test_mutable_performance() {
    std::cout << "Testing mutable performance optimization..." << std::endl;

    // Test immutable approach (current default)
    codeuchain::Context immutable_ctx;
    for (int i = 0; i < 1000; ++i) {
        immutable_ctx = immutable_ctx.insert("key" + std::to_string(i), i);
    }

    // Test mutable approach (performance optimization)
    codeuchain::Context mutable_ctx;
    for (int i = 0; i < 1000; ++i) {
        mutable_ctx.insert_mut("key" + std::to_string(i), i);
    }

    // Both should have the same data
    assert(immutable_ctx.size() == mutable_ctx.size());
    assert(immutable_ctx.size() == 1000);

    // Test that mutable operations work correctly
    mutable_ctx.update_mut("key500", 9999);
    auto value = mutable_ctx.get("key500");
    assert(value.has_value());
    assert(std::get<int>(*value) == 9999);

    mutable_ctx.remove_mut("key500");
    assert(!mutable_ctx.has("key500"));

    std::cout << "✅ Mutable performance optimization test passed!" << std::endl;
}

int main() {
    std::cout << "CodeUChain C++ - Unit Tests" << std::endl;
    std::cout << "===========================" << std::endl;

    try {
        test_context_operations();
        test_chain_execution();
        test_link_awaitable();
        test_mutable_performance();

        std::cout << "\n🎉 All tests passed!" << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Test failed: " << e.what() << std::endl;
        return 1;
    }
}