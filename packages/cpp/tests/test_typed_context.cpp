#include "codeuchain/typed_context.hpp"
#include <iostream>
#include <cassert>

using namespace codeuchain;

void test_basic_typed_operations() {
    std::cout << "Testing basic typed operations..." << std::endl;

    // Create typed context
    auto ctx = make_typed_context<std::string>(Context{});

    // Test type-safe insert
    auto ctx2 = ctx.insert("name", std::string("Alice"));
    auto ctx3 = ctx2.insert("age", 30);

    // Test type-safe retrieval
    auto name = ctx3.get_typed<std::string>("name");
    auto age = ctx3.get_typed<int>("age");

    assert(name.has_value() && "Name should be present");
    assert(*name == "Alice" && "Name should be Alice");

    assert(age.has_value() && "Age should be present");
    assert(*age == 30 && "Age should be 30");

    std::cout << "✓ Basic typed operations test passed" << std::endl;
}

void test_type_evolution() {
    std::cout << "Testing type evolution..." << std::endl;

    // Start with string context
    auto ctx = make_typed_context<std::string>(Context{});
    auto ctx2 = ctx.insert("data", std::string("hello"));

    // Evolve to different type
    auto ctx3 = ctx2.insert_as<int>("count", 42);

    // Verify type evolution worked
    auto count = ctx3.get_typed<int>("count");
    assert(count.has_value() && "Count should be present");
    assert(*count == 42 && "Count should be 42");

    // Original data should still be accessible via base context
    auto base_ctx = ctx3.to_context();
    auto data = base_ctx.get("data");
    assert(data.has_value() && "Data should be present");
    assert(std::holds_alternative<std::string>(*data) && "Data should be string");
    assert(std::get<std::string>(*data) == "hello" && "Data should be hello");

    std::cout << "✓ Type evolution test passed" << std::endl;
}

void test_type_safety() {
    std::cout << "Testing type safety..." << std::endl;

    // Create context with string data
    auto ctx = make_typed_context<std::string>(Context{});
    auto ctx2 = ctx.insert("name", std::string("Alice"));

    // Try to get string as int (should fail)
    auto wrong_type = ctx2.get_typed<int>("name");
    assert(!wrong_type.has_value() && "Wrong type should not be retrievable");

    // Try to get non-existent key
    auto missing = ctx2.get_typed<std::string>("missing");
    assert(!missing.has_value() && "Missing key should not be retrievable");

    std::cout << "✓ Type safety test passed" << std::endl;
}

void test_runtime_compatibility() {
    std::cout << "Testing runtime compatibility..." << std::endl;

    // Create typed context
    auto ctx = make_typed_context<std::string>(Context{});
    auto ctx2 = ctx.insert("name", std::string("Alice"));

    // Access via base context
    auto base_ctx = ctx2.to_context();
    auto runtime_name = base_ctx.get("name");

    assert(runtime_name.has_value() && "Runtime name should be present");
    assert(std::holds_alternative<std::string>(*runtime_name) && "Runtime name should be string");
    assert(std::get<std::string>(*runtime_name) == "Alice" && "Runtime name should be Alice");

    std::cout << "✓ Runtime compatibility test passed" << std::endl;
}

void test_context_operations() {
    std::cout << "Testing context operations..." << std::endl;

    // Test basic context operations
    auto ctx = make_typed_context<std::string>(Context{});
    auto ctx2 = ctx.insert("key1", std::string("value1"));
    auto ctx3 = ctx2.insert("key2", std::string("value2"));

    // Test size
    assert(ctx3.size() == 2u && "Size should be 2");
    assert(!ctx3.empty() && "Context should not be empty");

    // Test keys
    auto keys = ctx3.keys();
    assert(keys.size() == 2u && "Keys size should be 2");
    assert(std::find(keys.begin(), keys.end(), "key1") != keys.end() && "key1 should be in keys");
    assert(std::find(keys.begin(), keys.end(), "key2") != keys.end() && "key2 should be in keys");

    // Test has
    assert(ctx3.has("key1") && "Should have key1");
    assert(ctx3.has("key2") && "Should have key2");
    assert(!ctx3.has("missing") && "Should not have missing key");

    std::cout << "✓ Context operations test passed" << std::endl;
}

int main() {
    std::cout << "CodeUChain Typed Context Tests" << std::endl;
    std::cout << "===============================" << std::endl;

    try {
        test_basic_typed_operations();
        test_type_evolution();
        test_type_safety();
        test_runtime_compatibility();
        test_context_operations();

        std::cout << std::endl << "🎉 All tests passed!" << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "❌ Test failed: " << e.what() << std::endl;
        return 1;
    }
}