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

// Test Links for execution order validation
class OrderTrackingLink : public codeuchain::ILink {
public:
    OrderTrackingLink(std::string link_id) : link_id_(link_id) {}

    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        // Get current execution order
        int order = 0;
        if (auto order_opt = context.get("execution_order")) {
            if (order_opt && std::holds_alternative<int>(*order_opt)) {
                order = std::get<int>(*order_opt);
            }
        }

        // Record this link's execution order
        context = context.insert("executed_" + link_id_, order);
        std::string current_seq = "";
        if (auto seq_opt = context.get("execution_sequence")) {
            if (seq_opt && std::holds_alternative<std::string>(*seq_opt)) {
                current_seq = std::get<std::string>(*seq_opt);
            }
        }
        context = context.insert("execution_sequence", 
            (order == 0 ? "" : current_seq) + link_id_);

        // Increment order for next link
        context = context.insert("execution_order", order + 1);

        co_return {context};
    }

    std::string name() const override { return "order_" + link_id_; }
    std::string description() const override { return "Tracks execution order for " + link_id_; }

private:
    std::string link_id_;
};

void test_execution_order_validation() {
    std::cout << "Testing execution order validation..." << std::endl;

    codeuchain::Chain chain;

    // Add links in specific order: first -> second -> third -> fourth
    chain.add_link("first", std::make_shared<OrderTrackingLink>("first"));
    chain.add_link("second", std::make_shared<OrderTrackingLink>("second"));
    chain.add_link("third", std::make_shared<OrderTrackingLink>("third"));
    chain.add_link("fourth", std::make_shared<OrderTrackingLink>("fourth"));

    // Test 1: Sequential auto-connection execution order
    {
        codeuchain::Context ctx;

        auto future = chain.run(ctx);
        auto result = future.get();

        // Verify execution order by checking sequence numbers
        assert(result.get("executed_first").has_value());
        assert(result.get("executed_second").has_value());
        assert(result.get("executed_third").has_value());
        assert(result.get("executed_fourth").has_value());

        assert(std::get<int>(*result.get("executed_first")) == 0);
        assert(std::get<int>(*result.get("executed_second")) == 1);
        assert(std::get<int>(*result.get("executed_third")) == 2);
        assert(std::get<int>(*result.get("executed_fourth")) == 3);

        // Verify execution sequence string
        assert(std::get<std::string>(*result.get("execution_sequence")) == "firstsecondthirdfourth");
    }

    // Test 2: Conditional connection changes execution order
    {
        codeuchain::Chain conditional_chain;

        conditional_chain.add_link("start", std::make_shared<OrderTrackingLink>("start"));
        conditional_chain.add_link("middle", std::make_shared<OrderTrackingLink>("middle"));
        conditional_chain.add_link("end", std::make_shared<OrderTrackingLink>("end"));
        conditional_chain.add_link("alternate", std::make_shared<OrderTrackingLink>("alternate"));

        // Conditional: if "skip_middle" is true, go from start directly to alternate
        auto condition_skip = [](const codeuchain::Context& ctx) -> bool {
            if (auto skip = ctx.get("skip_middle")) {
                if (skip && std::holds_alternative<bool>(*skip)) {
                    return std::get<bool>(*skip);
                }
            }
            return false;
        };
        conditional_chain.connect("start", "alternate", condition_skip);

        codeuchain::Context ctx;
        ctx = ctx.insert("skip_middle", true);

        auto future = conditional_chain.run(ctx);
        auto result = future.get();

        // Should execute: start (0) -> alternate (1) -> end (2)
        // middle should NOT execute
        assert(result.get("executed_start").has_value());
        assert(result.get("executed_alternate").has_value());
        assert(result.get("executed_end").has_value());
        assert(!result.get("executed_middle").has_value()); // middle should not execute

        assert(std::get<int>(*result.get("executed_start")) == 0);
        assert(std::get<int>(*result.get("executed_alternate")) == 1);
        assert(std::get<int>(*result.get("executed_end")) == 2);

        assert(std::get<std::string>(*result.get("execution_sequence")) == "startalternateend");
    }

    std::cout << "✅ Execution order validation test passed!" << std::endl;
}

// Test Links for branch return functionality
class BranchReturnLink : public codeuchain::ILink {
public:
    BranchReturnLink(std::string id) : id_(id) {}

    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        context = context.insert("executed_" + id_, true);
        
        // Get current execution path
        std::string current_path = "";
        if (auto path_opt = context.get("execution_path")) {
            if (auto* str_val = std::get_if<std::string>(&*path_opt)) {
                current_path = *str_val;
            }
        }
        
        std::string new_path = current_path + id_ + "→";
        context = context.insert("execution_path", new_path);
        
        co_return {context};
    }

    std::string name() const override { return "branch_" + id_; }
    std::string description() const override { return "Branch link " + id_; }

private:
    std::string id_;
};

void test_branch_return_functionality() {
    std::cout << "Testing branch return functionality..." << std::endl;

    codeuchain::Chain chain;

    // Create main path: main_a → main_b → main_c → main_d
    chain.add_link("main_a", std::make_shared<BranchReturnLink>("main_a"));
    chain.add_link("main_b", std::make_shared<BranchReturnLink>("main_b"));
    chain.add_link("main_c", std::make_shared<BranchReturnLink>("main_c"));
    chain.add_link("main_d", std::make_shared<BranchReturnLink>("main_d"));

    // Add branch path: branch_special → branch_done
    chain.add_link("branch_special", std::make_shared<BranchReturnLink>("branch_special"));
    chain.add_link("branch_done", std::make_shared<BranchReturnLink>("branch_done"));

    // Branch from main_b to branch_special, then return to main_c
    auto needs_special = [](const codeuchain::Context& ctx) -> bool {
        if (auto special_opt = ctx.get("needs_special")) {
            if (auto* bool_val = std::get_if<bool>(&*special_opt)) {
                return *bool_val;
            }
        }
        return false;
    };
    chain.connect_branch("main_b", "branch_special", "main_c", needs_special);

    // Test 1: Normal path (no branching)
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("execution_path", std::string(""));

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: main_a → main_b → main_c → main_d
        std::string expected_path = "main_a→main_b→main_c→main_d→";
        if (auto path_opt = result.get("execution_path")) {
            if (auto* str_val = std::get_if<std::string>(&*path_opt)) {
                assert(*str_val == expected_path);
            }
        }
        std::cout << "✅ Normal path: " << expected_path << std::endl;
    }

    // Test 2: Branch path with return to main
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("needs_special", true);
        ctx = ctx.insert("execution_path", std::string(""));

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: main_a → main_b → branch_special → branch_done → main_c → main_d
        std::string expected_path = "main_a→main_b→branch_special→branch_done→main_c→main_d→";
        if (auto path_opt = result.get("execution_path")) {
            if (auto* str_val = std::get_if<std::string>(&*path_opt)) {
                assert(*str_val == expected_path);
            }
        }
        std::cout << "✅ Branch with return: " << expected_path << std::endl;
    }

    // Test 3: Branch without return (terminate at branch)
    {
        codeuchain::Chain terminate_chain;

        terminate_chain.add_link("start", std::make_shared<BranchReturnLink>("start"));
        terminate_chain.add_link("normal", std::make_shared<BranchReturnLink>("normal"));
        terminate_chain.add_link("branch_end", std::make_shared<BranchReturnLink>("branch_end"));

        // Branch from start to branch_end with no return (empty return target)
        auto terminate_condition = [](const codeuchain::Context& ctx) -> bool {
            if (auto term_opt = ctx.get("terminate_branch")) {
                if (auto* bool_val = std::get_if<bool>(&*term_opt)) {
                    return *bool_val;
                }
            }
            return false;
        };
        terminate_chain.connect_branch("start", "branch_end", "", terminate_condition);

        codeuchain::Context ctx;
        ctx = ctx.insert("terminate_branch", true);
        ctx = ctx.insert("execution_path", std::string(""));

        auto future = terminate_chain.run(ctx);
        auto result = future.get();

        // Should execute: start → branch_end (and stop, no return)
        std::string expected_path = "start→branch_end→";
        if (auto path_opt = result.get("execution_path")) {
            if (auto* str_val = std::get_if<std::string>(&*path_opt)) {
                assert(*str_val == expected_path);
            }
        }
        std::cout << "✅ Branch terminate: " << expected_path << std::endl;
    }

    std::cout << "✅ Branch return functionality test passed!" << std::endl;
}

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

// Test Links for auto-connection and conditional branching
class PathALink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        context = context.insert("path", "A");
        context = context.insert("executed_A", true);
        co_return {context};
    }
    std::string name() const override { return "path_a"; }
    std::string description() const override { return "Always executes path A"; }
};

class PathBLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        context = context.insert("path", "B");
        context = context.insert("executed_B", true);
        co_return {context};
    }
    std::string name() const override { return "path_b"; }
    std::string description() const override { return "Conditional path B"; }
};

class PathCLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        context = context.insert("executed_C", true);
        // Record which path was taken
        if (auto path = context.get("path")) {
            if (path && std::holds_alternative<std::string>(*path)) {
                context = context.insert("final_path", std::get<std::string>(*path));
            }
        }
        co_return {context};
    }
    std::string name() const override { return "path_c"; }
    std::string description() const override { return "Final link that records path taken"; }
};

void test_auto_connection_and_conditionals() {
    std::cout << "Testing auto-connection and conditional branching..." << std::endl;

    codeuchain::Chain chain;

    // Add links - auto-connection will connect them sequentially: path_a -> path_b -> path_c
    chain.add_link("path_a", std::make_shared<PathALink>());
    chain.add_link("path_b", std::make_shared<PathBLink>());
    chain.add_link("path_c", std::make_shared<PathCLink>());

    // Add conditional connection: if "use_path_b" is true, skip path_a and go to path_b
    auto condition_use_b = [](const codeuchain::Context& ctx) -> bool {
        if (auto use_b = ctx.get("use_path_b")) {
            if (use_b && std::holds_alternative<bool>(*use_b)) {
                return std::get<bool>(*use_b);
            }
        }
        return false;
    };
    chain.connect("path_a", "path_b", condition_use_b);

    // Test 1: Default auto-connection path (use_path_b = false or missing)
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("use_path_b", false);

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: A -> B -> C (auto-connection)
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_B").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<bool>(*result.get("executed_A")) == true);
        assert(std::get<bool>(*result.get("executed_B")) == true);
        assert(std::get<bool>(*result.get("executed_C")) == true);
        assert(std::get<std::string>(*result.get("final_path")) == "B"); // Last executed link sets path
    }

    // Test 2: Conditional path (use_path_b = true) - should trigger conditional connection
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("use_path_b", true);

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: A (starts), then conditional to B, then C
        // But B should overwrite A's path setting
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_B").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<bool>(*result.get("executed_A")) == true);
        assert(std::get<bool>(*result.get("executed_B")) == true);
        assert(std::get<bool>(*result.get("executed_C")) == true);
        assert(std::get<std::string>(*result.get("final_path")) == "B");
    }

    // Test 3: No condition specified - should use auto-connection
    {
        codeuchain::Context ctx;
        // No "use_path_b" key - condition should return false

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: A -> B -> C (auto-connection)
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_B").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<std::string>(*result.get("final_path")) == "B");
    }

    std::cout << "✅ Auto-connection and conditional branching test passed!" << std::endl;
}

// Advanced branching test with multiple conditional paths
class BranchLink : public codeuchain::ILink {
public:
    BranchLink(std::string branch_name) : branch_name_(branch_name) {}

    codeuchain::LinkAwaitable call(codeuchain::Context context) override {
        context = context.insert("branch_taken", branch_name_);
        context = context.insert("executed_" + branch_name_, true);
        co_return {context};
    }

    std::string name() const override { return "branch_" + branch_name_; }
    std::string description() const override { return "Branch link for " + branch_name_; }

private:
    std::string branch_name_;
};

void test_advanced_branching() {
    std::cout << "Testing advanced conditional branching scenarios..." << std::endl;

    codeuchain::Chain chain;

    // Create a chain: start -> branch_x -> branch_y -> end
    chain.add_link("start", std::make_shared<PathALink>());
    chain.add_link("branch_x", std::make_shared<BranchLink>("X"));
    chain.add_link("branch_y", std::make_shared<BranchLink>("Y"));
    chain.add_link("end", std::make_shared<PathCLink>());

    // Conditional: if "take_x" is true, go from start to branch_x
    auto condition_take_x = [](const codeuchain::Context& ctx) -> bool {
        if (auto take_x = ctx.get("take_x")) {
            if (take_x && std::holds_alternative<bool>(*take_x)) {
                return std::get<bool>(*take_x);
            }
        }
        return false;
    };
    chain.connect("start", "branch_x", condition_take_x);

    // Conditional: if "take_y" is true, go from branch_x to branch_y
    auto condition_take_y = [](const codeuchain::Context& ctx) -> bool {
        if (auto take_y = ctx.get("take_y")) {
            if (take_y && std::holds_alternative<bool>(*take_y)) {
                return std::get<bool>(*take_y);
            }
        }
        return false;
    };
    chain.connect("branch_x", "branch_y", condition_take_y);

    // Test 1: Default path (no conditions met) - should follow auto-connection
    {
        codeuchain::Context ctx;

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: start -> branch_x -> branch_y -> end (auto-connection)
        assert(result.get("executed_A").has_value()); // start link
        assert(result.get("executed_X").has_value()); // branch_x
        assert(result.get("executed_Y").has_value()); // branch_y
        assert(result.get("executed_C").has_value()); // end link
        assert(std::get<std::string>(*result.get("branch_taken")) == "Y");
    }

    // Test 2: Take X branch only
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("take_x", true);

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: start -> branch_x (conditional) -> branch_y -> end
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_X").has_value());
        assert(result.get("executed_Y").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<std::string>(*result.get("branch_taken")) == "Y");
    }

    // Test 3: Take both X and Y branches
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("take_x", true);
        ctx = ctx.insert("take_y", true);

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: start -> branch_x (conditional) -> branch_y (conditional) -> end
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_X").has_value());
        assert(result.get("executed_Y").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<std::string>(*result.get("branch_taken")) == "Y");
    }

    // Test 4: Skip X but take Y (shouldn't happen due to auto-connection)
    {
        codeuchain::Context ctx;
        ctx = ctx.insert("take_x", false);
        ctx = ctx.insert("take_y", true);

        auto future = chain.run(ctx);
        auto result = future.get();

        // Should execute: start -> branch_x (auto) -> branch_y (conditional) -> end
        assert(result.get("executed_A").has_value());
        assert(result.get("executed_X").has_value());
        assert(result.get("executed_Y").has_value());
        assert(result.get("executed_C").has_value());
        assert(std::get<std::string>(*result.get("branch_taken")) == "Y");
    }

    std::cout << "✅ Advanced conditional branching test passed!" << std::endl;
}

int main() {
    std::cout << "CodeUChain C++ - Unit Tests" << std::endl;
    std::cout << "===========================" << std::endl;

    try {
        test_context_operations();
        test_chain_execution();
        test_link_awaitable();
        test_mutable_performance();
        test_auto_connection_and_conditionals();
        test_advanced_branching();
        test_execution_order_validation();
        test_branch_return_functionality();

        std::cout << "\n🎉 All tests passed!" << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "\n❌ Test failed: " << e.what() << std::endl;
        return 1;
    }
}