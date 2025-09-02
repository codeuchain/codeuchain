#pragma once

#include "context.hpp"
#include "link.hpp"
#include "middleware.hpp"
#include <memory>
#include <unordered_map>
#include <vector>
#include <functional>
#include <coroutine>
#include <future>

/*!
Chain: The Loving Connector

With agape harmony, the Chain orchestrates link execution with conditional flows and middleware.
Core implementation that all chain implementations can build upon.
*/

namespace codeuchain {

class Chain {
public:
    // Create a new empty chain
    Chain();

    // Add a link to the chain
    void add_link(std::string name, std::shared_ptr<ILink> link);

    // Connect links with conditions
    void connect(std::string source, std::string target,
                 std::function<bool(const Context&)> condition);

    // Add middleware to the chain
    void use_middleware(std::shared_ptr<IMiddleware> middleware);

    // Execute the chain with initial context
    std::future<Context> run(Context initial_context);

    // Get links (for testing/debugging)
    const std::unordered_map<std::string, std::shared_ptr<ILink>>& links() const;

    // Get connections (for testing/debugging)
    const std::vector<std::tuple<std::string, std::string, std::function<bool(const Context&)>>>& connections() const;

    // Get middlewares (for testing/debugging)
    const std::vector<std::shared_ptr<IMiddleware>>& middlewares() const;

private:
    std::unordered_map<std::string, std::shared_ptr<ILink>> links_;
    std::vector<std::tuple<std::string, std::string, std::function<bool(const Context&)>>> connections_;
    std::vector<std::shared_ptr<IMiddleware>> middlewares_;
};

} // namespace codeuchain