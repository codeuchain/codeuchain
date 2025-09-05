#include "codeuchain/chain.hpp"
#include <iostream>
#include <thread>
#include <unordered_set>

namespace codeuchain {

Chain::Chain() = default;

void Chain::add_link(std::string name, std::shared_ptr<ILink> link) {
    links_.emplace(name, link);
    link_order_.push_back(std::move(name));

    // Auto-connect to previous link if it exists
    if (link_order_.size() > 1) {
        const auto& prev_name = link_order_[link_order_.size() - 2];
        const auto& current_name = link_order_.back();
        // Connect with always-true condition for sequential execution
        connections_.emplace_back(prev_name, current_name,
            [](const Context&) { return true; });
    }
}

void Chain::connect(std::string source, std::string target,
                   std::function<bool(const Context&)> condition) {
    connections_.emplace_back(std::move(source), std::move(target), std::move(condition));
}

void Chain::connect_branch(std::string source, std::string branch_target, 
                          std::string return_target,
                          std::function<bool(const Context&)> condition) {
    // Store branch connections separately with return target
    branch_connections_.emplace_back(std::move(source), std::move(branch_target), 
                                   std::move(return_target), std::move(condition));
}

void Chain::use_middleware(std::shared_ptr<IMiddleware> middleware) {
    middlewares_.emplace_back(std::move(middleware));
}

std::future<Context> Chain::run(Context initial_context) {
    return std::async(std::launch::async, [this, initial_context = std::move(initial_context)]() mutable {
        Context ctx = std::move(initial_context);

        // Execute middleware before hooks
        for (const auto& mw : middlewares_) {
            auto handle = mw->before(nullptr, ctx);
            if (handle) {
                handle.resume();
            }
        }

        // Execute links in the order they were added, but check for conditional connections
        std::unordered_set<std::string> executed_links;
        size_t current_index = 0;
        bool on_branch = false; // Track if we're currently on a branch
        std::string branch_return_target; // Where to return after branch completes

        while (current_index < link_order_.size()) {
            const auto& link_name = link_order_[current_index];
            auto link_it = links_.find(link_name);
            if (link_it == links_.end()) {
                ++current_index;
                continue;
            }

            // Check if any conditional connection should redirect execution
            bool should_execute_current = true;
            std::string next_link = (current_index + 1 < link_order_.size()) ? link_order_[current_index + 1] : "";

            // First check regular connections
            for (const auto& [source, target, condition] : connections_) {
                if (source == link_name && condition(ctx)) {
                    // Conditional connection triggered - redirect to target
                    next_link = target;
                    break;
                }
            }

            // Then check branch connections
            for (const auto& [source, branch_target, return_target, condition] : branch_connections_) {
                if (source == link_name && condition(ctx)) {
                    // Branch connection triggered - go to branch target
                    next_link = branch_target;
                    on_branch = true;
                    branch_return_target = return_target;
                    break;
                }
            }

            if (should_execute_current && executed_links.find(link_name) == executed_links.end()) {
                const auto& link = link_it->second;

                // Execute middleware before each link
                for (const auto& mw : middlewares_) {
                    auto handle = mw->before(link, ctx);
                    if (handle) {
                        handle.resume();
                    }
                }

                // Execute the link synchronously
                try {
                    // Call link and obtain awaitable
                    auto awaitable = link->call(ctx);
                    // Retrieve result (ensures single resume)
                    auto result = awaitable.get_result();
                    ctx = std::move(result.context);
                } catch (const std::exception& e) {
                    // Handle error - could be enhanced with error middleware
                    std::cerr << "Error executing link '" << link_name << "': " << e.what() << std::endl;
                    break;
                }

                // Execute middleware after each link
                for (const auto& mw : middlewares_) {
                    auto handle = mw->after(link, ctx);
                    if (handle) {
                        handle.resume();
                    }
                }

                executed_links.insert(link_name);

                // If we just executed a branch target, return to main path
                if (on_branch && link_name == next_link && !branch_return_target.empty()) {
                    next_link = branch_return_target;
                    on_branch = false;
                    branch_return_target.clear();
                }
            }

            // Move to next link (either sequential or conditional target)
            if (!next_link.empty()) {
                // Find the index of the next link
                for (size_t i = 0; i < link_order_.size(); ++i) {
                    if (link_order_[i] == next_link) {
                        current_index = i;
                        break;
                    }
                }
            } else {
                ++current_index;
            }
        }

        // Execute final middleware after hooks
        for (const auto& mw : middlewares_) {
            auto handle = mw->after(nullptr, ctx);
            if (handle) {
                handle.resume();
            }
        }

        return ctx;
    });
}

const std::unordered_map<std::string, std::shared_ptr<ILink>>& Chain::links() const {
    return links_;
}

const std::vector<std::tuple<std::string, std::string, std::function<bool(const Context&)>>>& Chain::connections() const {
    return connections_;
}

const std::vector<std::tuple<std::string, std::string, std::string, std::function<bool(const Context&)>>>& Chain::branch_connections() const {
    return branch_connections_;
}

const std::vector<std::shared_ptr<IMiddleware>>& Chain::middlewares() const {
    return middlewares_;
}

} // namespace codeuchain