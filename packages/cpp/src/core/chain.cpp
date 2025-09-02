#include "codeuchain/chain.hpp"
#include <iostream>
#include <thread>

namespace codeuchain {

Chain::Chain() = default;

void Chain::add_link(std::string name, std::shared_ptr<ILink> link) {
    links_.emplace(std::move(name), std::move(link));
}

void Chain::connect(std::string source, std::string target,
                   std::function<bool(const Context&)> condition) {
    connections_.emplace_back(std::move(source), std::move(target), std::move(condition));
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

        // Simple linear execution for now
        // TODO: Implement conditional flow execution based on connections
        for (const auto& [name, link] : links_) {
            // Execute middleware before each link
            for (const auto& mw : middlewares_) {
                auto handle = mw->before(link, ctx);
                if (handle) {
                    handle.resume();
                }
            }

            // Execute the link synchronously
            try {
                auto awaitable = link->call(std::move(ctx));
                auto result = awaitable.get_result();
                ctx = std::move(result.context);
            } catch (const std::exception& e) {
                // Handle error - could be enhanced with error middleware
                std::cerr << "Error executing link '" << name << "': " << e.what() << std::endl;
                break;
            }

            // Execute middleware after each link
            for (const auto& mw : middlewares_) {
                auto handle = mw->after(link, ctx);
                if (handle) {
                    handle.resume();
                }
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

const std::vector<std::shared_ptr<IMiddleware>>& Chain::middlewares() const {
    return middlewares_;
}

} // namespace codeuchain