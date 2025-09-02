#pragma once

#include "context.hpp"
#include <memory>
#include <coroutine>
#include <future>

/*!
Link: The Loving Processing Unit

With agape harmony, the Link processes data through selfless transformation.
Core interface that all link implementations must follow.
*/

namespace codeuchain {

// Forward declaration for coroutine return type
struct LinkResult {
    Context context;
};

// Simplified coroutine implementation for better compatibility
class LinkAwaitable {
public:
    struct promise_type {
        LinkResult result;

        LinkAwaitable get_return_object() {
            return LinkAwaitable{std::coroutine_handle<promise_type>::from_promise(*this)};
        }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        void unhandled_exception() { std::terminate(); }
        void return_value(LinkResult value) { result = std::move(value); }
    };

    std::coroutine_handle<promise_type> handle;

    LinkResult get_result() {
        handle.resume();
        return std::move(handle.promise().result);
    }

    ~LinkAwaitable() {
        if (handle) handle.destroy();
    }
};

class ILink {
public:
    virtual ~ILink() = default;

    // Process the context and return transformed context
    virtual LinkAwaitable call(Context context) = 0;

    // Get link name for identification
    virtual std::string name() const = 0;

    // Get link description
    virtual std::string description() const = 0;
};

} // namespace codeuchain