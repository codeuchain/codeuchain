#pragma once

#include "context.hpp"
#include "link.hpp"
#include <memory>
#include <coroutine>

/*!
Middleware: The Loving Cross-Cutting Concern

With agape harmony, the Middleware provides selfless cross-cutting functionality.
Core interface that all middleware implementations must follow.
*/

namespace codeuchain {

class IMiddleware {
public:
    virtual ~IMiddleware() = default;

    // Execute before link processing
    virtual std::coroutine_handle<> before(std::shared_ptr<ILink> link, const Context& context) = 0;

    // Execute after link processing
    virtual std::coroutine_handle<> after(std::shared_ptr<ILink> link, const Context& context) = 0;

    // Get middleware name for identification
    virtual std::string name() const = 0;

    // Get middleware description
    virtual std::string description() const = 0;
};

} // namespace codeuchain