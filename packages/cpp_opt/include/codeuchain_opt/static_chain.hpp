#pragma once

#include <tuple>
#include <utility>
#include <type_traits>
#include <cstddef>
#include "codeuchain/context.hpp"

namespace codeuchain_opt {

// Concept: Operation with signature Context op(Context)
template<class T>
concept ContextOp = requires(T t, ::codeuchain::Context ctx) {
    { t(ctx) } -> std::same_as<::codeuchain::Context>; 
};

// Helper to apply one op
template<ContextOp Op>
inline ::codeuchain::Context apply_one(Op& op, ::codeuchain::Context ctx){
    return op(ctx);
}

template<class... Ops>
class StaticChain {
public:
    StaticChain() = default;
    explicit StaticChain(Ops... ops): ops_(std::move(ops)...){ }

    ::codeuchain::Context run(::codeuchain::Context ctx) const {
        // Unroll over tuple
        std::apply([&](auto const&... op){ ((ctx = op(ctx)), ...); }, ops_);
        return ctx;
    }

private:
    std::tuple<Ops...> ops_{};
};

// Example tiny stateless ops mirroring benchmark arithmetic chain
struct DoubleOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x * 2);
        }
        return ctx;
    }
};

struct AddTenOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x + 10);
        }
        return ctx;
    }
};

struct SquareOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x * x);
        }
        return ctx;
    }
};

// Mutating variants (in-place) to measure impact of avoiding map copy
struct MutDoubleOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx.insert_mut("v", x * 2);
        }
        return ctx;
    }
};
struct MutAddTenOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx.insert_mut("v", x + 10);
        }
        return ctx;
    }
};
struct MutSquareOp {
    ::codeuchain::Context operator()(::codeuchain::Context ctx) const {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx.insert_mut("v", x * x);
        }
        return ctx;
    }
};

} // namespace codeuchain_opt
