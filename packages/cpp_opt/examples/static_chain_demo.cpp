#include <iostream>
#include <chrono>
#include <memory>
#include <vector>
#include <variant>
#include <sstream>
#include <iomanip>
#include "codeuchain/context.hpp"
#include "codeuchain/link.hpp"
#include "codeuchain/chain.hpp"
#include "codeuchain_opt/static_chain.hpp"

using Clock = std::chrono::steady_clock;

// Dynamic links reused from core style
class DoubleLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context ctx) override {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v); ctx = ctx.insert("v", x * 2); }
        co_return codeuchain::LinkResult{ctx};
    }
    std::string name() const override { return "DoubleLink"; }
    std::string description() const override { return "doubles v"; }
};
class AddTenLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context ctx) override {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v); ctx = ctx.insert("v", x + 10); }
        co_return codeuchain::LinkResult{ctx};
    }
    std::string name() const override { return "AddTenLink"; }
    std::string description() const override { return "adds 10 to v"; }
};
class SquareLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context ctx) override {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v); ctx = ctx.insert("v", x * x); }
        co_return codeuchain::LinkResult{ctx};
    }
    std::string name() const override { return "SquareLink"; }
    std::string description() const override { return "squares v"; }
};

// Helper: synchronous run over vector of dynamic links (copied from bench concept)
struct SyncLinkWrapper { std::shared_ptr<codeuchain::ILink> link; };
static codeuchain::Context run_chain_sync(std::vector<SyncLinkWrapper>& links, codeuchain::Context ctx) {
    for (auto& w : links) {
        auto aw = w.link->call(ctx); auto r = aw.get_result(); ctx = std::move(r.context);
    }
    return ctx;
}

int main() {
    constexpr int iterations = 20000;

    // Direct lambda pipeline (baseline)
    auto direct = [](int v){ v = v*2; v = v+10; v = v*v; return v; };

    // StaticChain (immutable ops)
    codeuchain_opt::StaticChain<codeuchain_opt::DoubleOp, codeuchain_opt::AddTenOp, codeuchain_opt::SquareOp> static_chain;
    // StaticChain (mutating ops)
    codeuchain_opt::StaticChain<codeuchain_opt::MutDoubleOp, codeuchain_opt::MutAddTenOp, codeuchain_opt::MutSquareOp> static_chain_mut;

    // Dynamic chain setup
    std::vector<SyncLinkWrapper> dyn; dyn.push_back({std::make_shared<DoubleLink>()});
    dyn.push_back({std::make_shared<AddTenLink>()});
    dyn.push_back({std::make_shared<SquareLink>()});

    // Baseline measurement helper
    auto measure = [&](auto&& fn){
        auto start = Clock::now();
        for (int i=0;i<iterations;++i) fn(i);
        auto end = Clock::now();
        return std::chrono::duration_cast<std::chrono::nanoseconds>(end-start).count()/double(iterations);
    };

    double direct_ns = measure([&](int i){ volatile int out = direct(i); (void)out; });
    double static_ns = measure([&](int i){ codeuchain::Context ctx; ctx = ctx.insert("v", i); auto out = static_chain.run(ctx); auto v = out.get("v"); if(!v) std::abort(); });
    double static_mut_ns = measure([&](int i){ codeuchain::Context ctx; ctx.insert_mut("v", i); auto out = static_chain_mut.run(ctx); auto v = out.get("v"); if(!v) std::abort(); });
    double dynamic_ns = measure([&](int i){ codeuchain::Context ctx; ctx = ctx.insert("v", i); auto out = run_chain_sync(dyn, ctx); auto v = out.get("v"); if(!v) std::abort(); });

    // Mutable in-place context sequence (no chain abstraction, same logical ops)
    double mutable_ctx_ns = measure([&](int i){
        codeuchain::Context ctx; // empty
        ctx.insert_mut("v", i); // initial
        {
            auto v = ctx.get("v"); if(!v || !std::holds_alternative<int>(*v)) std::abort();
            int x = std::get<int>(*v); ctx.insert_mut("v", x * 2);
        }
        {
            auto v = ctx.get("v"); if(!v || !std::holds_alternative<int>(*v)) std::abort();
            int x = std::get<int>(*v); ctx.insert_mut("v", x + 10);
        }
        {
            auto v = ctx.get("v"); if(!v || !std::holds_alternative<int>(*v)) std::abort();
            int x = std::get<int>(*v); ctx.insert_mut("v", x * x);
        }
        volatile auto final_v = ctx.get("v"); (void)final_v;
    });

    // Hot key slot (immutable): manually thread the int value without repeated lookups; write back only at end
    double hot_slot_imm_ns = measure([&](int i){
        // Simulate immutable semantics by building new contexts but skipping hash lookups inside arithmetic
        codeuchain::Context base; // empty
        // Insert initial (immutable)
        auto c1 = base.insert("v", i);
        // Instead of reading via get each time, keep a local copy
        int v = i;
        v = v * 2;
        v = v + 10;
        v = v * v;
        // Final write emulates result context after sequence
        auto c2 = c1.insert("v", v); // last insert cost only measured once here
        volatile auto check = c2.get("v"); (void)check;
    });

    // Hot key slot (mutable): single insert_mut then mutate cached value only; one final store
    double hot_slot_mut_ns = measure([&](int i){
        codeuchain::Context ctx; ctx.insert_mut("v", i);
        int v = i;
        v = v * 2;
        v = v + 10;
        v = v * v;
        ctx.insert_mut("v", v); // final store
        volatile auto check = ctx.get("v"); (void)check;
    });

    auto fmt = [](double ns){
        std::ostringstream oss; 
        if (ns < 1000.0) oss << std::fixed << std::setprecision(0) << ns << " ns";
        else if (ns < 1e6) oss << std::fixed << std::setprecision(2) << (ns/1e3) << " µs";
        else if (ns < 1e9) oss << std::fixed << std::setprecision(2) << (ns/1e6) << " ms";
        else oss << std::fixed << std::setprecision(3) << (ns/1e9) << " s";
        oss << " (" << std::fixed << std::setprecision(2) << ns << " ns)";
        return oss.str();
    };
    std::cout << "StaticChain Demo (per-op)\n";
    std::cout << " direct        : " << fmt(direct_ns) << "\n";
    std::cout << " static        : " << fmt(static_ns) << " (immutable)\n";
    std::cout << " static_mut    : " << fmt(static_mut_ns) << " (mutable)\n";
    std::cout << " dynamic       : " << fmt(dynamic_ns) << "\n";
    std::cout << " mutable       : " << fmt(mutable_ctx_ns) << "\n";
    std::cout << " hot_slot_imm  : " << fmt(hot_slot_imm_ns) << " (immutable cached)\n";
    std::cout << " hot_slot_mut  : " << fmt(hot_slot_mut_ns) << " (mutable cached)\n";
    std::cout << " overhead static vs direct    : " << ((static_ns - direct_ns)/direct_ns*100.0) << "%\n";
    std::cout << " overhead static_mut vs direct : " << ((static_mut_ns - direct_ns)/direct_ns*100.0) << "%\n";
    std::cout << " overhead dynamic vs static    : " << ((dynamic_ns - static_ns)/static_ns*100.0) << "%\n";
    std::cout << " overhead static vs static_mut  : " << ((static_ns - static_mut_ns)/static_mut_ns*100.0) << "%\n";
    std::cout << " overhead mutable vs direct  : " << ((mutable_ctx_ns - direct_ns)/direct_ns*100.0) << "%\n";
    std::cout << " overhead static vs mutable  : " << ((static_ns - mutable_ctx_ns)/mutable_ctx_ns*100.0) << "%\n";
    std::cout << " overhead dynamic vs mutable : " << ((dynamic_ns - mutable_ctx_ns)/mutable_ctx_ns*100.0) << "%\n";
    std::cout << " overhead hot_slot_imm vs static    : " << ((hot_slot_imm_ns - static_ns)/static_ns*100.0) << "%\n";
    std::cout << " overhead hot_slot_mut vs static_mut : " << ((hot_slot_mut_ns - static_mut_ns)/static_mut_ns*100.0) << "%\n";
    std::cout << " overhead hot_slot_mut vs mutable    : " << ((hot_slot_mut_ns - mutable_ctx_ns)/mutable_ctx_ns*100.0) << "%\n";
    return 0;
}
