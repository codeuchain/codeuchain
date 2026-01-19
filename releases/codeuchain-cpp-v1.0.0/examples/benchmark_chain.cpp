// CodeUChain C++ Benchmark Harness
// --------------------------------
// Objective: Empirically measure computational cost of CodeUChain patterns
// versus baseline / manual equivalents to validate "zero / minimal overhead" claims.
//
// Benchmarks Included:
// 1. Immutable Context insert() vs std::unordered_map copy & insert
// 2. Mutable Context insert_mut()/update_mut() vs direct std::unordered_map mutation
// 3. TypedContext insert/get vs untyped Context insert/get
// 4. Type evolution insert_as() cost
// 5. Link dispatch (virtual) vs direct function call
// 6. Chain execution (N links) vs manual sequential functions
//
// Methodology:
// - High iteration counts (configurable) with warm-up phase
// - Use steady_clock for stable timing
// - Report: total time, per-op nanoseconds, relative overhead (%)
// - All benchmarks run in Release build for meaningful numbers
//
// Future Extensions (placeholders):
// - Allocation counting (custom allocator hook)
// - Cache effects / branch prediction (perf / VTune guidance)
// - Multi-thread scalability

#include <chrono>
#include <cstdint>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <cstring>
#include <variant>
#include <coroutine>

// Optional: global allocation tracking (compile with -DCODEUCHAIN_BENCH_TRACK_ALLOC)
#ifdef CODEUCHAIN_BENCH_TRACK_ALLOC
#include <atomic>
#include <cstdlib>
namespace {
    static std::atomic<size_t> g_alloc_calls{0};
    static std::atomic<size_t> g_dealloc_calls{0};
    static std::atomic<size_t> g_alloc_bytes{0};
}

void* operator new(std::size_t sz) {
    g_alloc_calls.fetch_add(1, std::memory_order_relaxed);
    g_alloc_bytes.fetch_add(sz, std::memory_order_relaxed);
    if (void* p = std::malloc(sz)) return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept {
    if (p) {
        g_dealloc_calls.fetch_add(1, std::memory_order_relaxed);
        std::free(p);
    }
}
void operator delete(void* p, std::size_t) noexcept { operator delete(p); }
#endif // CODEUCHAIN_BENCH_TRACK_ALLOC

#include "codeuchain/context.hpp"
#include "codeuchain/typed_context.hpp"
#include "codeuchain/link.hpp"
#include "codeuchain/chain.hpp"
#include "codeuchain/timing_middleware.hpp"

using Clock = std::chrono::steady_clock;
using ns = std::chrono::nanoseconds;

struct BenchmarkResult {
    std::string name;
    double total_ms{0.0};
    double per_op_ns{0.0};
    double relative_overhead_pct{0.0}; // vs control
    std::string note;                  // annotation (e.g. baseline too small)
};

struct ControlGroup {
    std::string label;
    double per_op_ns{0.0};
};

// Utility to format numbers with alignment
static void print_header(const std::string& title) {
    std::cout << "\n== " << title << " ==\n";
}

// Convert nanoseconds to a compact human readable string choosing the largest reasonable unit.
// Rules:
//  < 1,000 ns -> show e.g. 432 ns
//  < 1,000,000 ns -> show microseconds with 2 decimals (e.g. 12.34 µs)
//  < 1,000,000,000 ns -> show milliseconds with 2 decimals (e.g. 3.21 ms)
//  else seconds with 3 decimals
// Always append original ns in parentheses for precision.
static std::string human_time_from_ns(double ns_val) {
    std::ostringstream oss;
    if (ns_val < 1000.0) {
        oss << std::fixed << std::setprecision(0) << ns_val << " ns";
    } else if (ns_val < 1e6) { // microseconds
        oss << std::fixed << std::setprecision(2) << (ns_val / 1e3) << " µs";
    } else if (ns_val < 1e9) { // milliseconds
        oss << std::fixed << std::setprecision(2) << (ns_val / 1e6) << " ms";
    } else { // seconds
        oss << std::fixed << std::setprecision(3) << (ns_val / 1e9) << " s";
    }
    oss << " (" << std::fixed << std::setprecision(2) << ns_val << " ns)";
    return oss.str();
}

static void print_result(const BenchmarkResult& r) {
    std::cout << std::left << std::setw(38) << r.name
              << " total(ms): " << std::setw(10) << std::fixed << std::setprecision(3) << r.total_ms
              << " per-op: " << std::setw(24) << human_time_from_ns(r.per_op_ns)
              << " overhead(%): " << std::setw(8) << std::fixed << std::setprecision(2) << r.relative_overhead_pct;
    if (!r.note.empty()) std::cout << "  " << r.note;
    std::cout << "\n";
}

template <typename F>
double time_loop(std::size_t iterations, F&& fn) {
    auto start = Clock::now();
    for (std::size_t i = 0; i < iterations; ++i) {
        fn(i);
    }
    auto end = Clock::now();
    return std::chrono::duration_cast<ns>(end - start).count();
}

// Repeat a measurement and take median per-op ns for stability against jitter
template <typename F>
double median_per_op(std::size_t iterations, int repeats, F&& fn) {
    std::vector<double> samples; samples.reserve(repeats);
    for (int r = 0; r < repeats; ++r) {
        auto total_ns = time_loop(iterations, fn);
        samples.push_back(total_ns / static_cast<double>(iterations));
    }
    std::sort(samples.begin(), samples.end());
    return samples[samples.size()/2];
}

inline double compute_overhead(double base_per_op_ns, double variant_per_op_ns, std::string& note) {
    if (base_per_op_ns <= 1.0) { // ~ timer resolution noise territory
        note = "baseline<1ns; overhead suppressed";
        return 0.0;
    }
    return (variant_per_op_ns - base_per_op_ns) / base_per_op_ns * 100.0;
}

// Warm-up to stabilize CPU frequency & caches
template <typename F>
void warmup(std::size_t iterations, F&& fn) {
    for (std::size_t i = 0; i < iterations; ++i) fn(i);
}

// Simple function used in baseline pipeline comparisons
inline int double_fn(int v) { return v * 2; }
inline int add_ten_fn(int v) { return v + 10; }
inline int square_fn(int v) { return v * v; }

// ---- Linear Nested Evaluation (Compile-Time Structured) ----
// We construct a nested set of function calls equivalent in transformation
// to the chain (double -> add_ten -> square) but expressed as nested
// templates to show pure call overhead (no virtual, no context, fully inlinable).

// Attribute macro for optional noinline nested evaluation
#if defined(_MSC_VER)
#define CODEUCHAIN_NESTED_NOINLINE __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#define CODEUCHAIN_NESTED_NOINLINE __attribute__((noinline))
#else
#define CODEUCHAIN_NESTED_NOINLINE
#endif

template <int I>
inline int apply_op(int v) {
    if constexpr (I % 3 == 0) {
        return double_fn(v);
    } else if constexpr (I % 3 == 1) {
        return add_ten_fn(v);
    } else {
        return square_fn(v);
    }
}

template <int N>
inline int nested_eval(int v) {
    if constexpr (N == 0) {
        return v;
    } else {
        return apply_op<N-1>(nested_eval<N-1>(v));
    }
}

// "noinline" variant: every recursion level becomes a real call frame.
// This exposes a measurable lower bound closer to worst-case pipeline
// (no inlining) for contrast with the fully inlined version.
template <int N>
CODEUCHAIN_NESTED_NOINLINE int nested_eval_noinline(int v) {
    if constexpr (N == 0) {
        return v;
    } else {
        return apply_op<N-1>(nested_eval_noinline<N-1>(v));
    }
}

// Minimal link for chain benchmark
class DoubleLink : public codeuchain::ILink {
public:
    codeuchain::LinkAwaitable call(codeuchain::Context ctx) override {
        auto v = ctx.get("v");
        if (v && std::holds_alternative<int>(*v)) {
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x * 2);
        }
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
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x + 10);
        }
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
            int x = std::get<int>(*v);
            ctx = ctx.insert("v", x * x);
        }
        co_return codeuchain::LinkResult{ctx};
    }
    std::string name() const override { return "SquareLink"; }
    std::string description() const override { return "squares v"; }
};

// Direct nested function pipeline control (baseline for chain)
inline int direct_pipeline(int v) {
    // Equivalent transformation sequence: double -> add ten -> square
    v = double_fn(v);
    v = add_ten_fn(v);
    v = square_fn(v);
    return v;
}

// Synchronous chain runner (no async / futures) for stable benchmarking
// It uses the public links() accessor to iterate deterministically.
// NOTE: Order: unordered_map iteration order is unspecified; for stable
// comparison we build chain using vector of shared_ptr below instead.
struct SyncLinkWrapper {
    std::string name;
    std::shared_ptr<codeuchain::ILink> link;
};

inline codeuchain::Context run_chain_sync(std::vector<SyncLinkWrapper>& links, codeuchain::Context ctx) {
    for (auto& lw : links) {
        auto awaitable = lw.link->call(ctx); // pass by value copy of ctx
        auto result = awaitable.get_result();
        ctx = std::move(result.context);
    }
    return ctx;
}

int main(int argc, char** argv) {
    // ---- CLI Parsing ----
    std::size_t iterations = 20000;
    int repeats = 5; // median repeats
    bool mode_sync = true;
    bool mode_async = false; // opt-in
    bool scaling_section = true;
    std::size_t batch = 1; // operations per iteration (for amplifying tiny ops)
    enum class NestedMode { Inline, Noinline };
    NestedMode nested_mode = NestedMode::Inline; // default
    bool validate = false; // correctness validation
    bool timing_mw = false; // attach timing middleware to async chain runs

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--iters" && i + 1 < argc) {
            iterations = static_cast<std::size_t>(std::stoull(argv[++i]));
        } else if (arg == "--repeat" && i + 1 < argc) {
            repeats = std::stoi(argv[++i]);
        } else if (arg == "--mode" && i + 1 < argc) {
            std::string m = argv[++i];
            if (m == "sync") { mode_sync = true; mode_async = false; }
            else if (m == "async") { mode_sync = false; mode_async = true; }
            else if (m == "both") { mode_sync = true; mode_async = true; }
        } else if (arg == "--no-scale") {
            scaling_section = false;
        } else if (arg == "--batch" && i + 1 < argc) {
            batch = static_cast<std::size_t>(std::stoull(argv[++i]));
            if (batch == 0) batch = 1;
        } else if (arg == "--nested-mode" && i + 1 < argc) {
            std::string m = argv[++i];
            if (m == "inline") nested_mode = NestedMode::Inline;
            else if (m == "noinline") nested_mode = NestedMode::Noinline;
            else {
                std::cerr << "Unknown nested-mode '" << m << "' (expected inline|noinline)\n";
                return 1;
            }
        } else if (arg == "--validate") {
            validate = true;
        } else if (arg == "--timing-mw") {
            timing_mw = true;
        } else if (arg == "--help") {
            std::cout << "Usage: benchmark_chain [--iters N] [--repeat R] [--mode sync|async|both] [--batch B] [--no-scale] [--nested-mode inline|noinline] [--validate] [--timing-mw]\n";
            return 0;
        }
    }

    std::cout << "CodeUChain Benchmark\n";
    std::cout << " iterations        : " << iterations << "\n";
    std::cout << " median repeats    : " << repeats << "\n";
    std::cout << " mode              : " << (mode_sync && mode_async ? "both" : (mode_sync ? "sync" : "async")) << "\n";
    std::cout << " batch factor      : " << batch << " (each loop performs this many ops)\n";
    std::cout << " scaling section   : " << (scaling_section ? "on" : "off") << "\n";
    std::cout << " nested-mode       : " << (nested_mode == NestedMode::Inline ? "inline" : "noinline") << "\n";
    std::cout << " validation        : " << (validate ? "on" : "off") << "\n";
    std::cout << " timing middleware : " << (timing_mw ? "on" : "off") << "\n";
    std::cout << "Build: EXPECT RELEASE (-O2/-O3) for meaningful results\n";

    // -----------------------------
    // Optional correctness validation (low cost, before timers)
    // -----------------------------
    if (validate) {
        bool ok = true;
        auto check = [&](int input){
            int dp = direct_pipeline(input);
            int nested = 0;
            if (nested_mode == NestedMode::Inline) nested = nested_eval<3>(input);
            else nested = nested_eval_noinline<3>(input);
            if (dp != nested) {
                std::cerr << "Validation mismatch: direct_pipeline(" << input << ")=" << dp << " nested=" << nested << "\n";
                ok = false;
            }
        };
        for (int seed : {0,1,2,5,17,42}) check(seed);

        // Build sync chain for validation if enabled
        std::vector<SyncLinkWrapper> validate_chain_links;
        validate_chain_links.push_back({"double", std::make_shared<DoubleLink>()});
        validate_chain_links.push_back({"add_ten", std::make_shared<AddTenLink>()});
        validate_chain_links.push_back({"square", std::make_shared<SquareLink>()});
        if (mode_sync) {
            for (int seed : {0,3,7,11}) {
                codeuchain::Context ctx; ctx = ctx.insert("v", seed);
                auto out = run_chain_sync(validate_chain_links, ctx); auto v = out.get("v");
                if (!v) { std::cerr << "Chain sync validation: missing v\n"; ok = false; }
                else if (!std::holds_alternative<int>(*v)) { std::cerr << "Chain sync validation: wrong type\n"; ok = false; }
                else {
                    int expected = direct_pipeline(seed);
                    if (std::get<int>(*v) != expected) {
                        std::cerr << "Chain sync mismatch seed=" << seed << " expected=" << expected << " got=" << std::get<int>(*v) << "\n"; ok = false; }
                }
            }
        }
        if (mode_async) {
            codeuchain::Chain chain_obj;
            chain_obj.add_link("double", std::make_shared<DoubleLink>());
            chain_obj.add_link("add_ten", std::make_shared<AddTenLink>());
            chain_obj.add_link("square", std::make_shared<SquareLink>());
            auto always = [](const codeuchain::Context&) { return true; };
            chain_obj.connect("double", "add_ten", always);
            chain_obj.connect("add_ten", "square", always);
            for (int seed : {0,4,9,13}) {
                codeuchain::Context ctx; ctx = ctx.insert("v", seed);
                auto fut = chain_obj.run(ctx); auto out = fut.get(); auto v = out.get("v");
                if (!v) { std::cerr << "Chain async validation: missing v\n"; ok = false; }
                else if (!std::holds_alternative<int>(*v)) { std::cerr << "Chain async validation: wrong type\n"; ok = false; }
                else {
                    int expected = direct_pipeline(seed);
                    if (std::get<int>(*v) != expected) {
                        std::cerr << "Chain async mismatch seed=" << seed << " expected=" << expected << " got=" << std::get<int>(*v) << "\n"; ok = false; }
                }
            }
        }
        if (!ok) {
            std::cerr << "Validation FAILED\n";
            return 2;
        }
        std::cout << "Validation: OK (direct == nested == chain)\n";
    }

    // -----------------------------
    // 1. Immutable Context insert
    // -----------------------------
    print_header("Context Insert (Immutable)");
    warmup(1000, [&](auto i) {
        for (std::size_t b=0; b<batch; ++b) {
            codeuchain::Context ctx;
            ctx = ctx.insert("k", static_cast<int>(i + b));
        }
    });

    auto ctl_insert = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) {
            std::unordered_map<std::string,int> m; auto m2 = m; m2["k"] = static_cast<int>(i + b);
        }
    });
    auto fw_insert = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) { codeuchain::Context ctx; ctx = ctx.insert("k", static_cast<int>(i + b)); }
    });
    std::string note1; double overhead1 = compute_overhead(ctl_insert, fw_insert, note1);
    BenchmarkResult br1{"Context.insert() vs manual copy", fw_insert * iterations / 1e6, fw_insert, overhead1, note1};
    print_result(br1);

    // -----------------------------
    // 2. Mutable Context insert_mut/update_mut
    // -----------------------------
    print_header("Context Mutable Insert");
    warmup(1000, [&](auto i) { for (std::size_t b=0; b<batch; ++b) { codeuchain::Context ctx; ctx.insert_mut("k", static_cast<int>(i + b)); } });
    auto ctl_mut = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) { std::unordered_map<std::string,int> m; m["k"] = static_cast<int>(i + b); }
    });
    auto fw_mut = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) { codeuchain::Context ctx; ctx.insert_mut("k", static_cast<int>(i + b)); }
    });
    std::string note2; double overhead2 = compute_overhead(ctl_mut, fw_mut, note2);
    BenchmarkResult br2{"Context.insert_mut()", fw_mut * iterations / 1e6, fw_mut, overhead2, note2};
    print_result(br2);

    // -----------------------------
    // 3. TypedContext insert/get vs Context
    // -----------------------------
    print_header("Typed vs Untyped Context");
    warmup(1000, [&](auto i) {
        for (std::size_t b=0; b<batch; ++b) {
            auto tctx = codeuchain::make_typed_context<int>(codeuchain::Context{});
            auto t2 = tctx.insert("v", static_cast<int>(i + b));
            (void)t2.get_typed<int>("v");
        }
    });

    auto untyped = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) {
            codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i + b)); auto v = ctx.get("v"); if (v && !std::holds_alternative<int>(*v)) std::abort(); }
    });
    auto typed = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) { auto tctx = codeuchain::make_typed_context<int>(codeuchain::Context{}); auto t2 = tctx.insert("v", static_cast<int>(i + b)); auto v = t2.get_typed<int>("v"); if(!v) std::abort(); }
    });
    std::string note3; double overhead3 = compute_overhead(untyped, typed, note3);
    BenchmarkResult br3{"TypedContext insert/get", typed * iterations / 1e6, typed, overhead3, note3};
    print_result(br3);

    // -----------------------------
    // 4. Type Evolution insert_as
    // -----------------------------
    print_header("Type Evolution (insert_as)");
    warmup(1000, [&](auto i) {
        for (std::size_t b=0; b<batch; ++b) { auto tctx = codeuchain::make_typed_context<int>(codeuchain::Context{}); auto t2 = tctx.insert("v", static_cast<int>(i + b)); auto t3 = t2.insert_as<double>("d", static_cast<double>(i + b) * 1.5); (void)t3; }
    });
    auto evo_per_op = median_per_op(iterations, repeats, [&](auto i){
        for (std::size_t b=0; b<batch; ++b) { auto tctx = codeuchain::make_typed_context<int>(codeuchain::Context{}); auto t2 = tctx.insert("v", static_cast<int>(i + b)); auto t3 = t2.insert_as<double>("d", static_cast<double>(i + b) * 1.5); (void)t3; }
    });
    BenchmarkResult br4{"TypedContext insert_as()", evo_per_op * iterations / 1e6, evo_per_op, 0.0, ""};
    print_result(br4);

    // -----------------------------
    // 5. Chain dispatch vs direct nested functions (control)
    // -----------------------------
    print_header("Chain vs Direct Function Pipeline");

    // Prepare chain link objects (vector for deterministic order)
    std::vector<SyncLinkWrapper> chain_links;
    chain_links.push_back({"double", std::make_shared<DoubleLink>()});
    chain_links.push_back({"add_ten", std::make_shared<AddTenLink>()});
    chain_links.push_back({"square", std::make_shared<SquareLink>()});

    // Warmup
    warmup(200, [&](auto i){
        (void)direct_pipeline(static_cast<int>(i));
        codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i));
        auto out = run_chain_sync(chain_links, ctx); (void)out.get("v");
    });

    auto direct_ns = median_per_op(iterations, repeats, [&](auto i){
        int v = static_cast<int>(i);
        for (std::size_t b=0; b<batch; ++b) { v = direct_pipeline(v); }
        if (v < 0) std::abort();
    });
    std::string note_chain_sync;
    BenchmarkResult br_chain_sync{"Chain sync (3 links)", 0, 0, 0, ""};
    if (mode_sync) {
        auto chain_sync_ns = median_per_op(iterations, repeats, [&](auto i){
            for (std::size_t b=0; b<batch; ++b) { codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i + b)); auto out = run_chain_sync(chain_links, ctx); auto v = out.get("v"); if(!v) std::abort(); }
        });
        double overhead_sync = compute_overhead(direct_ns, chain_sync_ns, note_chain_sync);
        br_chain_sync = {"Chain sync (3 links)", chain_sync_ns * iterations / 1e6, chain_sync_ns, overhead_sync, note_chain_sync};
        print_result(br_chain_sync);
    }

    // Async mode (experimental) using Chain::run
    if (mode_async) {
        // Build a Chain instance with deterministic connections
        codeuchain::Chain chain_obj;
        chain_obj.add_link("double", std::make_shared<DoubleLink>());
        chain_obj.add_link("add_ten", std::make_shared<AddTenLink>());
        chain_obj.add_link("square", std::make_shared<SquareLink>());
        std::shared_ptr<codeuchain::TimingMiddleware> timing;
        if (timing_mw) {
            // per_invocation=true to collect each call; auto_print deferred so we control placement
            timing = std::make_shared<codeuchain::TimingMiddleware>(true, false);
            chain_obj.use_middleware(timing);
        }
        // Connect sequentially (always true conditions)
        auto always = [](const codeuchain::Context&) { return true; };
        chain_obj.connect("double", "add_ten", always);
        chain_obj.connect("add_ten", "square", always);

        // Warmup async path
        warmup(50, [&](auto i){
            codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i));
            auto fut = chain_obj.run(ctx); auto out = fut.get(); (void)out.get("v");
        });

        auto chain_async_ns = median_per_op(iterations, repeats, [&](auto i){
            for (std::size_t b=0; b<batch; ++b) {
                codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i + b));
                auto fut = chain_obj.run(ctx);
                auto out = fut.get(); auto v = out.get("v"); if(!v) std::abort();
            }
        });
        std::string note_chain_async; double overhead_async = compute_overhead(direct_ns, chain_async_ns, note_chain_async);
        BenchmarkResult br_chain_async{"Chain async (3 links)", chain_async_ns * iterations / 1e6, chain_async_ns, overhead_async, note_chain_async};
        print_result(br_chain_async);
        if (timing_mw) {
            timing->report(std::cout);
        }
    }

    // -----------------------------
    // 6b. Linear Nested Evaluation (same logical steps)
    // -----------------------------
    print_header("Linear Nested Evaluation (Direct Calls)");
    // Warmup nested path (3 steps to mirror 3-link chain)
    if (nested_mode == NestedMode::Inline) {
        warmup(200, [&](auto i){ (void)nested_eval<3>(static_cast<int>(i)); });
    } else {
        warmup(200, [&](auto i){ (void)nested_eval_noinline<3>(static_cast<int>(i)); });
    }
    double nested3_ns = 0.0;
    if (nested_mode == NestedMode::Inline) {
        nested3_ns = median_per_op(iterations, repeats, [&](auto i){
            int v = static_cast<int>(i);
            for (std::size_t b=0; b<batch; ++b) { v = nested_eval<3>(v); }
            if (v < 0) std::abort();
        });
    } else { // noinline
        nested3_ns = median_per_op(iterations, repeats, [&](auto i){
            int v = static_cast<int>(i);
            for (std::size_t b=0; b<batch; ++b) { v = nested_eval_noinline<3>(v); }
            if (v < 0) std::abort();
        });
    }
    std::string note_nested3; double overhead_nested3 = compute_overhead(direct_ns, nested3_ns, note_nested3);
    BenchmarkResult br_nested3{nested_mode == NestedMode::Inline ? "Nested eval (3 levels, inline)" : "Nested eval (3 levels, noinline)", nested3_ns * iterations / 1e6, nested3_ns, overhead_nested3, note_nested3};
    print_result(br_nested3);

    // Compare directly with sync chain if present
    if (mode_sync) {
        std::string note_cmp_nested_chain; double overhead_nested_chain = 0.0;
        if (br_chain_sync.per_op_ns > 0) {
            // Overhead of chain vs nested pure calls
            overhead_nested_chain = compute_overhead(nested3_ns, br_chain_sync.per_op_ns, note_cmp_nested_chain);
        }
        BenchmarkResult br_chain_vs_nested{"Chain sync vs nested (Δ%)", 0.0, br_chain_sync.per_op_ns, overhead_nested_chain, note_cmp_nested_chain};
        print_result(br_chain_vs_nested);
    }

    // Scaling for nested evaluation analogous to chain scaling
    if (scaling_section) {
        print_header("Nested Eval Scaling");
        std::vector<int> counts{1,2,4,8};
    volatile int sink_guard = 1; // prevents compiler from discarding nested results
    for (int n : counts) {
            // Use a lambda that switches on n to call the right instantiation.
            auto per_ns = median_per_op(std::max<std::size_t>(50, iterations/10), std::max(1, repeats/2), [&](auto i){
                int v = static_cast<int>(i) + 2; // shift upward
                int out;
                if (nested_mode == NestedMode::Inline) {
                    if (n == 1) out = nested_eval<1>(v);
                    else if (n == 2) out = nested_eval<2>(v);
                    else if (n == 4) out = nested_eval<4>(v);
                    else /* n == 8 */ out = nested_eval<8>(v);
                } else {
                    if (n == 1) out = nested_eval_noinline<1>(v);
                    else if (n == 2) out = nested_eval_noinline<2>(v);
                    else if (n == 4) out = nested_eval_noinline<4>(v);
                    else /* n == 8 */ out = nested_eval_noinline<8>(v);
                }
                sink_guard ^= out; // side effect to retain work
            });
            BenchmarkResult br_nested_scale{std::string("Nested eval length ") + std::to_string(n) + (nested_mode == NestedMode::Inline ? " (inline)" : " (noinline)"), per_ns * iterations / 1e6, per_ns, 0.0, ""};
            print_result(br_nested_scale);
        }
    }

    // Extended scaling (1,2,4,8 links) using doubled sequence pattern
    if (scaling_section && mode_sync) {
        print_header("Chain Scaling (Sync Run)");
        std::vector<int> counts{1,2,4,8};
        for (int n : counts) {
            std::vector<SyncLinkWrapper> links_scaled; links_scaled.reserve(n);
            for (int k = 0; k < n; ++k) {
                switch (k % 3) {
                    case 0: links_scaled.push_back({"double", std::make_shared<DoubleLink>()}); break;
                    case 1: links_scaled.push_back({"add_ten", std::make_shared<AddTenLink>()}); break;
                    default: links_scaled.push_back({"square", std::make_shared<SquareLink>()}); break;
                }
            }
            std::size_t iters = std::max<std::size_t>(50, iterations / 10);
            auto chain_len_ns = median_per_op(iters, std::max(1, repeats/2), [&](auto i){
                codeuchain::Context ctx; ctx = ctx.insert("v", static_cast<int>(i) + 1);
                auto out = run_chain_sync(links_scaled, ctx); if(!out.get("v")) std::abort();
            });
            BenchmarkResult br_scale{"Chain sync length " + std::to_string(n), chain_len_ns * iters / 1e6, chain_len_ns, 0.0, ""};
            print_result(br_scale);
        }
    }

    // (Temporarily disabled chain scaling section while investigating segfault in dispatch)
    // print_header("Chain Scaling (build + run)");

    std::cout << "\nNOTE: Overhead suppressed when baseline < ~1ns (timer resolution).\n";
#ifdef CODEUCHAIN_BENCH_TRACK_ALLOC
    std::cout << "Allocation stats (global new/delete overrides active)\n  alloc calls   : " << g_alloc_calls.load() << "\n  dealloc calls : " << g_dealloc_calls.load() << "\n  alloc bytes   : " << g_alloc_bytes.load() << "\n";
#else
    std::cout << "(Rebuild with -DCODEUCHAIN_BENCH_TRACK_ALLOC for allocation stats)\n";
#endif
    std::cout << "Re-run examples:\n  ./examples/benchmark_chain --iters 100000 --repeat 7 --mode both\n  ./examples/benchmark_chain --iters 50000 --batch 4\n";
    std::cout << "Segments: context ops, typed ops, evolution, chain vs direct (sync/async), nested eval, scaling.";
    return 0;
}
