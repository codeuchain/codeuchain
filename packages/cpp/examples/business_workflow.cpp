// Business Workflow Example using CodeUChain
// ------------------------------------------
// Simulated order processing pipeline demonstrating the timing middleware
// and realistic context evolution without external systems.
//
// Purpose: Illustrates a multi-stage business workflow where each link
// performs meaningful work (validation, enrichment, calculation, persistence)
// and mutates the context. Uses TimingMiddleware to measure per-link
// performance, showing how real-world chains can be profiled.
//
// Stages:
//   1. ValidateInput      - checks that required fields exist
//   2. EnrichCustomer     - simulates lookup & enrichment (adds loyalty tier)
//   3. PriceCalculation   - computes line totals & subtotal
//   4. ApplyDiscounts     - applies simple rule-based discounts
//   5. PersistOrder       - simulates persistence (adds order_id & timestamps)
//   6. PublishEvent       - simulates outbound event publish
//
// Each stage mutates/extends context, giving us a realistic chain for the
// TimingMiddleware to measure. No real I/O: simulated delays via lightweight
// computations to avoid sleeping (sleep would dominate noise & wall clock).
//
// Build: part of examples (see CMake). Run:
//   ./examples/business_workflow --runs 3 --per-invocation
//
// Sample output includes timing report and final context keys.

#include <iostream>
#include <random>
#include <chrono>
#include <thread>
#include <sstream>
#include <iomanip>
#include <memory>
#include <vector>
#include <unordered_map>

#include "codeuchain/chain.hpp"
#include "codeuchain/link.hpp"
#include "codeuchain/context.hpp"
#include "codeuchain/timing_middleware.hpp"

using namespace codeuchain;

// Utility: pseudo-random small workload (hash scramble) to simulate CPU effort
static void cpu_burn(int iters, uint64_t seed_base = 0) {
    uint64_t x = 0x9e3779b97f4a7c15ULL ^ seed_base;
    for (int i = 0; i < iters; ++i) {
        x ^= (x << 7);
        x ^= (x >> 9);
        x *= 0x165667919E3779F9ULL;
    }
    if ((x & 0xff) == 0x42) { asm volatile(""); } // prevent over-optimization
}

class ValidateInputLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        auto customer = ctx.get("customer_id");
        auto items = ctx.get("items");
        bool ok = customer.has_value() && items.has_value();
        ctx = ctx.insert("valid", ok);
        cpu_burn(1200, 1);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "ValidateInput"; }
    std::string description() const override { return "Validates base required fields"; }
};

class EnrichCustomerLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        auto valid = ctx.get("valid");
        if (valid && std::holds_alternative<bool>(*valid) && std::get<bool>(*valid)) {
            // Simulate enrichment (tier based on hash of customer)
            std::string tier = "bronze";
            if (auto cid = ctx.get("customer_id")) {
                if (cid && std::holds_alternative<int>(*cid)) {
                    int v = std::get<int>(*cid);
                    tier = (v % 10 < 2) ? "platinum" : (v % 10 < 5 ? "gold" : "silver");
                }
            }
            ctx = ctx.insert("loyalty_tier", tier);
        }
        cpu_burn(2000, 2);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "EnrichCustomer"; }
    std::string description() const override { return "Adds loyalty tier based on customer id"; }
};

class PriceCalculationLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        // Items represented as vector<string> of numeric price tokens for simplicity
        double subtotal = 0.0;
        if (auto items = ctx.get("items")) {
            if (items && std::holds_alternative<std::vector<std::string>>(*items)) {
                for (const auto& s : std::get<std::vector<std::string>>(*items)) {
                    try { subtotal += std::stod(s); } catch(...) {}
                }
            }
        }
        ctx = ctx.insert("subtotal", subtotal);
        cpu_burn(2500, 3);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "PriceCalculation"; }
    std::string description() const override { return "Sums item prices"; }
};

class ApplyDiscountsLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        double subtotal = 0.0;
        if (auto st = ctx.get("subtotal")) {
            if (st && std::holds_alternative<double>(*st)) subtotal = std::get<double>(*st);
        }
        double discount = 0.0;
        if (auto tier = ctx.get("loyalty_tier")) {
            if (tier && std::holds_alternative<std::string>(*tier)) {
                const auto& t = std::get<std::string>(*tier);
                if (t == "platinum") discount = 0.15;
                else if (t == "gold") discount = 0.10;
                else if (t == "silver") discount = 0.05;
            }
        }
        double total = subtotal * (1.0 - discount);
        ctx = ctx.insert("discount_rate", discount);
        ctx = ctx.insert("total", total);
        cpu_burn(1800, 4);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "ApplyDiscounts"; }
    std::string description() const override { return "Applies loyalty discount"; }
};

class PersistOrderLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        // Simulate persistence cost with extra cpu burn and ID generation
        static std::atomic<uint64_t> next_id{1000};
        uint64_t oid = next_id.fetch_add(1, std::memory_order_relaxed);
        ctx = ctx.insert("order_id", static_cast<int>(oid));
        ctx = ctx.insert("persisted", true);
        cpu_burn(3200, 5);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "PersistOrder"; }
    std::string description() const override { return "Simulates database persistence"; }
};

class PublishEventLink : public ILink {
public:
    LinkAwaitable call(Context ctx) override {
        // Simulate event serialization hashing workload
        cpu_burn(2100, 6);
        ctx = ctx.insert("event_published", true);
        co_return LinkResult{ctx};
    }
    std::string name() const override { return "PublishEvent"; }
    std::string description() const override { return "Simulates outbound event"; }
};

int main(int argc, char** argv) {
    int runs = 1;
    bool per_invocation = false;
    codeuchain::TimingMiddleware::FormatConfig config;

    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        if (a == "--runs" && i + 1 < argc) runs = std::stoi(argv[++i]);
        else if (a == "--per-invocation") per_invocation = true;
        else if (a == "--format" && i + 1 < argc) {
            std::string fmt = argv[++i];
            if (fmt == "csv") config.format = codeuchain::TimingMiddleware::OutputFormat::CSV;
            else if (fmt == "tabular") config.format = codeuchain::TimingMiddleware::OutputFormat::Tabular;
        }
        else if (a == "--unit" && i + 1 < argc) {
            std::string unit = argv[++i];
            if (unit == "ns") config.time_unit = codeuchain::TimingMiddleware::TimeUnit::Nano;
            else if (unit == "us" || unit == "µs") config.time_unit = codeuchain::TimingMiddleware::TimeUnit::Micro;
            else if (unit == "ms") config.time_unit = codeuchain::TimingMiddleware::TimeUnit::Milli;
            else if (unit == "auto") config.time_unit = codeuchain::TimingMiddleware::TimeUnit::Auto;
        }
        else if (a == "--decimals" && i + 1 < argc) {
            config.decimal_places = std::stoi(argv[++i]);
        }
        else if (a == "--no-raw-ns") {
            config.show_raw_ns = false;
        }
        else if (a == "--no-calls") {
            config.show_calls = false;
        }
        else if (a == "--no-avg") {
            config.show_avg = false;
        }
        else if (a == "--no-total") {
            config.show_total = false;
        }
        else if (a == "--help") {
            std::cout << "Usage: business_workflow [options]\n";
            std::cout << "  --runs N              Number of workflow runs (default: 1)\n";
            std::cout << "  --per-invocation      Track per-invocation timing\n";
            std::cout << "  --format tabular|csv  Output format (default: tabular)\n";
            std::cout << "  --unit auto|ns|us|ms  Time unit (default: auto)\n";
            std::cout << "  --decimals N          Decimal places (default: 2)\n";
            std::cout << "  --no-raw-ns           Hide raw nanoseconds\n";
            std::cout << "  --no-calls            Hide call counts\n";
            std::cout << "  --no-avg              Hide average per call\n";
            std::cout << "  --no-total            Hide total time\n";
            return 0;
        }
    }

    Chain chain;
    chain.add_link("validate", std::make_shared<ValidateInputLink>());
    chain.add_link("enrich", std::make_shared<EnrichCustomerLink>());
    chain.add_link("price", std::make_shared<PriceCalculationLink>());
    chain.add_link("discount", std::make_shared<ApplyDiscountsLink>());
    chain.add_link("persist", std::make_shared<PersistOrderLink>());
    chain.add_link("publish", std::make_shared<PublishEventLink>());

    // Links are now auto-connected sequentially - no manual connections needed!
    // chain.connect("validate", "enrich", always);
    // chain.connect("enrich", "price", always);
    // chain.connect("price", "discount", always);
    // chain.connect("discount", "persist", always);
    // chain.connect("persist", "publish", always);

        auto timing = std::make_shared<codeuchain::TimingMiddleware>(config, per_invocation, false);
    chain.use_middleware(timing);

    std::cout << "Runs: " << runs << " per-invocation: " << (per_invocation ? "on" : "off") << "\n";

    for (int r = 0; r < runs; ++r) {
        Context ctx;
        // Seed context with simple order
        ctx = ctx.insert("customer_id", 123 + r);
        ctx = ctx.insert("items", std::vector<std::string>{"19.99","5.00","3.50"});
        auto fut = chain.run(ctx);
        auto out = fut.get();
        if (r == runs - 1) {
            std::cout << "Final order summary:\n";
            auto total = out.get("total");
            if (total && std::holds_alternative<double>(*total)) {
                std::cout << "  total: " << std::get<double>(*total) << "\n";
            }
            if (auto oid = out.get("order_id")) {
                if (oid && std::holds_alternative<int>(*oid)) std::cout << "  order_id: " << std::get<int>(*oid) << "\n";
            }
            if (auto tier = out.get("loyalty_tier")) {
                if (tier && std::holds_alternative<std::string>(*tier)) std::cout << "  loyalty_tier: " << std::get<std::string>(*tier) << "\n";
            }
        }
    }

    timing->report(std::cout);
    return 0;
}
