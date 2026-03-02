#pragma once

#include "middleware.hpp"
#include <chrono>
#include <mutex>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <iomanip>

namespace codeuchain {

// TimingMiddleware: measures wall-clock duration of each link invocation and overall chain execution.
// Usage:
//   auto mw = std::make_shared<TimingMiddleware>();
//   chain.use_middleware(mw);
// After chain.run(...).get(), call mw->report(std::ostream&) for a summary or fetch raw stats.
// Thread-safety: minimal locking; suitable for current single-threaded link execution model.
class TimingMiddleware : public IMiddleware {
public:
    struct Sample { double ns; };
    struct LinkStats {
        std::vector<double> samples_ns; // one per invocation (could aggregate later)
        double total_ns{0.0};
    };

    enum class OutputFormat { Tabular, CSV };
    enum class TimeUnit { Nano, Micro, Milli, Auto };

    struct FormatConfig {
        OutputFormat format = OutputFormat::Tabular;
        TimeUnit time_unit = TimeUnit::Auto;
        int decimal_places = 2;
        bool show_raw_ns = true;
        bool show_calls = true;
        bool show_avg = true;
        bool show_total = true;
    };

    TimingMiddleware(bool per_invocation = false, bool auto_print = false);
    TimingMiddleware(const FormatConfig& config, bool per_invocation = false, bool auto_print = false);

    std::coroutine_handle<> before(std::shared_ptr<ILink> link, const Context& context) override;
    std::coroutine_handle<> after(std::shared_ptr<ILink> link, const Context& context) override;

    std::string name() const override { return "TimingMiddleware"; }
    std::string description() const override { return "Measures per-link and total chain wall-clock time"; }

    // Produce a formatted report (human readable units + raw ns).
    void report(std::ostream& os) const;

    // Access raw stats (const).
    const std::unordered_map<std::string, LinkStats>& link_stats() const { return link_stats_; }
    double chain_total_ns() const { return chain_total_ns_; }

private:
    bool per_invocation_; // if false, keep only aggregate totals
    bool auto_print_;

    FormatConfig config_;

    using Clock = std::chrono::steady_clock;

    struct ActiveTiming {
        Clock::time_point start;
    };

    Clock::time_point chain_start_{};
    double chain_total_ns_{0.0};

    // We map raw pointer address (or special nullptr for chain-level) to start time.
    std::unordered_map<void*, ActiveTiming> active_; // ephemeral timing starts
    std::unordered_map<std::string, LinkStats> link_stats_;

    mutable std::mutex mutex_;

    std::string human_time(double ns_val) const;
};

} // namespace codeuchain
