#include "codeuchain/timing_middleware.hpp"
#include <iostream>
#include <algorithm>

namespace codeuchain {

TimingMiddleware::TimingMiddleware(bool per_invocation, bool auto_print)
    : per_invocation_(per_invocation), auto_print_(auto_print) {}

TimingMiddleware::TimingMiddleware(const FormatConfig& config, bool per_invocation, bool auto_print)
    : per_invocation_(per_invocation), auto_print_(auto_print), config_(config) {}

std::string TimingMiddleware::human_time(double ns_val) const {
    std::ostringstream oss;
    double display_val = ns_val;
    std::string unit;

    switch (config_.time_unit) {
        case TimeUnit::Nano:
            display_val = ns_val;
            unit = "ns";
            break;
        case TimeUnit::Micro:
            display_val = ns_val / 1e3;
            unit = "µs";
            break;
        case TimeUnit::Milli:
            display_val = ns_val / 1e6;
            unit = "ms";
            break;
        case TimeUnit::Auto:
        default:
            if (ns_val < 1000.0) {
                display_val = ns_val;
                unit = "ns";
            } else if (ns_val < 1e6) {
                display_val = ns_val / 1e3;
                unit = "µs";
            } else if (ns_val < 1e9) {
                display_val = ns_val / 1e6;
                unit = "ms";
            } else {
                display_val = ns_val / 1e9;
                unit = "s";
            }
            break;
    }

    oss << std::fixed << std::setprecision(config_.decimal_places) << display_val << " " << unit;
    if (config_.show_raw_ns && config_.time_unit != TimeUnit::Nano) {
        oss << " (" << std::fixed << std::setprecision(2) << ns_val << " ns)";
    }
    return oss.str();
}

std::coroutine_handle<> TimingMiddleware::before(std::shared_ptr<ILink> link, const Context&) {
    auto now = Clock::now();
    std::scoped_lock lock(mutex_);
    if (!link) {
        chain_start_ = now;
    } else {
        active_[link.get()] = ActiveTiming{now};
    }
    return std::coroutine_handle<>();
}

std::coroutine_handle<> TimingMiddleware::after(std::shared_ptr<ILink> link, const Context&) {
    auto now = Clock::now();
    std::scoped_lock lock(mutex_);
    if (!link) {
        if (chain_total_ns_ == 0.0 && chain_start_ != Clock::time_point{}) {
            chain_total_ns_ = std::chrono::duration_cast<std::chrono::nanoseconds>(now - chain_start_).count();
            if (auto_print_) {
                report(std::cout);
            }
        }
    } else {
        auto it = active_.find(link.get());
        if (it != active_.end()) {
            double dur_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(now - it->second.start).count();
            active_.erase(it);
            auto & stats = link_stats_[link->name()];
            stats.total_ns += dur_ns;
            if (per_invocation_) {
                stats.samples_ns.push_back(dur_ns);
            }
        }
    }
    return std::coroutine_handle<>();
}

void TimingMiddleware::report(std::ostream& os) const {
    std::scoped_lock lock(mutex_);

    if (config_.format == OutputFormat::CSV) {
        // CSV header
        os << "Link";
        if (config_.show_total) os << ",Total";
        if (config_.show_avg) os << ",Avg/Call";
        if (config_.show_calls) os << ",Calls";
        os << "\n";

        // CSV data rows
        for (const auto& [name, stats] : link_stats_) {
            size_t calls = per_invocation_ ? stats.samples_ns.size() : (stats.total_ns > 0 ? 1 : 0);
            double avg = 0.0;
            if (per_invocation_ && !stats.samples_ns.empty()) {
                avg = stats.total_ns / stats.samples_ns.size();
            } else {
                avg = stats.total_ns;
            }

            os << name;
            if (config_.show_total) os << "," << human_time(stats.total_ns);
            if (config_.show_avg) os << "," << human_time(avg);
            if (config_.show_calls) os << "," << calls;
            os << "\n";
        }

        // Chain total row
        os << "[Chain Total]";
        if (config_.show_total) os << "," << human_time(chain_total_ns_);
        if (config_.show_avg) os << ",";
        if (config_.show_calls) os << ",";
        os << "\n";
    } else {
        // Tabular format
        os << "\n== TimingMiddleware Report ==\n";

        // Calculate column widths
        int link_width = 24;
        int total_width = 18;
        int avg_width = 14;
        int calls_width = 10;

        // Header
        os << std::left << std::setw(link_width) << "Link";
        if (config_.show_total) os << std::setw(total_width) << "Total";
        if (config_.show_avg) os << std::setw(avg_width) << "Avg/Call";
        if (config_.show_calls) os << std::setw(calls_width) << "Calls";
        os << "\n";

        // Separator
        int total_sep_width = link_width;
        if (config_.show_total) total_sep_width += total_width;
        if (config_.show_avg) total_sep_width += avg_width;
        if (config_.show_calls) total_sep_width += calls_width;
        os << std::string(total_sep_width, '-') << "\n";

        // Data rows
        for (const auto& [name, stats] : link_stats_) {
            size_t calls = per_invocation_ ? stats.samples_ns.size() : (stats.total_ns > 0 ? 1 : 0);
            double avg = 0.0;
            if (per_invocation_ && !stats.samples_ns.empty()) {
                avg = stats.total_ns / stats.samples_ns.size();
            } else {
                avg = stats.total_ns;
            }

            os << std::left << std::setw(link_width) << name;
            if (config_.show_total) os << std::setw(total_width) << human_time(stats.total_ns);
            if (config_.show_avg) os << std::setw(avg_width) << human_time(avg);
            if (config_.show_calls) os << std::setw(calls_width) << calls;
            os << "\n";
        }

        // Chain total
        os << std::string(total_sep_width, '-') << "\n";
        os << std::left << std::setw(link_width) << "[Chain Total]";
        if (config_.show_total) os << human_time(chain_total_ns_);
        os << "\n";
    }
}

} // namespace codeuchain
