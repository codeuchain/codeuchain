/*!/*!/*!/*!

Timing Middleware

Timing Middleware

High-performance timing middleware for measuring link execution times.

*/Timing MiddlewareTiming Middleware Utility: High-Performance Timing Component



use crate::core::{Context, Middleware, LegacyLink};High-performance timing middleware for measuring link execution times with multiple output formats.

use std::collections::HashMap;

use std::sync::{Arc, Mutex};*/

use std::time::{Duration, Instant};



/// Timing middleware for measuring execution times

pub struct TimingMiddleware {use crate::core::{Context, Middleware, LegacyLink};High-performance timing middleware for measuring link execution times with multiple output formats.A comprehensive timing middleware utility for CodeUChain that provides:

    per_invocation: bool,

    auto_print: bool,use std::collections::HashMap;

    stats: Arc<Mutex<HashMap<String, LinkStats>>>,

    start_times: Arc<Mutex<HashMap<String, Instant>>>,use std::sync::{Arc, Mutex};*/- Per-invocation and aggregate timing statistics

}

use std::time::{Duration, Instant};

#[derive(Debug, Clone)]

struct LinkStats {use serde::{Deserialize, Serialize};- Multiple output formats (Tabular, CSV)

    name: String,

    calls: u64,

    total_ns: u128,

    min_ns: u128,/// Time unit for formatting durationsuse crate::core::{Context, Middleware, LegacyLink};- Configurable time units and precision

    max_ns: u128,

}#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]



impl TimingMiddleware {pub enum TimeUnit {use std::collections::HashMap;- Thread-safe operation

    /// Create a new timing middleware with default configuration

    pub fn new() -> Self {    Nano,

        Self {

            per_invocation: false,    Micro,use std::sync::{Arc, Mutex};- Auto-print functionality

            auto_print: false,

            stats: Arc::new(Mutex::new(HashMap::new())),    Milli,

            start_times: Arc::new(Mutex::new(HashMap::new())),

        }    Auto,use std::time::{Duration, Instant};- Marketplace-ready component design

    }

}

    /// Create timing middleware with custom configuration

    pub fn with_config(per_invocation: bool, auto_print: bool) -> Self {use serde::{Deserialize, Serialize};

        Self {

            per_invocation,/// Output format for timing reports

            auto_print,

            stats: Arc::new(Mutex::new(HashMap::new())),#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]Usage:

            start_times: Arc::new(Mutex::new(HashMap::new())),

        }pub enum OutputFormat {

    }

    Tabular,/// Time unit for formatting durations```rust

    /// Generate timing report

    pub fn report(&self) -> String {    Csv,

        let stats = self.stats.lock().unwrap();

        if stats.is_empty() {}#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]use codeuchain::utils::timing_middleware::{TimingMiddleware, FormatConfig, TimeUnit, OutputFormat};

            return "No timing data collected".to_string();

        }



        let mut result = String::new();/// Configuration for timing middleware output formattingpub enum TimeUnit {

        result.push_str("== TimingMiddleware Report ==\n");

        result.push_str(&format!("{:<20} {:<8} {:<12} {:<12} {:<12}\n",#[derive(Debug, Clone, Serialize, Deserialize)]

            "Link", "Calls", "Total", "Avg", "Max"));

        result.push_str(&"-".repeat(64));pub struct FormatConfig {    Nano,let timing = TimingMiddleware::new();

        result.push_str("\n");

    pub time_unit: TimeUnit,

        for stat in stats.values() {

            let avg_ns = if stat.calls > 0 { stat.total_ns / stat.calls as u128 } else { 0 };    pub decimal_places: usize,    Micro,chain.use_middleware(Box::new(timing));

            let total_ms = stat.total_ns as f64 / 1_000_000.0;

            let avg_ms = avg_ns as f64 / 1_000_000.0;    pub show_raw_ns: bool,

            let max_ms = stat.max_ns as f64 / 1_000_000.0;

    pub output_format: OutputFormat,    Milli,

            result.push_str(&format!("{:<20} {:<8} {:<12.2} {:<12.2} {:<12.2}\n",

                stat.name,    pub show_total: bool,

                stat.calls,

                total_ms,    pub show_avg: bool,    Auto,// Or with custom config

                avg_ms,

                max_ms));    pub show_calls: bool,

        }

}}let config = FormatConfig {

        result

    }

}

impl Default for FormatConfig {    time_unit: TimeUnit::Auto,

#[async_trait::async_trait]

impl Middleware for TimingMiddleware {    fn default() -> Self {

    async fn before(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {

        if self.per_invocation {        Self {/// Output format for timing reports    decimal_places: 3,

            let mut start_times = self.start_times.lock().unwrap();

            start_times.insert(link.name().to_string(), Instant::now());            time_unit: TimeUnit::Auto,

        }

        Ok(())            decimal_places: 2,#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]    show_raw_ns: true,

    }

            show_raw_ns: false,

    async fn after(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {

        let duration = if self.per_invocation {            output_format: OutputFormat::Tabular,pub enum OutputFormat {    output_format: OutputFormat::Tabular,

            let mut start_times = self.start_times.lock().unwrap();

            if let Some(start) = start_times.remove(&link.name().to_string()) {            show_total: true,

                start.elapsed()

            } else {            show_avg: true,    Tabular,    show_total: true,

                Duration::from_nanos(0)

            }            show_calls: true,

        } else {

            Duration::from_nanos(0)        }    Csv,    show_avg: true,

        };

    }

        let ns = duration.as_nanos();

        let link_name = link.name().to_string();}}    show_calls: true,



        {

            let mut stats = self.stats.lock().unwrap();

            let stat = stats.entry(link_name.clone()).or_insert(LinkStats {/// Timing statistics for a single link};

                name: link_name,

                calls: 0,#[derive(Debug, Clone)]

                total_ns: 0,

                min_ns: u128::MAX,struct LinkStats {/// Configuration for timing middleware output formatting

                max_ns: 0,

            });    name: String,



            stat.calls += 1;    calls: u64,#[derive(Debug, Clone, Serialize, Deserialize)]let timing = TimingMiddleware::with_config(true, true, config);

            stat.total_ns += ns;

            stat.min_ns = stat.min_ns.min(ns);    total_ns: u128,

            stat.max_ns = stat.max_ns.max(ns);

        }    min_ns: u128,pub struct FormatConfig {chain.use_middleware(Box::new(timing));



        if self.auto_print && self.per_invocation {    max_ns: u128,

            println!("Timing: {} took {} ms", link.name(), ns as f64 / 1_000_000.0);

        }}    pub time_unit: TimeUnit,```



        Ok(())

    }

/// Timing middleware for measuring execution times    pub decimal_places: usize,*/

    fn name(&self) -> &str {

        "TimingMiddleware"pub struct TimingMiddleware {

    }

}    per_invocation: bool,    pub show_raw_ns: bool,



/// Create a minimal timing middleware configuration    auto_print: bool,

pub fn create_minimal_timing_middleware() -> TimingMiddleware {

    TimingMiddleware::with_config(true, true)    config: FormatConfig,    pub output_format: OutputFormat,use async_trait::async_trait;

}

    stats: Arc<Mutex<HashMap<String, LinkStats>>>,

/// Create a detailed timing middleware configuration

pub fn create_detailed_timing_middleware() -> TimingMiddleware {    start_times: Arc<Mutex<HashMap<String, Instant>>>,    pub show_total: bool,use crate::core::context::Context;

    TimingMiddleware::with_config(true, false)

}}



/// Create a CSV timing middleware configuration    pub show_avg: bool,use crate::core::link::LegacyLink;

pub fn create_csv_timing_middleware() -> TimingMiddleware {

    TimingMiddleware::with_config(true, false)impl TimingMiddleware {

}
    /// Create a new timing middleware with default configuration    pub show_calls: bool,use crate::core::middleware::Middleware;

    pub fn new() -> Self {

        Self::with_config(false, false, FormatConfig::default())}use std::collections::{HashMap, VecDeque};

    }

use std::sync::Mutex;

    /// Create timing middleware with custom configuration

    pub fn with_config(per_invocation: bool, auto_print: bool, config: FormatConfig) -> Self {impl Default for FormatConfig {

        Self {

            per_invocation,    fn default() -> Self {/// Comprehensive timing middleware for performance monitoring

            auto_print,

            config,        Self {#[derive(Debug)]

            stats: Arc::new(Mutex::new(HashMap::new())),

            start_times: Arc::new(Mutex::new(HashMap::new())),            time_unit: TimeUnit::Auto,pub struct TimingMiddleware {

        }

    }            decimal_places: 2,    per_invocation: bool,



    /// Format duration in human-readable format            show_raw_ns: false,    auto_print: bool,

    fn format_duration(&self, ns: u128) -> String {

        let ns_f64 = ns as f64;            output_format: OutputFormat::Tabular,    format_config: FormatConfig,

        match self.config.time_unit {

            TimeUnit::Nano => format!("{:.1$} ns", ns_f64, self.config.decimal_places),            show_total: true,    link_stats: Mutex<HashMap<String, LinkStats>>,

            TimeUnit::Micro => format!("{:.1$} μs", ns_f64 / 1000.0, self.config.decimal_places),

            TimeUnit::Milli => format!("{:.1$} ms", ns_f64 / 1_000_000.0, self.config.decimal_places),            show_avg: true,    active_timings: Mutex<VecDeque<(String, std::time::Instant)>>,

            TimeUnit::Auto => {

                if ns < 1_000 {            show_calls: true,    chain_start: Mutex<Option<std::time::Instant>>,

                    format!("{:.1$} ns", ns_f64, self.config.decimal_places)

                } else if ns < 1_000_000 {        }    chain_total_ns: Mutex<f64>,

                    format!("{:.1$} μs", ns_f64 / 1000.0, self.config.decimal_places)

                } else {    }    link_counter: Mutex<u32>, // Add a counter for unique IDs

                    format!("{:.1$} ms", ns_f64 / 1_000_000.0, self.config.decimal_places)

                }}}

            }

        }

    }

/// Timing statistics for a single link/// Configuration for timing middleware output formatting

    /// Generate timing report

    pub fn report(&self) -> String {#[derive(Debug, Clone)]#[derive(Debug, Clone)]

        let stats = self.stats.lock().unwrap();

        if stats.is_empty() {struct LinkStats {pub struct FormatConfig {

            return "No timing data collected".to_string();

        }    name: String,    pub time_unit: TimeUnit,



        let mut result = String::new();    calls: u64,    pub decimal_places: usize,

        result.push_str("== TimingMiddleware Report ==\n");

    total_ns: u128,    pub show_raw_ns: bool,

        match self.config.output_format {

            OutputFormat::Tabular => {    min_ns: u128,    pub output_format: OutputFormat,

                result.push_str(&format!("{:<20} {:<8} {:<12} {:<12} {:<12}\n",

                    "Link", "Calls", "Total", "Avg", "Max"));    max_ns: u128,    pub show_total: bool,

                result.push_str(&"-".repeat(64));

                result.push_str("\n");}    pub show_avg: bool,



                for stat in stats.values() {    pub show_calls: bool,

                    let avg_ns = if stat.calls > 0 { stat.total_ns / stat.calls as u128 } else { 0 };

                    result.push_str(&format!("{:<20} {:<8} {:<12} {:<12} {:<12}\n",/// Timing middleware for measuring execution times}

                        stat.name,

                        stat.calls,pub struct TimingMiddleware {

                        self.format_duration(stat.total_ns),

                        self.format_duration(avg_ns),    per_invocation: bool,/// Time unit options for display

                        self.format_duration(stat.max_ns)));

                }    auto_print: bool,#[derive(Debug, Clone)]

            }

            OutputFormat::Csv => {    config: FormatConfig,pub enum TimeUnit {

                result.push_str("Link,Calls,Total,Avg,Max\n");

                for stat in stats.values() {    stats: Arc<Mutex<HashMap<String, LinkStats>>>,    Nano,

                    let avg_ns = if stat.calls > 0 { stat.total_ns / stat.calls as u128 } else { 0 };

                    result.push_str(&format!("{},{},{},{},{}\n",    start_times: Arc<Mutex<HashMap<String, Instant>>>,    Micro,

                        stat.name,

                        stat.calls,}    Milli,

                        self.format_duration(stat.total_ns),

                        self.format_duration(avg_ns),    Auto,

                        self.format_duration(stat.max_ns)));

                }impl TimingMiddleware {}

            }

        }    /// Create a new timing middleware with default configuration



        result    pub fn new() -> Self {/// Output format options

    }

}        Self::with_config(false, false, FormatConfig::default())#[derive(Debug, Clone)]



#[async_trait::async_trait]    }pub enum OutputFormat {

impl Middleware for TimingMiddleware {

    async fn before(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {    Tabular,

        if self.per_invocation {

            let mut start_times = self.start_times.lock().unwrap();    /// Create timing middleware with custom configuration    CSV,

            start_times.insert(link.name().to_string(), Instant::now());

        }    pub fn with_config(per_invocation: bool, auto_print: bool, config: FormatConfig) -> Self {}

        Ok(())

    }        Self {



    async fn after(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {            per_invocation,/// Statistics for a single link

        let duration = if self.per_invocation {

            let mut start_times = self.start_times.lock().unwrap();            auto_print,#[derive(Debug, Clone)]

            if let Some(start) = start_times.remove(&link.name().to_string()) {

                start.elapsed()            config,struct LinkStats {

            } else {

                Duration::from_nanos(0)            stats: Arc::new(Mutex::new(HashMap::new())),    total_ns: f64,

            }

        } else {            start_times: Arc::new(Mutex::new(HashMap::new())),    samples_ns: Vec<f64>,

            Duration::from_nanos(0) // Simplified for basic implementation

        };        }}



        let ns = duration.as_nanos();    }

        let link_name = link.name().to_string();

impl TimingMiddleware {

        {

            let mut stats = self.stats.lock().unwrap();    /// Format duration in human-readable format    /// Create a new timing middleware with default settings

            let stat = stats.entry(link_name.clone()).or_insert(LinkStats {

                name: link_name,    fn format_duration(&self, ns: u128) -> String {    pub fn new() -> Self {

                calls: 0,

                total_ns: 0,        let ns_f64 = ns as f64;        Self {

                min_ns: u128::MAX,

                max_ns: 0,        match self.config.time_unit {            per_invocation: false,

            });

            TimeUnit::Nano => format!("{:.1$} ns", ns_f64, self.config.decimal_places),            auto_print: true,

            stat.calls += 1;

            stat.total_ns += ns;            TimeUnit::Micro => format!("{:.1$} μs", ns_f64 / 1000.0, self.config.decimal_places),            format_config: FormatConfig {

            stat.min_ns = stat.min_ns.min(ns);

            stat.max_ns = stat.max_ns.max(ns);            TimeUnit::Milli => format!("{:.1$} ms", ns_f64 / 1_000_000.0, self.config.decimal_places),                time_unit: TimeUnit::Auto,

        }

            TimeUnit::Auto => {                decimal_places: 2,

        if self.auto_print && self.per_invocation {

            println!("Timing: {} took {}", link.name(), self.format_duration(ns));                if ns < 1_000 {                show_raw_ns: false,

        }

                    format!("{:.1$} ns", ns_f64, self.config.decimal_places)                output_format: OutputFormat::Tabular,

        Ok(())

    }                } else if ns < 1_000_000 {                show_total: true,



    fn name(&self) -> &str {                    format!("{:.1$} μs", ns_f64 / 1000.0, self.config.decimal_places)                show_avg: true,

        "TimingMiddleware"

    }                } else {                show_calls: true,

}

                    format!("{:.1$} ms", ns_f64 / 1_000_000.0, self.config.decimal_places)            },

/// Create a minimal timing middleware configuration

pub fn create_minimal_timing_middleware() -> TimingMiddleware {                }            link_stats: Mutex::new(HashMap::new()),

    TimingMiddleware::with_config(

        true,            }            active_timings: Mutex::new(VecDeque::new()),

        true,

        FormatConfig {        }            chain_start: Mutex::new(None),

            time_unit: TimeUnit::Auto,

            decimal_places: 1,    }            chain_total_ns: Mutex::new(0.0),

            show_raw_ns: false,

            output_format: OutputFormat::Tabular,            link_counter: Mutex::new(0),

            show_total: false,

            show_avg: true,    /// Generate timing report        }

            show_calls: true,

        }    pub fn report(&self) -> String {    }

    )

}        let stats = self.stats.lock().unwrap();



/// Create a detailed timing middleware configuration        if stats.is_empty() {    /// Create timing middleware with custom configuration

pub fn create_detailed_timing_middleware() -> TimingMiddleware {

    TimingMiddleware::with_config(            return "No timing data collected".to_string();    pub fn with_config(per_invocation: bool, auto_print: bool, format_config: FormatConfig) -> Self {

        true,

        false,        }        Self {

        FormatConfig {

            time_unit: TimeUnit::Micro,            per_invocation,

            decimal_places: 3,

            show_raw_ns: true,        let mut result = String::new();            auto_print,

            output_format: OutputFormat::Tabular,

            show_total: true,        result.push_str("== TimingMiddleware Report ==\n");            format_config,

            show_avg: true,

            show_calls: true,            link_stats: Mutex::new(HashMap::new()),

        }

    )        match self.config.output_format {            active_timings: Mutex::new(VecDeque::new()),

}

            OutputFormat::Tabular => {            chain_start: Mutex::new(None),

/// Create a CSV timing middleware configuration

pub fn create_csv_timing_middleware() -> TimingMiddleware {                result.push_str(&format!("{:<20} {:<8} {:<12} {:<12} {:<12}\n",            chain_total_ns: Mutex::new(0.0),

    TimingMiddleware::with_config(

        true,                    "Link", "Calls", "Total", "Avg", "Max"));            link_counter: Mutex::new(0),

        false,

        FormatConfig {                result.push_str(&"-".repeat(64));        }

            time_unit: TimeUnit::Milli,

            decimal_places: 2,                result.push_str("\n");    }

            show_raw_ns: false,

            output_format: OutputFormat::Csv,

            show_total: true,

            show_avg: true,                for stat in stats.values() {    /// Format time duration in human-readable format

            show_calls: true,

        }                    let avg_ns = if stat.calls > 0 { stat.total_ns / stat.calls as u128 } else { 0 };    fn human_time(&self, ns_val: f64) -> String {

    )

}                    result.push_str(&format!("{:<20} {:<8} {:<12} {:<12} {:<12}\n",        let mut display_val = ns_val;

                        stat.name,        let unit: String;

                        stat.calls,

                        self.format_duration(stat.total_ns),        match self.format_config.time_unit {

                        self.format_duration(avg_ns),            TimeUnit::Nano => {

                        self.format_duration(stat.max_ns)));                unit = "ns".to_string();

                }            }

            }            TimeUnit::Micro => {

            OutputFormat::Csv => {                display_val = ns_val / 1e3;

                result.push_str("Link,Calls,Total,Avg,Max\n");                unit = "µs".to_string();

                for stat in stats.values() {            }

                    let avg_ns = if stat.calls > 0 { stat.total_ns / stat.calls as u128 } else { 0 };            TimeUnit::Milli => {

                    result.push_str(&format!("{},{},{},{},{}\n",                display_val = ns_val / 1e6;

                        stat.name,                unit = "ms".to_string();

                        stat.calls,            }

                        self.format_duration(stat.total_ns),            TimeUnit::Auto => {

                        self.format_duration(avg_ns),                if ns_val < 1000.0 {

                        self.format_duration(stat.max_ns)));                    unit = "ns".to_string();

                }                } else if ns_val < 1e6 {

            }                    display_val = ns_val / 1e3;

        }                    unit = "µs".to_string();

                } else if ns_val < 1e9 {

        result                    display_val = ns_val / 1e6;

    }                    unit = "ms".to_string();

}                } else {

                    display_val = ns_val / 1e9;

#[async_trait::async_trait]                    unit = "s".to_string();

impl Middleware for TimingMiddleware {                }

    async fn before(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {            }

        if self.per_invocation {        }

            let mut start_times = self.start_times.lock().unwrap();

            start_times.insert(link.name().to_string(), Instant::now());        let mut result = format!("{:.precision$} {}", display_val, unit, precision = self.format_config.decimal_places);

        }        if self.format_config.show_raw_ns && !matches!(self.format_config.time_unit, TimeUnit::Nano) {

        Ok(())            result.push_str(&format!(" ({:.0} ns)", ns_val));

    }        }

        result

    async fn after(&self, link: &dyn LegacyLink, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {    }

        let duration = if self.per_invocation {

            let mut start_times = self.start_times.lock().unwrap();    /// Generate and print timing report

            if let Some(start) = start_times.remove(&link.name().to_string()) {    pub fn report(&self) {

                start.elapsed()        let link_stats = self.link_stats.lock().unwrap();

            } else {        let chain_total_ns = *self.chain_total_ns.lock().unwrap();

                Duration::from_nanos(0)

            }        match self.format_config.output_format {

        } else {            OutputFormat::CSV => {

            Duration::from_nanos(0) // Simplified for basic implementation                // CSV header

        };                print!("Link");

                if self.format_config.show_total { print!(",Total"); }

        let ns = duration.as_nanos();                if self.format_config.show_avg { print!(",Avg/Call"); }

        let link_name = link.name().to_string();                if self.format_config.show_calls { print!(",Calls"); }

                println!();

        {

            let mut stats = self.stats.lock().unwrap();                // CSV data rows

            let stat = stats.entry(link_name.clone()).or_insert(LinkStats {                for (name, stats) in link_stats.iter() {

                name: link_name,                    let calls = if self.per_invocation { stats.samples_ns.len() } else { if stats.total_ns > 0.0 { 1 } else { 0 } };

                calls: 0,                    let avg = if self.per_invocation && !stats.samples_ns.is_empty() {

                total_ns: 0,                        stats.total_ns / stats.samples_ns.len() as f64

                min_ns: u128::MAX,                    } else {

                max_ns: 0,                        stats.total_ns

            });                    };



            stat.calls += 1;                    print!("{}", name);

            stat.total_ns += ns;                    if self.format_config.show_total { print!(",{}", self.human_time(stats.total_ns)); }

            stat.min_ns = stat.min_ns.min(ns);                    if self.format_config.show_avg { print!(",{}", self.human_time(avg)); }

            stat.max_ns = stat.max_ns.max(ns);                    if self.format_config.show_calls { print!(",{}", calls); }

        }                    println!();

                }

        if self.auto_print && self.per_invocation {

            println!("Timing: {} took {}", link.name(), self.format_duration(ns));                // Chain total row

        }                print!("[Chain Total]");

                if self.format_config.show_total { print!(",{}", self.human_time(chain_total_ns)); }

        Ok(())                if self.format_config.show_avg { print!(","); }

    }                if self.format_config.show_calls { print!(","); }

                println!();

    fn name(&self) -> &str {            }

        "TimingMiddleware"            OutputFormat::Tabular => {

    }                println!("\n== TimingMiddleware Report ==");

}

                // Calculate column widths

/// Create a minimal timing middleware configuration                let link_width = 24;

pub fn create_minimal_timing_middleware() -> TimingMiddleware {                let total_width = 18;

    TimingMiddleware::with_config(                let avg_width = 14;

        true,                let calls_width = 10;

        true,

        FormatConfig {                // Header

            time_unit: TimeUnit::Auto,                print!("{:<link_width$}", "Link");

            decimal_places: 1,                if self.format_config.show_total { print!("{:<total_width$}", "Total"); }

            show_raw_ns: false,                if self.format_config.show_avg { print!("{:<avg_width$}", "Avg/Call"); }

            output_format: OutputFormat::Tabular,                if self.format_config.show_calls { print!("{:<calls_width$}", "Calls"); }

            show_total: false,                println!();

            show_avg: true,

            show_calls: true,                // Separator

        }                let mut sep_width = link_width;

    )                if self.format_config.show_total { sep_width += total_width; }

}                if self.format_config.show_avg { sep_width += avg_width; }

                if self.format_config.show_calls { sep_width += calls_width; }

/// Create a detailed timing middleware configuration                println!("{}", "-".repeat(sep_width));

pub fn create_detailed_timing_middleware() -> TimingMiddleware {

    TimingMiddleware::with_config(                // Data rows

        true,                for (name, stats) in link_stats.iter() {

        false,                    let calls = if self.per_invocation { stats.samples_ns.len() } else { if stats.total_ns > 0.0 { 1 } else { 0 } };

        FormatConfig {                    let avg = if self.per_invocation && !stats.samples_ns.is_empty() {

            time_unit: TimeUnit::Micro,                        stats.total_ns / stats.samples_ns.len() as f64

            decimal_places: 3,                    } else {

            show_raw_ns: true,                        stats.total_ns

            output_format: OutputFormat::Tabular,                    };

            show_total: true,

            show_avg: true,                    print!("{:<link_width$}", name);

            show_calls: true,                    if self.format_config.show_total { print!("{:<total_width$}", self.human_time(stats.total_ns)); }

        }                    if self.format_config.show_avg { print!("{:<avg_width$}", self.human_time(avg)); }

    )                    if self.format_config.show_calls { print!("{:<calls_width$}", calls); }

}                    println!();

                }

/// Create a CSV timing middleware configuration

pub fn create_csv_timing_middleware() -> TimingMiddleware {                // Chain total

    TimingMiddleware::with_config(                println!("{}", "-".repeat(sep_width));

        true,                print!("{:<link_width$}", "[Chain Total]");

        false,                if self.format_config.show_total { print!("{}", self.human_time(chain_total_ns)); }

        FormatConfig {                println!();

            time_unit: TimeUnit::Milli,            }

            decimal_places: 2,        }

            show_raw_ns: false,    }

            output_format: OutputFormat::Csv,

            show_total: true,    /// Get raw statistics (for programmatic access)

            show_avg: true,    pub fn get_stats(&self) -> HashMap<String, (f64, usize, f64)> {

            show_calls: true,        let link_stats = self.link_stats.lock().unwrap();

        }        let mut result = HashMap::new();

    )

}        for (name, stats) in link_stats.iter() {
            let calls = if self.per_invocation { stats.samples_ns.len() } else { if stats.total_ns > 0.0 { 1 } else { 0 } };
            let avg = if self.per_invocation && !stats.samples_ns.is_empty() {
                stats.total_ns / stats.samples_ns.len() as f64
            } else {
                stats.total_ns
            };
            result.insert(name.clone(), (stats.total_ns, calls, avg));
        }

        result
    }

    /// Reset all timing statistics
    pub fn reset(&self) {
        // Lock in consistent order to avoid deadlocks
        let mut chain_start = self.chain_start.lock().unwrap();
        let mut chain_total_ns = self.chain_total_ns.lock().unwrap();
        let mut link_stats = self.link_stats.lock().unwrap();
        let mut active_timings = self.active_timings.lock().unwrap();
        let mut link_counter = self.link_counter.lock().unwrap();

        link_stats.clear();
        active_timings.clear();
        *chain_start = None;
        *chain_total_ns = 0.0;
        *link_counter = 0;
    }
}

#[async_trait]
impl Middleware for TimingMiddleware {
    async fn before(&self, link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let now = std::time::Instant::now();

        if link.is_some() {
            let mut counter = self.link_counter.lock().unwrap();
            let link_id = format!("link_{}", *counter);
            *counter += 1;
            
            let mut active_timings = self.active_timings.lock().unwrap();
            active_timings.push_back((link_id, now));
        } else {
            let mut chain_start = self.chain_start.lock().unwrap();
            *chain_start = Some(now);
        }

        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let now = std::time::Instant::now();

        if link.is_some() {
            let mut active_timings = self.active_timings.lock().unwrap();
            // Get the most recently added timing (LIFO - pop from back)
            if let Some((link_id, start)) = active_timings.pop_back() {
                let duration_ns = now.duration_since(start).as_nanos() as f64;

                let mut link_stats = self.link_stats.lock().unwrap();
                let stats = link_stats.entry(link_id).or_insert(LinkStats {
                    total_ns: 0.0,
                    samples_ns: Vec::new(),
                });

                stats.total_ns += duration_ns;
                if self.per_invocation {
                    stats.samples_ns.push(duration_ns);
                }
            }
        } else {
            // Chain finished - calculate total time and print report
            // Lock in consistent order to avoid deadlocks
            let chain_start = *self.chain_start.lock().unwrap();
            let mut chain_total_ns = self.chain_total_ns.lock().unwrap();

            if let Some(start_time) = chain_start {
                *chain_total_ns = now.duration_since(start_time).as_nanos() as f64;
                if self.auto_print {
                    // Release locks before calling report to avoid deadlocks
                    drop(chain_total_ns);
                    self.report();
                    // Reset for next run
                    self.reset();
                    return Ok(());
                }
            }
        }

        Ok(())
    }
}

impl Default for TimingMiddleware {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            time_unit: TimeUnit::Auto,
            decimal_places: 2,
            show_raw_ns: false,
            output_format: OutputFormat::Tabular,
            show_total: true,
            show_avg: true,
            show_calls: true,
        }
    }
}

/// Convenience function to create a basic timing middleware
pub fn create_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::new()
}

/// Convenience function to create a CSV timing middleware for data export
pub fn create_csv_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(
        true, // per_invocation
        false, // auto_print
        FormatConfig {
            time_unit: TimeUnit::Micro,
            decimal_places: 2,
            show_raw_ns: false,
            output_format: OutputFormat::CSV,
            show_total: true,
            show_avg: true,
            show_calls: true,
        }
    )
}

/// Convenience function to create a minimal timing middleware (only totals)
pub fn create_minimal_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(
        false, // per_invocation
        true, // auto_print
        FormatConfig {
            time_unit: TimeUnit::Auto,
            decimal_places: 2,
            show_raw_ns: false,
            output_format: OutputFormat::Tabular,
            show_total: true,
            show_avg: false,
            show_calls: false,
        }
    )
}

/// Convenience function to create a detailed timing middleware with raw nanoseconds
pub fn create_detailed_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(
        true, // per_invocation
        true, // auto_print
        FormatConfig {
            time_unit: TimeUnit::Auto,
            decimal_places: 3,
            show_raw_ns: true,
            output_format: OutputFormat::Tabular,
            show_total: true,
            show_avg: true,
            show_calls: true,
        }
    )
}