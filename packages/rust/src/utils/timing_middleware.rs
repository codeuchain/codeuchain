/*!
Timing Middleware

High-performance timing middleware for measuring link execution times.
*/

use crate::{Context, Middleware, LegacyLink};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

/// Timing middleware for measuring execution times
pub struct TimingMiddleware {
    per_invocation: bool,
    auto_print: bool,
    stats: Arc<Mutex<HashMap<String, LinkStats>>>,
    start_times: Arc<Mutex<HashMap<String, Instant>>>,
}

#[derive(Debug, Clone)]
struct LinkStats {
    name: String,
    calls: u64,
    total_ns: u128,
    min_ns: u128,
    max_ns: u128,
}

impl TimingMiddleware {
    /// Create a new timing middleware with default configuration
    pub fn new() -> Self {
        Self {
            per_invocation: false,
            auto_print: false,
            stats: Arc::new(Mutex::new(HashMap::new())),
            start_times: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Create timing middleware with custom configuration
    pub fn with_config(per_invocation: bool, auto_print: bool) -> Self {
        Self {
            per_invocation,
            auto_print,
            stats: Arc::new(Mutex::new(HashMap::new())),
            start_times: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Get timing statistics
    pub fn get_stats(&self) -> HashMap<String, (f64, u64, f64)> {
        let stats = self.stats.lock().unwrap();
        let mut result = HashMap::new();

        for (name, stat) in stats.iter() {
            let total_ms = stat.total_ns as f64 / 1_000_000.0;
            let avg_ms = if stat.calls > 0 {
                stat.total_ns as f64 / (stat.calls as f64 * 1_000_000.0)
            } else {
                0.0
            };
            result.insert(name.clone(), (total_ms, stat.calls, avg_ms));
        }

        result
    }
}

#[async_trait::async_trait]
impl Middleware for TimingMiddleware {
    async fn before(&self, link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if self.per_invocation {
            if let Some(_link) = link {
                let link_name = std::any::type_name::<dyn LegacyLink>();
                let mut start_times = self.start_times.lock().unwrap();
                start_times.insert(link_name.to_string(), Instant::now());
            }
        }
        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let duration = if self.per_invocation {
            if let Some(_link) = link {
                let link_name = std::any::type_name::<dyn LegacyLink>();
                let mut start_times = self.start_times.lock().unwrap();
                if let Some(start) = start_times.remove(&link_name.to_string()) {
                    start.elapsed()
                } else {
                    Duration::from_nanos(0)
                }
            } else {
                Duration::from_nanos(0)
            }
        } else {
            Duration::from_nanos(0)
        };

        let ns = duration.as_nanos();
        let link_name = if let Some(_) = link {
            std::any::type_name::<dyn LegacyLink>().to_string()
        } else {
            "unknown".to_string()
        };

        {
            let mut stats = self.stats.lock().unwrap();
            let stat = stats.entry(link_name.clone()).or_insert(LinkStats {
                name: link_name,
                calls: 0,
                total_ns: 0,
                min_ns: u128::MAX,
                max_ns: 0,
            });

            stat.calls += 1;
            stat.total_ns += ns;
            stat.min_ns = stat.min_ns.min(ns);
            stat.max_ns = stat.max_ns.max(ns);
        }

        if self.auto_print && self.per_invocation && link.is_some() {
            println!("Timing: {} took {} ms", std::any::type_name::<dyn LegacyLink>(), ns as f64 / 1_000_000.0);
        }

        Ok(())
    }
}

/// Create a minimal timing middleware configuration
pub fn create_minimal_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(true, true)
}

/// Create a detailed timing middleware configuration
pub fn create_detailed_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(true, false)
}

/// Create a CSV timing middleware configuration
pub fn create_csv_timing_middleware() -> TimingMiddleware {
    TimingMiddleware::with_config(true, false)
}