/*!
Timing Middleware Format Test Example

This example demonstra    // Test 5: Custom configuration - microseconds with more precision
    println!("\n📊 Test 5: Custom Config (Microseconds, No Calls)");
    println!("---------------------------------------------------");
    let custom_timing = TimingMiddleware::with_config(
        false, // per_invocation
        true,  // auto_print
        FormatConfig {
            time_unit: TimeUnit::Micro,
            decimal_places: 2,
            show_raw_ns: false,
            output_format: OutputFormat::Tabular,
            show_total: true,
            show_avg: true,
            show_calls: false,
        }
    );
    test_format(custom_timing, &link1, &link2, &link3).await?;rent output formats and configurations
available in the CodeUChain timing middleware, matching the C++ implementation.
*/

use codeuchain::core::{Context, Chain};
use codeuchain::core::link::LegacyLink;
use codeuchain::utils::TimingMiddleware;
use codeuchain::utils::timing_middleware::{FormatConfig, TimeUnit, OutputFormat, create_csv_timing_middleware, create_minimal_timing_middleware, create_detailed_timing_middleware};
use std::collections::HashMap;
use serde_json::Value;

#[derive(Clone)]
struct TestLink {
    name: String,
    delay_ms: u64,
}

impl TestLink {
    fn new(name: String, delay_ms: u64) -> Self {
        Self { name, delay_ms }
    }
}

#[async_trait::async_trait]
impl LegacyLink for TestLink {
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        // Simulate some work
        Ok(ctx.insert("processed".to_string(), Value::String(format!("{} processed", self.name))))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("🚀 CodeUChain Timing Middleware Format Test");
    println!("==========================================\n");

    // Create test links
    let link1 = TestLink::new("link1".to_string(), 10);
    let link2 = TestLink::new("link2".to_string(), 20);
    let link3 = TestLink::new("link3".to_string(), 15);

    // Test 1: Default tabular format (all options enabled)
    println!("📊 Test 1: Default Tabular Format (All Options)");
    println!("------------------------------------------------");
    test_format(TimingMiddleware::new(), &link1, &link2, &link3).await?;

    // Test 2: Minimal format (only totals)
    println!("\n📊 Test 2: Minimal Format (Totals Only)");
    println!("---------------------------------------");
    test_format(create_minimal_timing_middleware(), &link1, &link2, &link3).await?;

    // Test 3: Detailed format with raw nanoseconds
    println!("\n📊 Test 3: Detailed Format (With Raw Nanoseconds)");
    println!("--------------------------------------------------");
    test_format(create_detailed_timing_middleware(), &link1, &link2, &link3).await?;

    // Test 4: CSV format (with auto_print enabled for demo)
    println!("\n📊 Test 4: CSV Format");
    println!("---------------------");
    let csv_timing = TimingMiddleware::with_config(
        true, // per_invocation
        true, // auto_print - enabled for demo
        FormatConfig {
            time_unit: TimeUnit::Micro,
            decimal_places: 2,
            show_raw_ns: false,
            output_format: OutputFormat::CSV,
            show_total: true,
            show_avg: true,
            show_calls: true,
        }
    );
    test_format(csv_timing, &link1, &link2, &link3).await?;

    // Test 5: Custom configuration - milliseconds only
    println!("\n📊 Test 5: Custom Config (Milliseconds, No Calls)");
    println!("---------------------------------------------------");
    let custom_timing = TimingMiddleware::with_config(
        false, // per_invocation
        true,  // auto_print
        FormatConfig {
            time_unit: TimeUnit::Milli,
            decimal_places: 1,
            show_raw_ns: false,
            output_format: OutputFormat::Tabular,
            show_total: true,
            show_avg: true,
            show_calls: false,
        }
    );
    test_format(custom_timing, &link1, &link2, &link3).await?;

    println!("\n✅ All format tests completed successfully!");
    println!("💡 The timing middleware supports multiple output formats:");
    println!("   - Tabular (with configurable columns)");
    println!("   - CSV (for data export)");
    println!("   - Custom time units and precision");
    println!("   - Optional raw nanosecond display");

    Ok(())
}

async fn test_format(
    timing: TimingMiddleware,
    link1: &TestLink,
    link2: &TestLink,
    link3: &TestLink,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Create chain
    let mut chain = Chain::new();
    chain.add_link("link1".to_string(), Box::new(link1.clone()));
    chain.add_link("link2".to_string(), Box::new(link2.clone()));
    chain.add_link("link3".to_string(), Box::new(link3.clone()));
    chain.use_middleware(Box::new(timing));

    // Create context
    let mut initial_data = HashMap::new();
    initial_data.insert("test".to_string(), Value::String("data".to_string()));
    let ctx = Context::new(initial_data);

    // Run chain
    let result = chain.run(ctx).await?;
    assert!(result.get("processed").is_some());

    Ok(())
}