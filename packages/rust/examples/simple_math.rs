/*!
Simple Example: Math Chain Processing

Demonstrates modular chain processing with math links and hook.
Shows the new modular structure: core protocols, component implementations.
*/

use std::collections::HashMap;
use codeuchain::core::state::State;

// Import from local examples
mod components;
use components::{BasicChain, MathLink, LoggingHook};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Set up the chain using component implementations
    let mut chain = BasicChain::new();
    chain.add_link("sum".to_string(), Box::new(MathLink::new("sum".to_string())));
    chain.add_link("mean".to_string(), Box::new(MathLink::new("mean".to_string())));
    chain.connect(
        "sum".to_string(),
        "mean".to_string(),
        |ctx| ctx.get("result").is_some(),
    );
    chain.use_hook(Box::new(LoggingHook::new()));

    // Run with initial state
    let mut data = HashMap::new();
    data.insert("numbers".to_string(), serde_json::json!([1, 2, 3, 4, 5]));
    let ctx = State::new(data);

    let result = chain.run(ctx).await;
    let result = match result {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error: {}", e);
            return Ok(());
        }
    };

    println!("Final result: {:?}", result.get("result"));
    println!("Full state: {:?}", result.to_hashmap());

    Ok(())
}