/*!
Simple Example: Math Chain with Agape

With loving simplicity, chain math links and observe with middleware.
Demonstrates the new modular structure: core protocols, component implementations.
*/

use std::collections::HashMap;
use codeuchain::core::context::Context;

// Import from local examples
mod components;
use components::{BasicChain, MathLink, LoggingMiddleware};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Lovingly set up the chain using component implementations
    let mut chain = BasicChain::new();
    chain.add_link("sum".to_string(), Box::new(MathLink::new("sum".to_string())));
    chain.add_link("mean".to_string(), Box::new(MathLink::new("mean".to_string())));
    chain.connect(
        "sum".to_string(),
        "mean".to_string(),
        |ctx| ctx.get("result").is_some(),
    );
    chain.use_middleware(Box::new(LoggingMiddleware::new()));

    // Run with initial context
    let mut data = HashMap::new();
    data.insert("numbers".to_string(), serde_json::json!([1, 2, 3, 4, 5]));
    let ctx = Context::new(data);

    let result = chain.run(ctx).await;
    let result = match result {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error: {}", e);
            return Ok(());
        }
    };

    println!("Final result: {:?}", result.get("result"));
    println!("Full context: {:?}", result.to_hashmap());

    Ok(())
}