# CodeUChain Rust: Memory-Safe Implementation

CodeUChain provides a memory-safe framework for chaining processing links with hook support and ownership guarantees.

## 🤖 LLM Support

This package supports the [llm.txt standard](https://codeuchain.github.io/codeuchain/rust/llm.txt) for easy AI/LLM integration. See [llm-full.txt](https://codeuchain.github.io/codeuchain/rust/llm-full.txt) for comprehensive documentation.

## Features
- **State:** Immutable by default, mutable for flexibility—embracing Rust's ownership model.
- **Link:** Selfless processors, async and ecosystem-rich.
- **Chain:** Harmonious connectors with conditional flows.
- **Hook:** Gentle enhancers, optional and forgiving.
- **Error Handling:** Compassionate routing and retries.

## Installation
Add this to your `Cargo.toml`:
```toml
[dependencies]
codeuchain = "0.1.0"
tokio = { version = "1.0", features = ["full"] }
```

## Quick Start
```rust
use codeuchain::{State, Chain, MathLink, LoggingHook};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut chain = Chain::new();
    chain.add_link("math".to_string(), Box::new(MathLink::new("sum".to_string())));
    chain.use_hook(Box::new(LoggingHook::new()));

    let mut data = HashMap::new();
    data.insert("numbers".to_string(), serde_json::json!([1, 2, 3]));
    let ctx = State::new(data);

    let result = chain.run(ctx).await?;
    println!("Result: {:?}", result.get("result"));  // 6.0
    Ok(())
}
```

## Architecture

### Core Module (`src/core/`)
- **`State`**: Immutable data container with serde integration
- **`Link`**: Async trait for processing units
- **`Chain`**: Orchestrator for link execution
- **`Hook`**: Trait for cross-cutting concerns

### Utils Module (`src/utils/`)
- **Error Handling**: Retry mechanisms and error routing
- **Common Utilities**: Shared functionality across implementations

### Examples (`examples/`)
- **Components**: Reusable implementations
- **Simple Math**: Basic usage demonstration

## Usage Patterns

### 1. Basic Usage (Library Components)
```rust
use codeuchain::{State, Chain};
use codeuchain::examples::components::{MathLink, LoggingHook};
use std::collections::HashMap;

let mut chain = Chain::new();
chain.add_link("math".to_string(), Box::new(MathLink::new("sum".to_string())));
chain.use_hook(Box::new(LoggingHook::new()));
```

### 2. Custom Components (Project-Specific)
```rust
use async_trait::async_trait;
use codeuchain::core::{State, Link};
use serde_json::Value;

struct MyCustomLink;

#[async_trait]
impl Link for MyCustomLink {
    async fn call(&self, ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>> {
        // Your custom logic
        Ok(ctx.insert("result".to_string(), Value::String("custom_value".to_string())))
    }
}
```

### 3. Project Composition (Human Oversight)
```rust
use codeuchain::examples::components::BasicChain;

let mut chain = BasicChain::new();
chain.add_link("custom".to_string(), Box::new(MyCustomLink));
chain.use_hook(Box::new(MyCustomHook::new()));
```

## Design Approach
Optimized for Rust's safety and performance—memory-safe, async-native, with zero-cost abstractions. Start fresh, build reliable processing pipelines.

## Running Examples
```bash
# Run the simple math example
cargo run --example simple_math

# Run tests
cargo test
```

## Development
```bash
# Build
cargo build

# Run all tests
cargo test

# Run specific test
cargo test test_state_operations

# Check code
cargo check

# Format code
cargo fmt

# Lint code
cargo clippy
```