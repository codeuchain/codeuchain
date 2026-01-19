---
applyTo: 'packages/rust/**'
---

# Rust Development Instructions

## 🦀 Rust-Specific Guidelines

CodeUChain's Rust implementation is **production-ready** with strong memory safety guarantees and excellent performance.

## Project Structure

```
packages/rust/
├── src/
│   ├── core/          # Core framework (Context, Link, Chain)
│   ├── middleware/    # Middleware implementations
│   └── lib.rs        # Library root
├── tests/            # Integration tests
├── examples/         # Example implementations
└── Cargo.toml       # Package manifest
```

## Development Setup

### Prerequisites
- Rust 1.70 or higher
- Cargo (comes with Rust)

### Installation
```bash
cd packages/rust
cargo build          # Build project
cargo build --release # Release build
```

### Running Tests
```bash
cd packages/rust
cargo test                    # Run all tests
cargo test --verbose          # Verbose output
cargo test test_name          # Run specific test
cargo test -- --nocapture    # Show stdout
cargo test --doc             # Run doc tests
```

### Running Linters
```bash
cd packages/rust
cargo fmt            # Format code
cargo clippy         # Run linter
cargo check          # Check without building
```

## Code Style

### Module Organization
```rust
// src/core/context.rs
use serde_json::Value;
use std::collections::HashMap;

/// Public struct with documentation
pub struct Context<T = Value> {
    data: HashMap<String, Value>,
    _phantom: std::marker::PhantomData<T>,
}

/// Private helper function
fn create_empty() -> HashMap<String, Value> {
    HashMap::new()
}
```

### Error Handling
```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LinkError {
    #[error("processing failed: {0}")]
    ProcessingError(String),
    #[error("invalid input: {0}")]
    InvalidInput(String),
}

impl MyLink {
    pub async fn call(&self, ctx: Context<InputData>) 
        -> Result<Context<OutputData>, LinkError> 
    {
        let value = ctx.get("key")
            .ok_or_else(|| LinkError::InvalidInput("missing key".into()))?;
        
        // Process and return
        Ok(ctx.insert_as("result", value))
    }
}
```

### Documentation
```rust
/// Processes input data and transforms context.
///
/// # Arguments
///
/// * `ctx` - Context containing input data
///
/// # Returns
///
/// Result containing transformed context or error
///
/// # Examples
///
/// ```
/// use codeuchain::core::{Context, Link};
///
/// let ctx = Context::new(serde_json::json!({"numbers": [1, 2, 3]}));
/// let link = SumLink::new();
/// let result = link.call(ctx).await?;
/// assert_eq!(result.get("result"), Some(&6.into()));
/// ```
pub async fn call(&self, ctx: Context<InputData>) -> Result<Context<OutputData>, LinkError> {
    // Implementation
}
```

## Testing Patterns

### Basic Test Structure
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use tokio;

    #[tokio::test]
    async fn test_my_link() {
        // Arrange
        let ctx = Context::new(serde_json::json!({
            "numbers": [1, 2, 3]
        }));
        let link = MyLink::new();
        
        // Act
        let result = link.call(ctx).await.unwrap();
        
        // Assert
        assert_eq!(result.get("result"), Some(&6.into()));
    }
}
```

### Testing Errors
```rust
#[tokio::test]
async fn test_error_handling() {
    let ctx = Context::new(serde_json::json!({"invalid": "data"}));
    let link = RiskyLink::new();
    
    let result = link.call(ctx).await;
    
    assert!(result.is_err());
    match result {
        Err(LinkError::InvalidInput(_)) => (),
        _ => panic!("Expected InvalidInput error"),
    }
}
```

### Property-Based Testing
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_context_insert(key in "[a-z]+", value in any::<i32>()) {
        let ctx = Context::<()>::new(serde_json::json!({}));
        let result = ctx.insert(&key, value.into());
        
        prop_assert_eq!(result.get(&key), Some(&value.into()));
    }
}
```

## Common Patterns

### Creating a Simple Link
```rust
use async_trait::async_trait;
use serde_json::Value;

pub struct MyLink;

#[async_trait]
impl Link for MyLink {
    type Input = Value;
    type Output = Value;
    type Error = LinkError;
    
    async fn call(&self, ctx: Context<Self::Input>) 
        -> Result<Context<Self::Output>, Self::Error> 
    {
        // Get value from context
        let value = ctx.get("input_key")
            .ok_or_else(|| LinkError::InvalidInput("missing key".into()))?;
        
        // Process value
        let result = self.process(value)?;
        
        // Return new context with result
        Ok(ctx.insert("output_key", result))
    }
}

impl MyLink {
    fn process(&self, value: &Value) -> Result<Value, LinkError> {
        // Your processing logic
        Ok(value.clone())
    }
}
```

### Creating a Generic Link
```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputData {
    pub numbers: Vec<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputData {
    pub result: i32,
}

pub struct SumLink;

#[async_trait]
impl Link for SumLink {
    type Input = InputData;
    type Output = OutputData;
    type Error = LinkError;
    
    async fn call(&self, ctx: Context<Self::Input>) 
        -> Result<Context<Self::Output>, Self::Error> 
    {
        let numbers = ctx.get("numbers")
            .and_then(|v| v.as_array())
            .ok_or_else(|| LinkError::InvalidInput("missing numbers".into()))?;
        
        let total: i32 = numbers.iter()
            .filter_map(|v| v.as_i64())
            .map(|n| n as i32)
            .sum();
        
        Ok(ctx.insert_as("result", total.into()))
    }
}
```

### Creating a Chain
```rust
use codeuchain::core::Chain;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple chain
    let chain = Chain::new(vec![
        Box::new(Link1),
        Box::new(Link2),
        Box::new(Link3),
    ]);
    
    // Execute chain
    let initial_ctx = Context::new(serde_json::json!({"input": "data"}));
    let result = chain.execute(initial_ctx).await?;
    
    Ok(())
}
```

### Using Middleware
```rust
use codeuchain::middleware::LoggingMiddleware;

let chain = Chain::builder()
    .add_link(Link1)
    .add_link(Link2)
    .add_middleware(LoggingMiddleware::new())
    .build();
```

## Type Evolution

### Using insert() - Preserves Type
```rust
let ctx: Context<InputData> = Context::new(serde_json::json!({
    "numbers": [1, 2, 3]
}));
// Type is still Context<InputData>
let ctx2 = ctx.insert("extra", "value".into());
```

### Using insert_as() - Changes Type
```rust
let ctx: Context<InputData> = Context::new(serde_json::json!({
    "numbers": [1, 2, 3]
}));
// Type is now Context<OutputData>
let ctx2: Context<OutputData> = ctx.insert_as("result", 6.into());
```

## Async/Await Patterns

### Using Tokio Runtime
```rust
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let link = MyLink::new();
    let ctx = Context::new(serde_json::json!({"data": "test"}));
    
    let result = link.call(ctx).await?;
    
    println!("Result: {:?}", result);
    Ok(())
}
```

### Concurrent Execution
```rust
use futures::future::join_all;

async fn process_concurrent(&self, contexts: Vec<Context<InputData>>) 
    -> Vec<Result<Context<OutputData>, LinkError>> 
{
    let futures: Vec<_> = contexts
        .into_iter()
        .map(|ctx| self.call(ctx))
        .collect();
    
    join_all(futures).await
}
```

## Ownership and Borrowing

### Context Ownership
```rust
// Context takes ownership
let ctx1 = Context::new(serde_json::json!({"key": "value"}));
let ctx2 = ctx1.insert("new_key", "new_value".into());
// ctx1 is moved, can't use it anymore

// Use clone if you need to keep original
let ctx1 = Context::new(serde_json::json!({"key": "value"}));
let ctx2 = ctx1.clone().insert("new_key", "new_value".into());
// Both ctx1 and ctx2 are usable
```

### Borrowing in Links
```rust
impl MyLink {
    // Borrow context immutably
    fn check(&self, ctx: &Context<InputData>) -> bool {
        ctx.get("flag").is_some()
    }
    
    // Take ownership for transformation
    async fn call(&self, ctx: Context<InputData>) 
        -> Result<Context<OutputData>, LinkError> 
    {
        Ok(ctx.insert_as("result", 42.into()))
    }
}
```

## Best Practices

### Error Handling
- Use `Result<T, E>` for operations that can fail
- Use `Option<T>` for values that might not exist
- Use `thiserror` or `anyhow` for error types
- Wrap errors with context using `?` operator

### Memory Management
- Let Rust's ownership system guide your design
- Use `Arc<T>` for shared ownership
- Use `Rc<T>` for single-threaded shared ownership
- Avoid `unsafe` unless absolutely necessary

### Performance
- Use `&str` for borrowed strings, `String` for owned
- Prefer `Vec<T>` over `LinkedList<T>`
- Use `HashMap` for key-value storage
- Profile before optimizing

### Concurrency
- Use `async`/`await` for I/O-bound operations
- Use threads for CPU-bound operations
- Use `Arc<Mutex<T>>` for shared mutable state
- Prefer message passing over shared memory

## Debugging

### Print Debug Information
```rust
let ctx = Context::new(serde_json::json!({"key": "value"}));
println!("Context: {:?}", ctx);
dbg!(&ctx);  // More detailed debug output
```

### Using println! Debugging
```rust
impl MyLink {
    async fn call(&self, ctx: Context<InputData>) 
        -> Result<Context<OutputData>, LinkError> 
    {
        println!("Before: {:?}", ctx);
        let result = self.process(ctx)?;
        println!("After: {:?}", result);
        Ok(result)
    }
}
```

### Using Rust Debugger
```bash
# Install rust-lldb or rust-gdb
rustup component add lldb-preview

# Debug test
rust-lldb target/debug/deps/my_test-*

# In debugger
(lldb) breakpoint set -f src/core/context.rs -l 42
(lldb) run
```

## Common Issues

### Lifetime Errors
```rust
// Won't compile - dangling reference
fn get_value<'a>(ctx: &Context<InputData>) -> &'a str {
    let value = String::from("temp");
    &value  // Error: value doesn't live long enough
}

// Fixed - return owned String
fn get_value(ctx: &Context<InputData>) -> String {
    String::from("temp")
}
```

### Async Trait Issues
```rust
// Use async_trait for async methods in traits
use async_trait::async_trait;

#[async_trait]
pub trait Link {
    async fn call(&self, ctx: Context<Self::Input>) 
        -> Result<Context<Self::Output>, Self::Error>;
}
```

### Serde Issues
```rust
// Ensure types implement Serialize/Deserialize
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MyData {
    pub field: String,
}
```

## Building and Distribution

### Build Release
```bash
cargo build --release
# Binary in target/release/
```

### Publishing to crates.io
```bash
# Update Cargo.toml version
cargo publish --dry-run
cargo publish
```

### Cross-Compilation
```bash
# Install target
rustup target add x86_64-unknown-linux-musl

# Build for target
cargo build --target x86_64-unknown-linux-musl --release
```

## Reference

- **Core Implementation**: `src/core/`
- **Test Examples**: `tests/`
- **Rust Documentation**: https://doc.rust-lang.org/
- **async_trait**: https://docs.rs/async-trait/
- **tokio**: https://docs.rs/tokio/
