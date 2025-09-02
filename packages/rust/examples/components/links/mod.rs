/*!
Link Components: Reusable Link Implementations

Concrete implementations of the Link trait.
These are the building blocks that get swapped between projects.
*/

use async_trait::async_trait;
use codeuchain::core::context::Context;
use codeuchain::core::link::Link;
use serde_json::Value;

/// Forgiving link that does nothing—pure love.
pub struct IdentityLink;

impl IdentityLink {
    /// Create a new identity link
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Link for IdentityLink {
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        Ok(ctx)
    }
}

/// Math-focused link
pub struct MathLink {
    operation: String,
}

impl MathLink {
    /// Create a new math link
    pub fn new(operation: String) -> Self {
        Self { operation }
    }
}

#[async_trait]
impl Link for MathLink {
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        if let Some(Value::Array(numbers)) = ctx.get("numbers") {
            let numbers: Vec<f64> = numbers
                .iter()
                .filter_map(|v| v.as_f64())
                .collect();

            if numbers.is_empty() {
                return Ok(ctx.insert("error".to_string(), Value::String("Invalid numbers".to_string())));
            }

            let result = match self.operation.as_str() {
                "sum" => numbers.iter().sum(),
                "mean" => numbers.iter().sum::<f64>() / numbers.len() as f64,
                "max" => numbers.iter().fold(f64::NEG_INFINITY, |a, &b| a.max(b)),
                "min" => numbers.iter().fold(f64::INFINITY, |a, &b| a.min(b)),
                _ => 0.0,
            };

            Ok(ctx.insert("result".to_string(), Value::Number(serde_json::Number::from_f64(result).unwrap())))
        } else {
            Ok(ctx.insert("error".to_string(), Value::String("Invalid numbers".to_string())))
        }
    }
}