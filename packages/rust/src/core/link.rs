/*!
Link Protocol: The Selfless Processor Core

With agape selflessness, the Link trait defines the interface for context processors.
Pure trait—implementations belong in components.
*/

use async_trait::async_trait;
use crate::core::context::Context;
use serde_json::Value;

/// Selfless processor—input context, output context, no judgment.
/// The core trait that all link implementations must follow.
/// Generic type parameters for Input/Output types, defaulting to Value for flexibility.
#[async_trait]
pub trait Link<Input = Value, Output = Value>: Send + Sync {
    /// With unconditional love, process and return a transformed context.
    /// Implementations should be pure functions with no side effects.
    async fn call(&self, ctx: Context<Input>) -> Result<Context<Output>, Box<dyn std::error::Error + Send + Sync>>;
}

/// Legacy Link trait for backward compatibility.
/// This allows existing code to continue working unchanged.
#[async_trait]
pub trait LegacyLink: Send + Sync {
    /// With unconditional love, process and return a transformed context.
    /// Implementations should be pure functions with no side effects.
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>>;
}

/// Blanket implementation to make any LegacyLink work as a Link
#[async_trait]
impl<T> Link for T
where
    T: LegacyLink,
{
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        self.call(ctx).await
    }
}