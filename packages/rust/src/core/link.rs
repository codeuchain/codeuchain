/*!
Link Protocol: The Selfless Processor Core

With agape selflessness, the Link trait defines the interface for context processors.
Pure trait—implementations belong in components.
*/

use async_trait::async_trait;
use crate::core::context::Context;

/// Selfless processor—input context, output context, no judgment.
/// The core trait that all link implementations must follow.
#[async_trait]
pub trait Link: Send + Sync {
    /// With unconditional love, process and return a transformed context.
    /// Implementations should be pure functions with no side effects.
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>>;
}