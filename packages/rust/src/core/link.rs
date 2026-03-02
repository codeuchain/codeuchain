/*!
Link Protocol: The Processing Unit Core

The Link trait defines the interface for state processors.
Pure trait—implementations belong in components.
*/

use async_trait::async_trait;
use crate::core::state::State;
use serde_json::Value;

/// Selfless processor—input state, output state, no judgment.
/// The core trait that all link implementations must follow.
/// Generic type parameters for Input/Output types, defaulting to Value for flexibility.
#[async_trait]
pub trait Link<Input = Value, Output = Value>: Send + Sync {
    /// With unconditional love, process and return a transformed state.
    /// Implementations should be pure functions with no side effects.
    async fn call(&self, ctx: State<Input>) -> Result<State<Output>, Box<dyn std::error::Error + Send + Sync>>;
}

/// Legacy Link trait for backward compatibility.
/// This allows existing code to continue working unchanged.
#[async_trait]
pub trait LegacyLink: Send + Sync {
    /// With unconditional love, process and return a transformed state.
    /// Implementations should be pure functions with no side effects.
    async fn call(&self, ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>>;
}

/// Blanket implementation to make any LegacyLink work as a Link
#[async_trait]
impl<T> Link for T
where
    T: LegacyLink,
{
    async fn call(&self, ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>> {
        self.call(ctx).await
    }
}