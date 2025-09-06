/*!
Middleware Trait: The Gentle Enhancer Core

With agape gentleness, the Middleware trait defines optional enhancement hooks.
Trait with default implementations—implementations can override any/all methods.
*/

use async_trait::async_trait;
use crate::core::context::Context;
use crate::core::link::LegacyLink;

/// Gentle enhancer—optional hooks with forgiving defaults.
/// Trait that middleware implementations can implement.
/// Implementors can override any combination of before(), after(), and on_error().
#[async_trait]
pub trait Middleware: Send + Sync {
    /// With selfless optionality, do nothing by default.
    async fn before(&self, _link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        Ok(())
    }

    /// Forgiving default.
    async fn after(&self, _link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        Ok(())
    }

    /// Compassionate error handling.
    async fn on_error(&self, _link: Option<&dyn LegacyLink>, _error: &Box<dyn std::error::Error + Send + Sync>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        Ok(())
    }
}