/*!
Middleware Components: Reusable Middleware Implementations

Concrete implementations of the Middleware trait.
These are the utilities that get swapped between projects.
*/

use async_trait::async_trait;
use codeuchain::core::context::Context;
use codeuchain::core::link::LegacyLink;
use codeuchain::core::middleware::Middleware;

/// Example middleware that only implements before - demonstrates flexibility.
pub struct BeforeOnlyMiddleware;

impl BeforeOnlyMiddleware {
    /// Create a new before-only middleware
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Middleware for BeforeOnlyMiddleware {
    async fn before(&self, _link: Option<&dyn LegacyLink>, ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("🚀 Starting execution with context: {:?}", ctx);
        Ok(())
    }
    // after and on_error use default implementations (do nothing)
}

/// Logging with ecosystem integration.
pub struct LoggingMiddleware;

impl LoggingMiddleware {
    /// Create a new logging middleware
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Middleware for LoggingMiddleware {
    async fn before(&self, link: Option<&dyn LegacyLink>, ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("Before link {:?}: {:?}", link.map(|_| "Link"), ctx);
        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("After link {:?}: {:?}", link.map(|_| "Link"), ctx);
        Ok(())
    }

    // on_error is not implemented - uses default (does nothing)
}

/// Timing for performance observation.
pub struct TimingMiddleware {
    start_times: std::collections::HashMap<String, std::time::Instant>,
}

impl TimingMiddleware {
    /// Create a new timing middleware
    pub fn new() -> Self {
        Self {
            start_times: std::collections::HashMap::new(),
        }
    }
}

#[async_trait]
impl Middleware for TimingMiddleware {
    async fn before(&self, _link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // For simplicity, we'll just track timing without unique IDs
        // In a real implementation, you might want to use TypeId or similar
        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Simplified timing - just print that the link completed
        if link.is_some() {
            println!("Link completed");
        }
        Ok(())
    }

    async fn on_error(&self, link: Option<&dyn LegacyLink>, error: &Box<dyn std::error::Error + Send + Sync>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if link.is_some() {
            println!("Error in link: {}", error);
        }
        Ok(())
    }
}