/*!
Hook Components: Reusable Hook Implementations

Concrete implementations of the Hook trait.
These are the utilities that get swapped between projects.
*/

use async_trait::async_trait;
use codeuchain::core::state::State;
use codeuchain::core::link::LegacyLink;
use codeuchain::core::hook::Hook;

/// Example hook that only implements before - demonstrates flexibility.
pub struct BeforeOnlyHook;

impl BeforeOnlyHook {
    /// Create a new before-only hook
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Hook for BeforeOnlyHook {
    async fn before(&self, _link: Option<&dyn LegacyLink>, ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("🚀 Starting execution with state: {:?}", ctx);
        Ok(())
    }
    // after and on_error use default implementations (do nothing)
}

/// Logging with ecosystem integration.
pub struct LoggingHook;

impl LoggingHook {
    /// Create a new logging hook
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl Hook for LoggingHook {
    async fn before(&self, link: Option<&dyn LegacyLink>, ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("Before link {:?}: {:?}", link.map(|_| "Link"), ctx);
        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        println!("After link {:?}: {:?}", link.map(|_| "Link"), ctx);
        Ok(())
    }

    // on_error is not implemented - uses default (does nothing)
}

/// Timing for performance observation.
pub struct TimingHook {
    start_times: std::collections::HashMap<String, std::time::Instant>,
}

impl TimingHook {
    /// Create a new timing hook
    pub fn new() -> Self {
        Self {
            start_times: std::collections::HashMap::new(),
        }
    }
}

#[async_trait]
impl Hook for TimingHook {
    async fn before(&self, _link: Option<&dyn LegacyLink>, _ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // For simplicity, we'll just track timing without unique IDs
        // In a real implementation, you might want to use TypeId or similar
        Ok(())
    }

    async fn after(&self, link: Option<&dyn LegacyLink>, _ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Simplified timing - just print that the link completed
        if link.is_some() {
            println!("Link completed");
        }
        Ok(())
    }

    async fn on_error(&self, link: Option<&dyn LegacyLink>, error: &Box<dyn std::error::Error + Send + Sync>, _ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if link.is_some() {
            println!("Error in link: {}", error);
        }
        Ok(())
    }
}