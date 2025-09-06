/*!
Chain Components: Reusable Chain Implementations

Concrete implementations of the Chain protocol.
These are the orchestrators that get composed into features.
*/

use codeuchain::core::context::Context;
use codeuchain::core::link::LegacyLink;
use codeuchain::core::middleware::Middleware;
use codeuchain::core::chain::Chain;

/// Loving weaver of links—connects with conditions, runs with selfless execution.
/// A concrete implementation of the Chain protocol.
pub struct BasicChain {
    chain: Chain,
}

impl BasicChain {
    /// Create a new basic chain
    pub fn new() -> Self {
        Self {
            chain: Chain::new(),
        }
    }

    /// With gentle inclusion, store the link.
    pub fn add_link(&mut self, name: String, link: Box<dyn LegacyLink>) {
        self.chain.add_link(name, link);
    }

    /// With compassionate logic, add a connection.
    pub fn connect<F>(&mut self, source: String, target: String, condition: F)
    where
        F: Fn(&Context) -> bool + Send + Sync + 'static,
    {
        self.chain.connect(source, target, condition);
    }

    /// Lovingly attach middleware.
    pub fn use_middleware(&mut self, middleware: Box<dyn Middleware>) {
        self.chain.use_middleware(middleware);
    }

    /// With selfless execution, flow through links.
    pub async fn run(&self, initial_ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        self.chain.run(initial_ctx).await
    }

    /// Get a reference to the underlying chain
    pub fn chain(&self) -> &Chain {
        &self.chain
    }
}

impl Default for BasicChain {
    fn default() -> Self {
        Self::new()
    }
}