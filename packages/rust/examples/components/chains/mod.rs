/*!
Chain Components: Reusable Chain Implementations

Concrete implementations of the Chain protocol.
These are the orchestrators that get composed into features.
*/

use codeuchain::core::state::State;
use codeuchain::core::link::LegacyLink;
use codeuchain::core::hook::Hook;
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
        F: Fn(&State) -> bool + Send + Sync + 'static,
    {
        self.chain.connect(source, target, condition);
    }

    /// Lovingly attach hook.
    pub fn use_hook(&mut self, hook: Box<dyn Hook>) {
        self.chain.use_hook(hook);
    }

    /// With selfless execution, flow through links.
    pub async fn run(&self, initial_ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>> {
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