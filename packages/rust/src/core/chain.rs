/*!
Chain: The Orchestrator

The Chain orchestrates link execution with conditional flows and hook.
Core implementation that all chain implementations can build upon.
*/

use std::collections::HashMap;
use crate::core::state::State;
use crate::core::link::LegacyLink;
use crate::core::hook::Hook;

/// Loving weaver of links—connects with conditions, runs with selfless execution.
/// Core implementation that provides full chain functionality.
pub struct Chain {
    links: HashMap<String, Box<dyn LegacyLink>>,
    connections: Vec<(String, String, Box<dyn Fn(&State) -> bool + Send + Sync>)>,
    hooks: Vec<Box<dyn Hook>>,
}

impl Chain {
    /// Create a new empty chain
    pub fn new() -> Self {
        Self {
            links: HashMap::new(),
            connections: Vec::new(),
            hooks: Vec::new(),
        }
    }

    /// With gentle inclusion, store the link.
    pub fn add_link(&mut self, name: String, link: Box<dyn LegacyLink>) {
        self.links.insert(name, link);
    }

    /// With compassionate logic, add a connection.
    pub fn connect<F>(&mut self, source: String, target: String, condition: F)
    where
        F: Fn(&State) -> bool + Send + Sync + 'static,
    {
        self.connections.push((source, target, Box::new(condition)));
    }

    /// Lovingly attach hook.
    pub fn use_hook(&mut self, hook: Box<dyn Hook>) {
        self.hooks.push(hook);
    }

    /// With selfless execution, flow through links.
    pub async fn run(&self, initial_ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>> {
        let mut ctx = initial_ctx;

        // Execute hook before hooks
        for mw in &self.hooks {
            mw.before(None, &ctx).await?;
        }

        // Simple linear execution for now
        // TODO: Implement conditional flow execution
        for (_name, link) in &self.links {
            // Execute hook before each link
            for mw in &self.hooks {
                mw.before(Some(link.as_ref()), &ctx).await?;
            }

            // Execute the link
            ctx = link.call(ctx).await?;

            // Execute hook after each link
            for mw in &self.hooks {
                mw.after(Some(link.as_ref()), &ctx).await?;
            }
        }

        // Execute final hook after hooks
        for mw in &self.hooks {
            mw.after(None, &ctx).await?;
        }

        Ok(ctx)
    }

    /// Get a reference to the links
    pub fn links(&self) -> &HashMap<String, Box<dyn LegacyLink>> {
        &self.links
    }

    /// Get a reference to the connections
    pub fn connections(&self) -> &[(String, String, Box<dyn Fn(&State) -> bool + Send + Sync>)] {
        &self.connections
    }

    /// Get a reference to the hooks
    pub fn hooks(&self) -> &[Box<dyn Hook>] {
        &self.hooks
    }
}

impl Default for Chain {
    fn default() -> Self {
        Self::new()
    }
}