/*!
Chain: The Harmonious Connector

With agape harmony, the Chain orchestrates link execution with conditional flows and middleware.
Core implementation that all chain implementations can build upon.
*/

use std::collections::HashMap;
use crate::core::context::Context;
use crate::core::link::Link;
use crate::core::middleware::Middleware;

/// Loving weaver of links—connects with conditions, runs with selfless execution.
/// Core implementation that provides full chain functionality.
pub struct Chain {
    links: HashMap<String, Box<dyn Link>>,
    connections: Vec<(String, String, Box<dyn Fn(&Context) -> bool + Send + Sync>)>,
    middlewares: Vec<Box<dyn Middleware>>,
}

impl Chain {
    /// Create a new empty chain
    pub fn new() -> Self {
        Self {
            links: HashMap::new(),
            connections: Vec::new(),
            middlewares: Vec::new(),
        }
    }

    /// With gentle inclusion, store the link.
    pub fn add_link(&mut self, name: String, link: Box<dyn Link>) {
        self.links.insert(name, link);
    }

    /// With compassionate logic, add a connection.
    pub fn connect<F>(&mut self, source: String, target: String, condition: F)
    where
        F: Fn(&Context) -> bool + Send + Sync + 'static,
    {
        self.connections.push((source, target, Box::new(condition)));
    }

    /// Lovingly attach middleware.
    pub fn use_middleware(&mut self, middleware: Box<dyn Middleware>) {
        self.middlewares.push(middleware);
    }

    /// With selfless execution, flow through links.
    pub async fn run(&self, initial_ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        let mut ctx = initial_ctx;

        // Execute middleware before hooks
        for mw in &self.middlewares {
            mw.before(None, &ctx).await?;
        }

        // Simple linear execution for now
        // TODO: Implement conditional flow execution
        for (_name, link) in &self.links {
            // Execute middleware before each link
            for mw in &self.middlewares {
                mw.before(Some(link.as_ref()), &ctx).await?;
            }

            // Execute the link
            ctx = link.call(ctx).await?;

            // Execute middleware after each link
            for mw in &self.middlewares {
                mw.after(Some(link.as_ref()), &ctx).await?;
            }
        }

        // Execute final middleware after hooks
        for mw in &self.middlewares {
            mw.after(None, &ctx).await?;
        }

        Ok(ctx)
    }

    /// Get a reference to the links
    pub fn links(&self) -> &HashMap<String, Box<dyn Link>> {
        &self.links
    }

    /// Get a reference to the connections
    pub fn connections(&self) -> &[(String, String, Box<dyn Fn(&Context) -> bool + Send + Sync>)] {
        &self.connections
    }

    /// Get a reference to the middlewares
    pub fn middlewares(&self) -> &[Box<dyn Middleware>] {
        &self.middlewares
    }
}

impl Default for Chain {
    fn default() -> Self {
        Self::new()
    }
}