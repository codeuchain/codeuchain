/*!
Core Module: Base Protocols and Traits

The foundation that AI maintains and humans rarely touch.
Contains traits, structs, and fundamental types.
*/

pub mod context;
pub mod link;
pub mod chain;
pub mod middleware;

// Re-export for convenience
pub use context::{Context, MutableContext};
pub use link::Link;
pub use chain::Chain;
pub use middleware::Middleware;