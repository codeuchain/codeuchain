/*!
Components Module: Example Implementations

Concrete implementations of core protocols.
These are examples that can be used as-is or as templates.
*/

pub mod links;
pub mod chains;
pub mod middleware;

// Re-export for convenience
pub use links::MathLink;
pub use chains::BasicChain;
pub use middleware::LoggingMiddleware;