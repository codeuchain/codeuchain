/*!
Utils Module: Shared Utilities

Common utilities and helpers for the CodeUChain ecosystem.
*/

pub mod error_handling;
pub mod timing_hook;

// Re-export for convenience
pub use error_handling::{ErrorHandlingMixin, RetryLink};
pub use timing_hook::TimingHook;