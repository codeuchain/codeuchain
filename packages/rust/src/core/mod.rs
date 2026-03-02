/*!
Core Module: Base Protocols and Traits

The foundation that AI maintains and humans rarely touch.
Contains traits, structs, and fundamental types.
*/

pub mod state;
pub mod link;
pub mod chain;
pub mod hook;

// Re-export for convenience
pub use state::{State, MutableState};
pub use link::Link;
pub use chain::Chain;
pub use hook::Hook;