pub mod core;
pub mod utils;

// Re-export core types for convenience
pub use core::state::{State, MutableState};
pub use core::link::{Link, LegacyLink};
pub use core::chain::Chain;
pub use core::hook::Hook;

// Re-export common utilities
pub use utils::error_handling::{ErrorHandlingMixin, RetryLink};
pub use utils::timing_hook::{TimingHook, create_csv_timing_hook, create_minimal_timing_hook, create_detailed_timing_hook};