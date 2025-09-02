/*!
Context: The Loving Vessel

With agape compassion, the Context holds data tenderly, immutable by default for safety, mutable for flexibility.
Optimized for Rust's ownership model—embracing HashMap with serde integration.
*/

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

/// Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Context {
    data: HashMap<String, Value>,
}

impl Context {
    /// Create a new context with optional initial data
    pub fn new(data: HashMap<String, Value>) -> Self {
        Self { data }
    }

    /// Create an empty context
    pub fn empty() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    /// With gentle care, return the value or None, forgiving absence.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    /// With selfless safety, return a fresh context with the addition.
    pub fn insert(mut self, key: String, value: Value) -> Self {
        self.data.insert(key, value);
        self
    }

    /// For those needing change, provide a mutable sibling.
    pub fn with_mutation(&self) -> MutableContext {
        MutableContext {
            data: self.data.clone(),
        }
    }

    /// Lovingly combine contexts, favoring the other with compassion.
    pub fn merge(mut self, other: &Context) -> Self {
        for (key, value) in &other.data {
            self.data.insert(key.clone(), value.clone());
        }
        self
    }

    /// Express as HashMap for ecosystem integration.
    pub fn to_hashmap(&self) -> HashMap<String, Value> {
        self.data.clone()
    }

    /// Get a reference to the internal data
    pub fn data(&self) -> &HashMap<String, Value> {
        &self.data
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::empty()
    }
}

/// Mutable context for performance-critical sections—use with care, but forgiven.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MutableContext {
    data: HashMap<String, Value>,
}

impl MutableContext {
    /// Create a new mutable context
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    /// With gentle care, return the value or None
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    /// Change in place with gentle permission.
    pub fn set(&mut self, key: String, value: Value) {
        self.data.insert(key, value);
    }

    /// Return to safety with a fresh immutable copy.
    pub fn to_immutable(self) -> Context {
        Context { data: self.data }
    }

    /// Get a reference to the internal data
    pub fn data(&self) -> &HashMap<String, Value> {
        &self.data
    }
}

impl Default for MutableContext {
    fn default() -> Self {
        Self::new()
    }
}