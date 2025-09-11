/*!
Context: The Data Container

The Context holds data carefully, immutable by default for safety, mutable for flexibility.
Optimized for Rust's ownership model—embracing HashMap with serde integration.
*/

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

/// Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
/// Generic type parameter T represents the current data shape, defaulting to serde_json::Value for flexibility.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Context<T = Value> {
    data: HashMap<String, Value>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> Context<T> {
    /// Create a new context with optional initial data
    pub fn new(data: HashMap<String, Value>) -> Self {
        Self {
            data,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Create an empty context
    pub fn empty() -> Self {
        Self {
            data: HashMap::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    /// With gentle care, return the value or None, forgiving absence.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    /// With selfless safety, return a fresh context with the addition (preserves type).
    pub fn insert(self, key: String, value: Value) -> Context<T> {
        let mut new_data = self.data;
        new_data.insert(key, value);
        Context {
            data: new_data,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Type evolution: Transform to a new type while preserving data.
    pub fn insert_as<U>(self, key: String, value: Value) -> Context<U> {
        let mut new_data = self.data;
        new_data.insert(key, value);
        Context {
            data: new_data,
            _phantom: std::marker::PhantomData,
        }
    }

    /// For those needing change, provide a mutable sibling.
    pub fn with_mutation(&self) -> MutableContext {
        MutableContext {
            data: self.data.clone(),
        }
    }

    /// Lovingly combine contexts, favoring the other with compassion.
    pub fn merge(mut self, other: &Context<T>) -> Context<T> {
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

impl Context<Value> {
    /// Create context from HashMap (for backward compatibility)
    pub fn from_hashmap(data: HashMap<String, Value>) -> Self {
        Self::new(data)
    }
}

impl<T> Default for Context<T> {
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
    pub fn to_immutable<T>(self) -> Context<T> {
        Context {
            data: self.data,
            _phantom: std::marker::PhantomData,
        }
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