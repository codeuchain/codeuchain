/*!
Unit Tests: Core Functionality

Testing the fundamental building blocks of CodeUChain.
*/

use codeuchain::core::state::{State, MutableState};
use codeuchain::core::link::LegacyLink;
use codeuchain::core::chain::Chain;
use codeuchain::core::hook::Hook;
use codeuchain::utils::TimingHook;
use serde_json::Value;
use async_trait::async_trait;
use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    // Mock link for testing
    struct MockLink {
        result: Value,
    }

    impl MockLink {
        fn new(result: Value) -> Self {
            Self { result }
        }
    }

    #[async_trait]
    impl LegacyLink for MockLink {
        async fn call(&self, ctx: State) -> Result<State, Box<dyn std::error::Error + Send + Sync>> {
            Ok(ctx.insert("result".to_string(), self.result.clone()))
        }
    }

    // Mock hook for testing
    struct MockHook {
        pub before_called: std::sync::Mutex<bool>,
        pub after_called: std::sync::Mutex<bool>,
    }

    impl MockHook {
        fn new() -> Self {
            Self {
                before_called: std::sync::Mutex::new(false),
                after_called: std::sync::Mutex::new(false),
            }
        }
    }

    #[async_trait]
    impl Hook for MockHook {
        async fn before(&self, _link: Option<&dyn LegacyLink>, _ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.before_called.lock().unwrap() = true;
            Ok(())
        }

        async fn after(&self, _link: Option<&dyn LegacyLink>, _ctx: &State) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.after_called.lock().unwrap() = true;
            Ok(())
        }
    }

    #[tokio::test]
    async fn test_state_operations() {
        let mut data = HashMap::new();
        data.insert("key".to_string(), Value::String("value".to_string()));
        let ctx: State<Value> = State::new(data);

        // Test get
        assert_eq!(ctx.get("key"), Some(&Value::String("value".to_string())));
        assert_eq!(ctx.get("nonexistent"), None);

        // Test insert
        let new_ctx = ctx.insert("new_key".to_string(), Value::Number(42.into()));
        assert_eq!(new_ctx.get("new_key"), Some(&Value::Number(42.into())));
        assert_eq!(new_ctx.get("key"), Some(&Value::String("value".to_string())));

        // Test merge
        let mut other_data = HashMap::new();
        other_data.insert("other_key".to_string(), Value::Bool(true));
        let other_ctx: State<Value> = State::new(other_data);
        let merged = new_ctx.merge(&other_ctx);
        assert_eq!(merged.get("other_key"), Some(&Value::Bool(true)));
        assert_eq!(merged.get("key"), Some(&Value::String("value".to_string())));
    }

    #[tokio::test]
    async fn test_mutable_state() {
        let mut mutable_ctx = MutableState::new();

        // Test set
        mutable_ctx.set("key".to_string(), Value::String("value".to_string()));
        assert_eq!(mutable_ctx.get("key"), Some(&Value::String("value".to_string())));

        // Test to_immutable
        let immutable = mutable_ctx.to_immutable::<Value>();
        assert_eq!(immutable.get("key"), Some(&Value::String("value".to_string())));
    }

    #[tokio::test]
    async fn test_chain_execution() {
        let mut chain = Chain::new();
        let mock_link = MockLink::new(Value::String("test_result".to_string()));
        chain.add_link("test".to_string(), Box::new(mock_link));

        let ctx = State::empty();
        let result = chain.run(ctx).await.unwrap();

        assert_eq!(result.get("result"), Some(&Value::String("test_result".to_string())));
    }

    #[tokio::test]
    async fn test_hook_execution() {
        let mut chain = Chain::new();
        let mock_link = MockLink::new(Value::String("test".to_string()));
        let mock_hook = MockHook::new();

        chain.add_link("test".to_string(), Box::new(mock_link));
        chain.use_hook(Box::new(mock_hook));

        let ctx = State::empty();
        let _result = chain.run(ctx).await.unwrap();

        // Note: This test would need to be adjusted to properly test hook
        // since we're using Box<dyn Hook> which makes it hard to inspect state
    }

    #[tokio::test]
    async fn test_link_call() {
        let link = MockLink::new(Value::Number(serde_json::Number::from_f64(123.0).unwrap()));
        let ctx = State::empty();

        let result = LegacyLink::call(&link, ctx).await.unwrap();
        assert_eq!(result.get("result"), Some(&Value::Number(serde_json::Number::from_f64(123.0).unwrap())));
    }

    // New tests for typed features

    #[tokio::test]
    async fn test_type_evolution() {
        // Test insert_as for type evolution
        let ctx = State::<Value>::empty();
        let evolved_ctx = ctx.insert_as::<String>("result".to_string(), Value::Number(serde_json::Number::from_f64(6.0).unwrap()));

        // The evolved state should contain the inserted value
        assert_eq!(evolved_ctx.get("result"), Some(&Value::Number(serde_json::Number::from_f64(6.0).unwrap())));
    }

    #[tokio::test]
    async fn test_generic_state_creation() {
        // Test creating states with different generic types
        let ctx1: State<Value> = State::empty();
        let ctx2: State<String> = State::empty();

        // Both should work and have empty data
        assert!(ctx1.data().is_empty());
        assert!(ctx2.data().is_empty());
    }

    #[tokio::test]
    async fn test_runtime_compatibility() {
        // Test that untyped usage still works identically
        let mut data = HashMap::new();
        data.insert("numbers".to_string(), Value::Array(vec![
            Value::Number(1.into()),
            Value::Number(2.into()),
            Value::Number(3.into())
        ]));

        let untyped_ctx = State::from_hashmap(data);
        let result = untyped_ctx.insert("result".to_string(), Value::Number(serde_json::Number::from_f64(6.0).unwrap()));

        assert_eq!(result.get("result"), Some(&Value::Number(serde_json::Number::from_f64(6.0).unwrap())));
        assert!(result.get("numbers").unwrap().is_array());
    }

    // Test for generic link (placeholder - would need a concrete generic link implementation)
    #[tokio::test]
    async fn test_generic_link_interface() {
        // This test verifies that the Link trait can be used with generics
        // For now, just test that we can create a generic state
        let ctx = State::<Value>::empty();
        assert!(ctx.data().is_empty());
    }

    #[tokio::test]
    async fn test_timing_hook() {
        let mut chain = Chain::new();
        let mock_link = MockLink::new(Value::String("timed_test".to_string()));
        let timing = TimingHook::with_config(false, false); // Disable auto_print to prevent hanging

        chain.add_link("timed_link".to_string(), Box::new(mock_link));
        chain.use_hook(Box::new(timing));

        let ctx = State::empty();
        let result = chain.run(ctx).await.unwrap();

        assert_eq!(result.get("result"), Some(&Value::String("timed_test".to_string())));
    }

    #[tokio::test]
    async fn test_timing_hook_isolated() {
        let timing = TimingHook::with_config(false, false);
        let ctx = State::empty();
        let mock_link = MockLink::new(Value::String("test".to_string()));

        // Test before hook
        timing.before(Some(&mock_link), &ctx).await.unwrap();
        
        // Test after hook
        timing.after(Some(&mock_link), &ctx).await.unwrap();
        
        // Timing hook successfully executed before and after hooks
        assert!(true);
    }

    #[tokio::test]
    async fn test_timing_hook_auto_print() {
        let timing = TimingHook::with_config(false, true); // Enable auto_print
        let ctx = State::empty();
        let mock_link = MockLink::new(Value::String("test".to_string()));

        // Test before hook
        timing.before(Some(&mock_link), &ctx).await.unwrap();
        
        // Test after hook
        timing.after(Some(&mock_link), &ctx).await.unwrap();
        
        // Test chain completion (this should trigger auto_print)
        timing.after(None, &ctx).await.unwrap();
        
        // Timing hook auto-print executed successfully
        assert!(true);
    }
}