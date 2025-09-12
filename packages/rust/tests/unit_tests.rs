/*!
Unit Tests: Core Functionality

Testing the fundamental building blocks of CodeUChain.
*/

use codeuchain::core::context::{Context, MutableContext};
use codeuchain::core::link::LegacyLink;
use codeuchain::core::chain::Chain;
use codeuchain::core::middleware::Middleware;
use codeuchain::utils::TimingMiddleware;
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
        async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
            Ok(ctx.insert("result".to_string(), self.result.clone()))
        }
    }

    // Mock middleware for testing
    struct MockMiddleware {
        pub before_called: std::sync::Mutex<bool>,
        pub after_called: std::sync::Mutex<bool>,
    }

    impl MockMiddleware {
        fn new() -> Self {
            Self {
                before_called: std::sync::Mutex::new(false),
                after_called: std::sync::Mutex::new(false),
            }
        }
    }

    #[async_trait]
    impl Middleware for MockMiddleware {
        async fn before(&self, _link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.before_called.lock().unwrap() = true;
            Ok(())
        }

        async fn after(&self, _link: Option<&dyn LegacyLink>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.after_called.lock().unwrap() = true;
            Ok(())
        }
    }

    #[tokio::test]
    async fn test_context_operations() {
        let mut data = HashMap::new();
        data.insert("key".to_string(), Value::String("value".to_string()));
        let ctx: Context<Value> = Context::new(data);

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
        let other_ctx: Context<Value> = Context::new(other_data);
        let merged = new_ctx.merge(&other_ctx);
        assert_eq!(merged.get("other_key"), Some(&Value::Bool(true)));
        assert_eq!(merged.get("key"), Some(&Value::String("value".to_string())));
    }

    #[tokio::test]
    async fn test_mutable_context() {
        let mut mutable_ctx = MutableContext::new();

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

        let ctx = Context::empty();
        let result = chain.run(ctx).await.unwrap();

        assert_eq!(result.get("result"), Some(&Value::String("test_result".to_string())));
    }

    #[tokio::test]
    async fn test_middleware_execution() {
        let mut chain = Chain::new();
        let mock_link = MockLink::new(Value::String("test".to_string()));
        let mock_middleware = MockMiddleware::new();

        chain.add_link("test".to_string(), Box::new(mock_link));
        chain.use_middleware(Box::new(mock_middleware));

        let ctx = Context::empty();
        let _result = chain.run(ctx).await.unwrap();

        // Note: This test would need to be adjusted to properly test middleware
        // since we're using Box<dyn Middleware> which makes it hard to inspect state
    }

    #[tokio::test]
    async fn test_link_call() {
        let link = MockLink::new(Value::Number(serde_json::Number::from_f64(123.0).unwrap()));
        let ctx = Context::empty();

        let result = LegacyLink::call(&link, ctx).await.unwrap();
        assert_eq!(result.get("result"), Some(&Value::Number(serde_json::Number::from_f64(123.0).unwrap())));
    }

    // New tests for typed features

    #[tokio::test]
    async fn test_type_evolution() {
        // Test insert_as for type evolution
        let ctx = Context::<Value>::empty();
        let evolved_ctx = ctx.insert_as::<String>("result".to_string(), Value::Number(serde_json::Number::from_f64(6.0).unwrap()));

        // The evolved context should contain the inserted value
        assert_eq!(evolved_ctx.get("result"), Some(&Value::Number(serde_json::Number::from_f64(6.0).unwrap())));
    }

    #[tokio::test]
    async fn test_generic_context_creation() {
        // Test creating contexts with different generic types
        let ctx1: Context<Value> = Context::empty();
        let ctx2: Context<String> = Context::empty();

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

        let untyped_ctx = Context::from_hashmap(data);
        let result = untyped_ctx.insert("result".to_string(), Value::Number(serde_json::Number::from_f64(6.0).unwrap()));

        assert_eq!(result.get("result"), Some(&Value::Number(serde_json::Number::from_f64(6.0).unwrap())));
        assert!(result.get("numbers").unwrap().is_array());
    }

    // Test for generic link (placeholder - would need a concrete generic link implementation)
    #[tokio::test]
    async fn test_generic_link_interface() {
        // This test verifies that the Link trait can be used with generics
        // For now, just test that we can create a generic context
        let ctx = Context::<Value>::empty();
        assert!(ctx.data().is_empty());
    }

    #[tokio::test]
    async fn test_timing_middleware() {
        let mut chain = Chain::new();
        let mock_link = MockLink::new(Value::String("timed_test".to_string()));
        let timing = TimingMiddleware::with_config(false, false); // Disable auto_print to prevent hanging

        chain.add_link("timed_link".to_string(), Box::new(mock_link));
        chain.use_middleware(Box::new(timing));

        let ctx = Context::empty();
        let result = chain.run(ctx).await.unwrap();

        assert_eq!(result.get("result"), Some(&Value::String("timed_test".to_string())));
    }

    #[tokio::test]
    async fn test_timing_middleware_isolated() {
        let timing = TimingMiddleware::with_config(false, false);
        let ctx = Context::empty();
        let mock_link = MockLink::new(Value::String("test".to_string()));

        // Test before hook
        timing.before(Some(&mock_link), &ctx).await.unwrap();
        
        // Test after hook
        timing.after(Some(&mock_link), &ctx).await.unwrap();
        
        // Check that we have timing data
        let stats = timing.get_stats();
        assert!(!stats.is_empty());
        
        // Check that we have the expected link (using the trait type name as used by middleware)
        let expected_key = "dyn codeuchain::core::link::LegacyLink";
        assert!(stats.contains_key(expected_key));
        
        // Check that the timing values are reasonable (should be small but non-zero)
        let (total_ms, calls, avg_ms) = stats[expected_key];
        assert!(total_ms >= 0.0);
        assert_eq!(calls, 1);
        assert_eq!(avg_ms, total_ms);
    }

    #[tokio::test]
    async fn test_timing_middleware_auto_print() {
        let timing = TimingMiddleware::with_config(false, true); // Enable auto_print
        let ctx = Context::empty();
        let mock_link = MockLink::new(Value::String("test".to_string()));

        // Test before hook
        timing.before(Some(&mock_link), &ctx).await.unwrap();
        
        // Test after hook
        timing.after(Some(&mock_link), &ctx).await.unwrap();
        
        // Test chain completion (this should trigger auto_print)
        timing.after(None, &ctx).await.unwrap();
        
        // Check that we have timing data
        let stats = timing.get_stats();
        assert!(!stats.is_empty());
    }
}