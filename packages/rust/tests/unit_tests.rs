/*!
Unit Tests: Core Functionality

Testing the fundamental building blocks of CodeUChain.
*/

use codeuchain::core::context::{Context, MutableContext};
use codeuchain::core::link::Link;
use codeuchain::core::chain::Chain;
use codeuchain::core::middleware::Middleware;
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
    impl Link for MockLink {
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
        async fn before(&self, _link: Option<&dyn Link>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.before_called.lock().unwrap() = true;
            Ok(())
        }

        async fn after(&self, _link: Option<&dyn Link>, _ctx: &Context) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
            *self.after_called.lock().unwrap() = true;
            Ok(())
        }
    }

    #[tokio::test]
    async fn test_context_operations() {
        let mut data = HashMap::new();
        data.insert("key".to_string(), Value::String("value".to_string()));
        let ctx = Context::new(data);

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
        let other_ctx = Context::new(other_data);
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
        let immutable = mutable_ctx.to_immutable();
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
        let link = MockLink::new(Value::Number(123.into()));
        let ctx = Context::empty();

        let result = link.call(ctx).await.unwrap();
        assert_eq!(result.get("result"), Some(&Value::Number(123.into())));
    }
}