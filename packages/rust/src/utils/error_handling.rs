/*!
Error Handling: The Forgiving Guardian

With agape forgiveness, handle errors compassionately, routing with love.
Optimized for Rust—Result types, retries, ecosystem integrations.
*/

use async_trait::async_trait;
use crate::core::context::Context;
use crate::core::link::Link;

/// Mixin for chains to handle errors with forgiveness.
pub struct ErrorHandlingMixin {
    error_connections: Vec<(String, String, Box<dyn Fn(&Box<dyn std::error::Error + Send + Sync>) -> bool + Send + Sync>)>,
}

impl ErrorHandlingMixin {
    /// Create a new error handling mixin
    pub fn new() -> Self {
        Self {
            error_connections: Vec::new(),
        }
    }

    /// With gentle care, add error routing.
    pub fn on_error<F>(&mut self, source: String, handler: String, condition: F)
    where
        F: Fn(&Box<dyn std::error::Error + Send + Sync>) -> bool + Send + Sync + 'static,
    {
        self.error_connections.push((source, handler, Box::new(condition)));
    }

    /// Compassionately find and call error handler.
    pub async fn handle_error(
        &self,
        link_name: &str,
        error: &Box<dyn std::error::Error + Send + Sync>,
        ctx: Context,
        links: &std::collections::HashMap<String, Box<dyn Link>>,
    ) -> Result<Option<Context>, Box<dyn std::error::Error + Send + Sync>> {
        for (src, hdl, cond) in &self.error_connections {
            if src == link_name && cond(error) {
                if let Some(handler) = links.get(hdl) {
                    let ctx_with_error = ctx.insert(
                        "error".to_string(),
                        serde_json::json!(error.to_string()),
                    );
                    return Ok(Some(handler.call(ctx_with_error).await?));
                }
            }
        }
        Ok(None)
    }
}

impl Default for ErrorHandlingMixin {
    fn default() -> Self {
        Self::new()
    }
}

/// Retry with patience—agape's forgiveness in action.
pub struct RetryLink<L: Link> {
    inner: L,
    max_retries: usize,
}

impl<L: Link> RetryLink<L> {
    /// Create a new retry link
    pub fn new(inner: L, max_retries: usize) -> Self {
        Self { inner, max_retries }
    }
}

#[async_trait]
impl<L: Link> Link for RetryLink<L> {
    async fn call(&self, ctx: Context) -> Result<Context, Box<dyn std::error::Error + Send + Sync>> {
        let mut last_error: Option<Box<dyn std::error::Error + Send + Sync>> = None;

        for attempt in 0..=self.max_retries {
            match self.inner.call(ctx.clone()).await {
                Ok(result) => return Ok(result),
                Err(e) => {
                    last_error = Some(e);
                    if attempt == self.max_retries {
                        break;
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| "Max retries exceeded".into()))
    }
}