"""
Error Handling: The Resilience Layer

Handle errors comprehensively, with retry logic and proper error propagation.
Optimized for Python—exceptions, retries, ecosystem integrations.
"""

from typing import Callable, Optional, List, Tuple
from codeuchain.core.context import Context
from codeuchain.core.link import Link

__all__ = ["ErrorHandlingMixin", "RetryLink"]


class ErrorHandlingMixin:
    """
    Mixin for chains to handle errors with forgiveness.
    """

    def __init__(self):
        self.error_connections: List[Tuple[str, str, Callable[[Exception], bool]]] = []

    def on_error(self, source: str, handler: str, condition: Callable[[Exception], bool]) -> None:
        """With gentle care, add error routing."""
        self.error_connections.append((source, handler, condition))

    async def _handle_error(self, link_name: str, error: Exception, ctx: Context) -> Optional[Context]:
        """Compassionately find and call error handler."""
        for src, hdl, cond in self.error_connections:
            if src == link_name and cond(error):
                handler = getattr(self, 'links', {}).get(hdl)
                if handler and hasattr(handler, 'call'):
                    return await handler.call(ctx.insert("error", str(error)))
        return None


class RetryLink(Link):
    """Retry with resilience—comprehensive error recovery in action."""

    def __init__(self, inner_link: Link, max_retries: int = 3):
        self.inner = inner_link
        self.max_retries = max_retries

    async def call(self, ctx: Context) -> Context:
        if self.max_retries == 0:
            # If no retries allowed, try once and handle failure
            try:
                return await self.inner.call(ctx)
            except Exception as e:
                return ctx.insert("error", f"Max retries: {e}")
        
        for attempt in range(self.max_retries):
            try:
                return await self.inner.call(ctx)
            except Exception as e:
                if attempt == self.max_retries - 1:
                    return ctx.insert("error", f"Max retries: {e}")
        return ctx