"""
Middleware Components: Reusable Middleware Implementations

Concrete implementations of the Middleware protocol.
These are the utilities that get swapped between projects.
"""

from typing import Optional
from codeuchain.core.context import Context
from codeuchain.core.link import Link
from codeuchain.core.middleware import Middleware

__all__ = ["LoggingMiddleware", "TimingMiddleware", "BeforeOnlyMiddleware"]


class BeforeOnlyMiddleware(Middleware):
    """Example middleware that only implements before - demonstrates flexibility."""

    async def before(self, link: Optional[Link], ctx: Context) -> None:
        print(f"🚀 Starting execution with context: {ctx}")

    # after and on_error use default implementations (do nothing)


class LoggingMiddleware(Middleware):
    """Logging with ecosystem integration."""

    async def before(self, link: Optional[Link], ctx: Context) -> None:
        print(f"Before link {link}: {ctx}")

    async def after(self, link: Optional[Link], ctx: Context) -> None:
        print(f"After link {link}: {ctx}")

    # on_error is not implemented - uses default (does nothing)


class TimingMiddleware(Middleware):
    """Timing for performance observation."""

    def __init__(self):
        self.start_times = {}

    async def before(self, link: Optional[Link], ctx: Context) -> None:
        import time
        if link:
            self.start_times[id(link)] = time.time()

    async def after(self, link: Optional[Link], ctx: Context) -> None:
        import time
        if link and id(link) in self.start_times:
            duration = time.time() - self.start_times[id(link)]
            print(f"Link {link} took {duration:.2f}s")
            del self.start_times[id(link)]

    async def on_error(self, link: Optional[Link], error: Exception, ctx: Context) -> None:
        import time
        if link and id(link) in self.start_times:
            duration = time.time() - self.start_times[id(link)]
            print(f"Error in link {link} after {duration:.2f}s: {error}")
            del self.start_times[id(link)]