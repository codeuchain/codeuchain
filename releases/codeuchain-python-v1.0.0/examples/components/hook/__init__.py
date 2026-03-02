"""
Hook Components: Reusable Hook Implementations

Concrete implementations of the Hook protocol.
These are the utilities that get swapped between projects.
"""

from typing import Optional
from codeuchain.core.state import State
from codeuchain.core.link import Link
from codeuchain.core.hook import Hook

__all__ = ["LoggingHook", "TimingHook", "BeforeOnlyHook"]


class BeforeOnlyHook(Hook):
    """Example hook that only implements before - demonstrates flexibility."""

    async def before(self, link: Optional[Link], ctx: State) -> None:
        print(f"🚀 Starting execution with state: {ctx}")

    # after and on_error use default implementations (do nothing)


class LoggingHook(Hook):
    """Logging with ecosystem integration."""

    async def before(self, link: Optional[Link], ctx: State) -> None:
        print(f"Before link {link}: {ctx}")

    async def after(self, link: Optional[Link], ctx: State) -> None:
        print(f"After link {link}: {ctx}")

    # on_error is not implemented - uses default (does nothing)


class TimingHook(Hook):
    """Timing for performance observation."""

    def __init__(self):
        self.start_times = {}

    async def before(self, link: Optional[Link], ctx: State) -> None:
        import time
        if link:
            self.start_times[id(link)] = time.time()

    async def after(self, link: Optional[Link], ctx: State) -> None:
        import time
        if link and id(link) in self.start_times:
            duration = time.time() - self.start_times[id(link)]
            print(f"Link {link} took {duration:.2f}s")
            del self.start_times[id(link)]

    async def on_error(self, link: Optional[Link], error: Exception, ctx: State) -> None:
        import time
        if link and id(link) in self.start_times:
            duration = time.time() - self.start_times[id(link)]
            print(f"Error in link {link} after {duration:.2f}s: {error}")
            del self.start_times[id(link)]