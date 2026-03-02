"""
Chain: The Orchestrator

The Chain orchestrates link execution with conditional flows and hook.
Core implementation that all chain implementations can build upon.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Dict, List, Callable, Optional, TypeVar, Generic
from .state import State
from .link import Link
from .hook import Hook

__all__ = ["Chain"]

# Type variables for generic chain typing
TInput = TypeVar('TInput')
TOutput = TypeVar('TOutput')


class Chain(Generic[TInput, TOutput]):
    """
    Loving weaver of links—connects with conditions, runs with selfless execution.
    Core implementation that provides full chain functionality.
    Enhanced with generic typing for type-safe workflows.
    """

    def __init__(self):
        self._links: Dict[str, Link] = {}
        self._connections: List[tuple] = []
        self._hook: List[Hook] = []

    def add_link(self, link: Link[TInput, TOutput], name: Optional[str] = None) -> None:
        """With gentle inclusion, store the link."""
        # Use provided name or default to link's class name
        link_name = name or link.__class__.__name__
        self._links[link_name] = link

    def connect(self, source: str, target: str, condition: Callable[[State[TInput]], bool]) -> None:
        """With compassionate logic, add a connection."""
        self._connections.append((source, target, condition))

    def use_hook(self, hook: Hook) -> None:
        """Lovingly attach hook."""
        self._hook.append(hook)

    async def run(self, initial_ctx: State[TInput]) -> State[TOutput]:
        """With selfless execution, flow through links."""
        ctx = initial_ctx

        # Execute hook before hooks
        for mw in self._hook:
            await mw.before(None, ctx)

        try:
            # Simple linear execution for now
            for name, link in self._links.items():
                # Execute hook before each link
                for mw in self._hook:
                    await mw.before(link, ctx)

                # Execute the link - this evolves the state type
                ctx = await link.call(ctx)  # type: ignore

                # Execute hook after each link
                for mw in self._hook:
                    await mw.after(link, ctx)

        except Exception as e:
            # Execute hook error hooks
            for mw in self._hook:
                await mw.on_error(None, e, ctx)
            raise

        # Execute final hook after hooks
        for mw in self._hook:
            await mw.after(None, ctx)

        return ctx  # type: ignore