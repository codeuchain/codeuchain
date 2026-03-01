"""
Chain: The Orchestrator

The Chain orchestrates link execution with conditional flows and middleware.
Core implementation that all chain implementations can build upon.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Dict, List, Callable, Optional, Tuple, TypeVar, Generic
from .context import Context
from .link import Link
from .middleware import Middleware

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
        self._middleware: List[Middleware] = []

    def add_link(self, link: Link[TInput, TOutput], name: Optional[str] = None) -> None:
        """With gentle inclusion, store the link."""
        # Use provided name or default to link's class name
        link_name = name or link.__class__.__name__
        self._links[link_name] = link

    def connect(self, source: str, target: str, condition: Callable[[Context[TInput]], bool]) -> None:
        """
        With compassionate logic, add a conditional connection between two links.

        The condition is evaluated just before the target link would execute.
        If *any* registered condition for a given target link evaluates to True,
        the target link executes.  If *all* conditions evaluate to False the
        target link is skipped entirely.

        Links that have no incoming connections are always executed.
        """
        self._connections.append((source, target, condition))

    def use_middleware(self, middleware: Middleware) -> None:
        """Lovingly attach middleware."""
        self._middleware.append(middleware)

    async def run(self, initial_ctx: Context[TInput]) -> Context[TOutput]:
        """With selfless execution, flow through links."""
        ctx = initial_ctx

        # Build a map of target link name -> list of (source, condition) pairs
        # so we can evaluate predicates before each link executes.
        incoming: Dict[str, List[Tuple[str, Callable[[Context[TInput]], bool]]]] = {}
        for source, target, condition in self._connections:
            incoming.setdefault(target, []).append((source, condition))

        # Execute middleware before hooks
        for mw in self._middleware:
            await mw.before(None, ctx)

        try:
            # Simple linear execution for now
            for name, link in self._links.items():
                # If this link has incoming connections, skip it unless at
                # least one predicate evaluates to True.
                if name in incoming:
                    if not any(cond(ctx) for _, cond in incoming[name]):
                        continue

                # Execute middleware before each link
                for mw in self._middleware:
                    await mw.before(link, ctx)

                # Execute the link - this evolves the context type
                ctx = await link.call(ctx)  # type: ignore

                # Execute middleware after each link
                for mw in self._middleware:
                    await mw.after(link, ctx)

        except Exception as e:
            # Execute middleware error hooks
            for mw in self._middleware:
                await mw.on_error(None, e, ctx)
            raise

        # Execute final middleware after hooks
        for mw in self._middleware:
            await mw.after(None, ctx)

        return ctx  # type: ignore