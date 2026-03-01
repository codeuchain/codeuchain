"""
Chain: The Orchestrator

The Chain orchestrates link execution with conditional flows and middleware.
Core implementation that all chain implementations can build upon.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Any, Dict, List, Callable, Optional, Set, Tuple, TypeVar, Generic
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

    def connect(self, source: str, target: str, condition: Callable[[Context[Any]], bool]) -> None:
        """
        With compassionate logic, add a conditional connection between two links.

        The condition is evaluated just before the target link would execute, but
        only if its *source* link has already executed in this run.  If *any*
        registered condition (whose source has run) evaluates to True, the target
        link executes.  If *all* such conditions evaluate to False—or if no source
        link has executed yet—the target link is skipped entirely.

        Links that have no incoming connections are always executed.

        The predicate accepts ``Context[Any]`` so it works correctly across all
        stages of a typed chain where the context type evolves between links.
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
        incoming: Dict[str, List[Tuple[str, Callable[[Context[Any]], bool]]]] = {}
        for source, target, condition in self._connections:
            incoming.setdefault(target, []).append((source, condition))

        # Track which links have completed so we only evaluate predicates from
        # sources that have actually run (prevents spurious skips for out-of-order
        # or self-referential connections).
        executed_links: Set[str] = set()

        # Execute middleware before hooks
        for mw in self._middleware:
            await mw.before(None, ctx)

        try:
            # Simple linear execution for now
            for name, link in self._links.items():
                # If this link has incoming connections, only evaluate predicates
                # whose source has already executed.  Skip the link unless at
                # least one such predicate evaluates to True.
                if name in incoming:
                    relevant = [
                        cond for src, cond in incoming[name]
                        if src in executed_links
                    ]
                    if not relevant or not any(cond(ctx) for cond in relevant):
                        continue

                # Execute middleware before each link
                for mw in self._middleware:
                    await mw.before(link, ctx)

                # Execute the link - this evolves the context type
                ctx = await link.call(ctx)  # type: ignore

                executed_links.add(name)

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