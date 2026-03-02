"""
Chain: The Orchestrator

The Chain orchestrates link execution with conditional flows and hook.
Core implementation that all chain implementations can build upon.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Any, Dict, List, Callable, Optional, Set, Tuple, TypeVar, Generic
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

    def connect(self, source: str, target: str, condition: Callable[[State[Any]], bool]) -> None:
        """
        With compassionate logic, add a conditional connection between two links.

        The condition is evaluated just before the target link would execute, but
        only if its *source* link has already executed in this run.  If *any*
        registered condition (whose source has run) evaluates to True, the target
        link executes.  If *all* such conditions evaluate to False—or if no source
        link has executed yet—the target link is skipped entirely.

        Links that have no incoming connections are always executed.

        The predicate accepts ``State[Any]`` so it works correctly across all
        stages of a typed chain where the state type evolves between links.
        """
        self._connections.append((source, target, condition))

    def use_hook(self, hook: Hook) -> None:
        """Lovingly attach hook."""
        self._hook.append(hook)

    async def run(self, initial_ctx: State[TInput]) -> State[TOutput]:
        """With selfless execution, flow through links."""
        ctx = initial_ctx

        # Build a map of target link name -> list of (source, condition) pairs
        # so we can evaluate predicates before each link executes.
        incoming: Dict[str, List[Tuple[str, Any]]] = {}
        for source, target, condition in self._connections:
            incoming.setdefault(target, []).append((source, condition))

        # Track which links have completed so we only evaluate predicates from
        # sources that have actually run.
        executed_links: Set[str] = set()

        # Execute hook before hooks
        for mw in self._hook:
            await mw.before(None, ctx)

        try:
            # Simple linear execution for now
            for name, link in self._links.items():
                # If this link has incoming connections, only evaluate predicates
                # whose source has already executed.  Skip unless at least one
                # predicate evaluates to True.
                if name in incoming:
                    relevant = [
                        cond for src, cond in incoming[name]
                        if src in executed_links
                    ]
                    if not relevant or not any(cond(ctx) for cond in relevant):
                        continue

                # Execute hook before each link
                for mw in self._hook:
                    await mw.before(link, ctx)

                # Execute the link - this evolves the state type
                ctx = await link.call(ctx)  # type: ignore

                executed_links.add(name)

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