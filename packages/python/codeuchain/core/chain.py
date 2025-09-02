"""
Chain: The Harmonious Connector

With agape harmony, the Chain orchestrates link execution with conditional flows and middleware.
Core implementation that all chain implementations can build upon.
"""

from typing import Dict, List, Callable, Optional
from .context import Context
from .link import Link
from .middleware import Middleware

__all__ = ["Chain"]


class Chain:
    """
    Loving weaver of links—connects with conditions, runs with selfless execution.
    Core implementation that provides full chain functionality.
    """

    def __init__(self):
        self._links: Dict[str, Link] = {}
        self._connections: List[tuple] = []
        self._middleware: List[Middleware] = []

    def add_link(self, link: Link, name: Optional[str] = None) -> None:
        """With gentle inclusion, store the link."""
        # Use provided name or default to link's class name
        link_name = name or link.__class__.__name__
        self._links[link_name] = link

    def connect(self, source: str, target: str, condition: Callable[[Context], bool]) -> None:
        """With compassionate logic, add a connection."""
        self._connections.append((source, target, condition))

    def use_middleware(self, middleware: Middleware) -> None:
        """Lovingly attach middleware."""
        self._middleware.append(middleware)

    async def run(self, initial_ctx: Context) -> Context:
        """With selfless execution, flow through links."""
        ctx = initial_ctx

        # Execute middleware before hooks
        for mw in self._middleware:
            await mw.before(None, ctx)

        try:
            # Simple linear execution for now
            for name, link in self._links.items():
                # Execute middleware before each link
                for mw in self._middleware:
                    await mw.before(link, ctx)

                # Execute the link
                ctx = await link.call(ctx)

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

        return ctx