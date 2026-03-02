"""
Chain Components: Reusable Chain Implementations

Concrete implementations of the Chain protocol.
These are the orchestrators that get composed into features.
"""

from typing import Dict, List, Callable, Set
from collections import deque
from codeuchain.core.state import State
from codeuchain.core.link import Link
from codeuchain.core.hook import Hook
from codeuchain.core.chain import Chain

__all__ = ["BasicChain"]


class BasicChain(Chain):
    """
    Loving weaver of links—connects with conditions, runs with selfless execution.
    A concrete implementation of the Chain protocol.
    """

    def __init__(self):
        self.links: Dict[str, Link] = {}
        self.connections: List[tuple[str, str, Callable[[State], bool]]] = []
        self.hooks: List[Hook] = []

    def add_link(self, name: str, link: Link) -> None:
        """With gentle inclusion, store the link."""
        self.links[name] = link

    def connect(self, source: str, target: str, condition: Callable[[State], bool]) -> None:
        """With compassionate logic, add a connection."""
        self.connections.append((source, target, condition))

    def use_hook(self, hook: Hook) -> None:
        """Lovingly attach hook."""
        self.hooks.append(hook)

    async def run(self, initial_ctx: State) -> State:
        """With selfless execution, flow through links."""
        ctx = initial_ctx
        for mw in self.hooks:
            await mw.before(None, ctx)

        executed: Set[str] = set()
        to_execute: deque[str] = deque(["start"] if "start" in self.links else list(self.links.keys())[:1])

        while to_execute:
            link_name = to_execute.popleft()
            if link_name in executed:
                continue
            link = self.links.get(link_name)
            if link:
                ctx = await link.call(ctx)
                executed.add(link_name)
                for src, tgt, cond in self.connections:
                    if src == link_name and cond(ctx):
                        to_execute.append(tgt)

        for mw in self.hooks:
            await mw.after(None, ctx)

        return ctx