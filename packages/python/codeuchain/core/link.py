"""
Link Protocol: The Selfless Processor Core

With agape selflessness, the Link protocol defines the interface for context processors.
Pure protocol—implementations belong in components.
"""

from typing import Protocol
from .context import Context

__all__ = ["Link"]


class Link(Protocol):
    """
    Selfless processor—input context, output context, no judgment.
    The core protocol that all link implementations must follow.
    """

    async def call(self, ctx: Context) -> Context:
        """
        With unconditional love, process and return a transformed context.
        Implementations should be pure functions with no side effects.
        """
        ...