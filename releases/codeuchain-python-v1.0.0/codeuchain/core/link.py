"""
Link Protocol: The Selfless Processor Core

With agape selflessness, the Link protocol defines the interface for context processors.
Pure protocol—implementations belong in components.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Protocol, TypeVar
from .context import Context

__all__ = ["Link"]

# Type variables for generic link typing
TInput = TypeVar('TInput')
TOutput = TypeVar('TOutput')


class Link(Protocol[TInput, TOutput]):
    """
    Selfless processor—input context, output context, no judgment.
    The core protocol that all link implementations must follow.
    Enhanced with generic typing for type-safe workflows.
    """

    async def call(self, ctx: Context[TInput]) -> Context[TOutput]:
        """
        With unconditional love, process and return a transformed context.
        Implementations should be pure functions with no side effects.
        """
        ...