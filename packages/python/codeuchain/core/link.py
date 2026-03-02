"""
Link Protocol: The Processing Unit Core

The Link protocol defines the interface for state processors.
Pure protocol—implementations belong in components.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Protocol, TypeVar
from .state import State

__all__ = ["Link"]

# Type variables for generic link typing
TInput = TypeVar('TInput')
TOutput = TypeVar('TOutput')


class Link(Protocol[TInput, TOutput]):
    """
    Selfless processor—input state, output state, no judgment.
    The core protocol that all link implementations must follow.
    Enhanced with generic typing for type-safe workflows.
    """

    async def call(self, ctx: State[TInput]) -> State[TOutput]:
        """
        With unconditional love, process and return a transformed state.
        Implementations should be pure functions with no side effects.
        """
        ...