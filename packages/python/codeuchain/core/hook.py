"""
Hook ABC: The Enhancement Layer Core

The Hook ABC defines optional enhancement hooks.
Abstract base class—implementations belong in components and can override any/all methods.
Enhanced with generic typing for type-safe workflows.
"""

from abc import ABC
from typing import Optional, TypeVar
from .state import State
from .link import Link

__all__ = ["Hook"]

# Type variables for generic hook typing
T = TypeVar('T')


class Hook(ABC):
    """
    Gentle enhancer—optional hooks with forgiving defaults.
    Abstract base class that hook implementations can inherit from.
    Subclasses can override any combination of before(), after(), and on_error().
    Enhanced with generic typing for type-safe workflows.
    """

    async def before(self, link: Optional[Link], ctx: State[T]) -> None:
        """With selfless optionality, do nothing by default."""
        pass

    async def after(self, link: Optional[Link], ctx: State[T]) -> None:
        """Forgiving default."""
        pass

    async def on_error(self, link: Optional[Link], error: Exception, ctx: State[T]) -> None:
        """Compassionate error handling."""
        pass