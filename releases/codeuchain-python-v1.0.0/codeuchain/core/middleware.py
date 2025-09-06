"""
Middleware ABC: The Gentle Enhancer Core

With agape gentleness, the Middleware ABC defines optional enhancement hooks.
Abstract base class—implementations belong in components and can override any/all methods.
Enhanced with generic typing for type-safe workflows.
"""

from abc import ABC
from typing import Optional, TypeVar
from .context import Context
from .link import Link

__all__ = ["Middleware"]

# Type variables for generic middleware typing
T = TypeVar('T')


class Middleware(ABC):
    """
    Gentle enhancer—optional hooks with forgiving defaults.
    Abstract base class that middleware implementations can inherit from.
    Subclasses can override any combination of before(), after(), and on_error().
    Enhanced with generic typing for type-safe workflows.
    """

    async def before(self, link: Optional[Link], ctx: Context[T]) -> None:
        """With selfless optionality, do nothing by default."""
        pass

    async def after(self, link: Optional[Link], ctx: Context[T]) -> None:
        """Forgiving default."""
        pass

    async def on_error(self, link: Optional[Link], error: Exception, ctx: Context[T]) -> None:
        """Compassionate error handling."""
        pass