"""
Middleware ABC: The Gentle Enhancer Core

With agape gentleness, the Middleware ABC defines optional enhancement hooks.
Abstract base class—implementations belong in components and can override any/all methods.
"""

from abc import ABC
from typing import Optional
from .context import Context
from .link import Link

__all__ = ["Middleware"]


class Middleware(ABC):
    """
    Gentle enhancer—optional hooks with forgiving defaults.
    Abstract base class that middleware implementations can inherit from.
    Subclasses can override any combination of before(), after(), and on_error().
    """

    async def before(self, link: Optional[Link], ctx: Context) -> None:
        """With selfless optionality, do nothing by default."""
        pass

    async def after(self, link: Optional[Link], ctx: Context) -> None:
        """Forgiving default."""
        pass

    async def on_error(self, link: Optional[Link], error: Exception, ctx: Context) -> None:
        """Compassionate error handling."""
        pass