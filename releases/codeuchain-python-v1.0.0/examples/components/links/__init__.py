"""
Link Components: Reusable Link Implementations

Concrete implementations of the Link protocol.
These are the building blocks that get swapped between projects.
"""

from typing import List
from codeuchain.core.state import State
from codeuchain.core.link import Link

__all__ = ["IdentityLink", "MathLink"]


class IdentityLink(Link):
    """Forgiving link that does nothing—pure love."""

    async def call(self, ctx: State) -> State:
        return ctx


class MathLink(Link):
    """Math-focused link, embracing NumPy ecosystem."""

    def __init__(self, operation: str = "sum"):
        self.operation = operation

    async def call(self, ctx: State) -> State:
        numbers = ctx.get("numbers")
        if isinstance(numbers, list) and numbers:
            if self.operation == "sum":
                result = sum(numbers)
            elif self.operation == "mean":
                result = sum(numbers) / len(numbers)
            else:
                result = 0
            return ctx.insert("result", result)
        return ctx.insert("error", "Invalid numbers")