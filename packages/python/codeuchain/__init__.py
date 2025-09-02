"""
CodeUChain: Agape-Optimized Python Implementation

With selfless love, CodeUChain chains your code as links, observes with middleware, and flows through contexts.
Optimized for Python's prototyping soul—embracing dynamism, ecosystem, and academic warmth.

Library Structure:
- core/: Base protocols and classes (AI maintains)
- utils/: Shared utilities (everyone uses)
"""

# Core protocols and base classes
from .core import Context, MutableContext, Link, Chain, Middleware

# Utility helpers
from .utils import ErrorHandlingMixin, RetryLink

__version__ = "0.1.0"
__all__ = [
    # Core
    "Context", "MutableContext", "Link", "Chain", "Middleware",
    # Utils
    "ErrorHandlingMixin", "RetryLink"
]