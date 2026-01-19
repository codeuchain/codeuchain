"""
CodeUChain: Modular Python Implementation

CodeUChain provides a modular framework for chaining processing links with middleware support.
Optimized for Python's prototyping capabilities—embracing dynamism, ecosystem, and flexibility.

Library Structure:
- core/: Base protocols and classes (AI maintains)
- utils/: Shared utilities (everyone uses)
"""

# Core protocols and base classes
from .core import Context, MutableContext, Link, Chain, Middleware

# Utility helpers
from .utils import ErrorHandlingMixin, RetryLink

__version__ = "1.1.0"
__all__ = [
    # Core
    "Context", "MutableContext", "Link", "Chain", "Middleware",
    # Utils
    "ErrorHandlingMixin", "RetryLink"
]