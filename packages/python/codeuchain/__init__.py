"""
CodeUChain: Modular Python Implementation

CodeUChain provides a modular framework for chaining processing links with hook support.
Optimized for Python's prototyping capabilities—embracing dynamism, ecosystem, and flexibility.

Library Structure:
- core/: Base protocols and classes (AI maintains)
- utils/: Shared utilities (everyone uses)
"""

# Core protocols and base classes
from .core import State, MutableState, Link, Chain, Hook

# Utility helpers
from .utils import ErrorHandlingMixin, RetryLink

__version__ = "1.1.0"
__all__ = [
    # Core
    "State", "MutableState", "Link", "Chain", "Hook",
    # Utils
    "ErrorHandlingMixin", "RetryLink"
]