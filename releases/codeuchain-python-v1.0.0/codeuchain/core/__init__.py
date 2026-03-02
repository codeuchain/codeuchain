"""
Core Module: Base Protocols and Classes

The foundation that AI maintains and humans rarely touch.
Contains protocols, abstract base classes, and fundamental types.
"""

from .state import State, MutableState
from .link import Link
from .chain import Chain
from .hook import Hook

__all__ = ["State", "MutableState", "Link", "Chain", "Hook"]