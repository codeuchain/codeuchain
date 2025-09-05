"""
Core Module: Base Protocols and Classes

The foundation that AI maintains and humans rarely touch.
Contains protocols, abstract base classes, and fundamental types.
"""

from .context import Context, MutableContext
from .link import Link
from .chain import Chain
from .middleware import Middleware

__all__ = ["Context", "MutableContext", "Link", "Chain", "Middleware"]