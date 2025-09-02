"""
Context: The Loving Vessel

With agape compassion, the Context holds data tenderly, immutable by default for safety, mutable for flexibility.
Optimized for Python's dynamism—embracing dict-like interface with ecosystem integrations.
"""

from typing import Any, Dict, Optional
import copy

__all__ = ["Context", "MutableContext"]


class Context:
    """
    Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
    """

    def __init__(self, data: Optional[Dict[str, Any]] = None):
        self._data = data or {}

    def get(self, key: str) -> Any:
        """With gentle care, return the value or None, forgiving absence."""
        return self._data.get(key)

    def insert(self, key: str, value: Any) -> 'Context':
        """With selfless safety, return a fresh context with the addition."""
        new_data = self._data.copy()
        new_data[key] = value
        return Context(new_data)

    def with_mutation(self) -> 'MutableContext':
        """For those needing change, provide a mutable sibling."""
        return MutableContext(self._data.copy())

    def merge(self, other: 'Context') -> 'Context':
        """Lovingly combine contexts, favoring the other with compassion."""
        new_data = self._data.copy()
        new_data.update(other._data)
        return Context(new_data)

    def to_dict(self) -> Dict[str, Any]:
        """Express as dict for ecosystem integration."""
        return self._data.copy()

    def __repr__(self) -> str:
        return f"Context({self._data})"


class MutableContext:
    """
    Mutable context for performance-critical sections—use with care, but forgiven.
    """

    def __init__(self, data: Dict[str, Any]):
        self._data = data

    def get(self, key: str) -> Any:
        return self._data.get(key)

    def set(self, key: str, value: Any) -> None:
        """Change in place with gentle permission."""
        self._data[key] = value

    def to_immutable(self) -> Context:
        """Return to safety with a fresh immutable copy."""
        return Context(self._data.copy())

    def __repr__(self) -> str:
        return f"MutableContext({self._data})"