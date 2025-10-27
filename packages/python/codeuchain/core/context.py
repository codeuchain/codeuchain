"""
Context: The Data Container

The Context holds data carefully, immutable by default for safety, mutable for flexibility.
Optimized for Python's dynamism—embracing dict-like interface with ecosystem integrations.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Any, Dict, Optional, TypeVar, Generic, Union

__all__ = ["Context", "MutableContext"]

# Type variables for generic typing
T = TypeVar('T')  # For single type contexts
TInput = TypeVar('TInput')  # For input types in chains
TOutput = TypeVar('TOutput')  # For output types in chains


class Context(Generic[T]):
    """
    Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
    Enhanced with generic typing for type-safe workflows.
    """

    def __init__(self, data: Optional[Union[Dict[str, Any], T]] = None):
        if data is None:
            self._data: Dict[str, Any] = {}
        elif isinstance(data, dict):
            self._data = data.copy() if data else {}
        else:
            # Handle TypedDict case - convert to dict for internal storage
            # Use getattr to safely access items if it's a TypedDict-like object
            try:
                self._data = dict(data)  # type: ignore
            except (TypeError, ValueError):
                self._data = {}

    def get(self, key: str, default: Any = None) -> Any:
        """With gentle care, return the value or default, forgiving absence."""
        return self._data.get(key, default)

    def insert(self, key: str, value: Any) -> 'Context[T]':
        """With selfless safety, return a fresh context with the addition."""
        new_data = self._data.copy()
        new_data[key] = value
        return Context[T](new_data)

    def insert_as(self, key: str, value: Any) -> 'Context[T]':
        """
        Create a new Context with type evolution, allowing clean transformation
        between TypedDict shapes without explicit casting.
        """
        new_data = self._data.copy()
        new_data[key] = value
        return Context[T](new_data)

    def with_mutation(self) -> 'MutableContext[T]':
        """For those needing change, provide a mutable sibling."""
        return MutableContext[T](self._data.copy())

    def merge(self, other: 'Context[T]') -> 'Context[T]':
        """Lovingly combine contexts, favoring the other with compassion."""
        new_data = self._data.copy()
        new_data.update(other._data)
        return Context[T](new_data)

    def to_dict(self) -> Dict[str, Any]:
        """Express as dict for ecosystem integration."""
        return self._data.copy()

    def __repr__(self) -> str:
        return f"Context({self._data})"


class MutableContext(Generic[T]):
    """
    Mutable context for performance-critical sections—use with care, but forgiven.
    Enhanced with generic typing for type-safe workflows.
    """

    def __init__(self, data: Optional[Dict[str, Any]] = None):
        self._data = data or {}

    def get(self, key: str, default: Any = None) -> Any:
        return self._data.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """Change in place with gentle permission."""
        self._data[key] = value

    def to_immutable(self) -> Context[T]:
        """Return to safety with a fresh immutable copy."""
        return Context[T](self._data.copy())

    def __repr__(self) -> str:
        return f"MutableContext({self._data})"