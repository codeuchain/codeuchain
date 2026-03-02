"""
State: The Data Container

The State holds data carefully, immutable by default for safety, mutable for flexibility.
Optimized for Python's dynamism—embracing dict-like interface with ecosystem integrations.
Enhanced with generic typing for type-safe workflows.
"""

from typing import Any, Dict, Optional, TypeVar, Generic, Union

__all__ = ["State", "MutableState"]

# Type variables for generic typing
T = TypeVar('T')  # For single type states
TInput = TypeVar('TInput')  # For input types in chains
TOutput = TypeVar('TOutput')  # For output types in chains


class State(Generic[T]):
    """
    Immutable state with selfless love—holds data without judgment, returns fresh copies for changes.
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

    def insert(self, key: str, value: Any) -> 'State[T]':
        """With selfless safety, return a fresh state with the addition."""
        new_data = self._data.copy()
        new_data[key] = value
        return State[T](new_data)

    def insert_as(self, key: str, value: Any) -> 'State[T]':
        """
        Create a new State with type evolution, allowing clean transformation
        between TypedDict shapes without explicit casting.
        """
        new_data = self._data.copy()
        new_data[key] = value
        return State[T](new_data)

    def with_mutation(self) -> 'MutableState[T]':
        """For those needing change, provide a mutable sibling."""
        return MutableState[T](self._data.copy())

    def merge(self, other: 'State[T]') -> 'State[T]':
        """Lovingly combine states, favoring the other with compassion."""
        new_data = self._data.copy()
        new_data.update(other._data)
        return State[T](new_data)

    def to_dict(self) -> Dict[str, Any]:
        """Express as dict for ecosystem integration."""
        return self._data.copy()

    def __repr__(self) -> str:
        return f"State({self._data})"


class MutableState(Generic[T]):
    """
    Mutable state for performance-critical sections—use with care, but forgiven.
    Enhanced with generic typing for type-safe workflows.
    """

    def __init__(self, data: Optional[Dict[str, Any]] = None):
        self._data = data or {}

    def get(self, key: str, default: Any = None) -> Any:
        return self._data.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """Change in place with gentle permission."""
        self._data[key] = value

    def to_immutable(self) -> State[T]:
        """Return to safety with a fresh immutable copy."""
        return State[T](self._data.copy())

    def __repr__(self) -> str:
        return f"MutableState({self._data})"