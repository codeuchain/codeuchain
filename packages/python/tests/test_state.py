"""
Tests for State Classes

Testing immutable State and mutable MutableState functionality.
"""

import pytest
from codeuchain.core.state import State, MutableState


class TestState:
    """Test the immutable State class."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_empty_state(self):
        """Test creating an empty state."""
        ctx = State()
        assert ctx.get("nonexistent") is None
        assert ctx.to_dict() == {}

    @pytest.mark.unit
    @pytest.mark.core
    def test_state_with_data(self):
        """Test creating state with initial data."""
        data = {"name": "Alice", "age": 30}
        ctx = State(data)
        assert ctx.get("name") == "Alice"
        assert ctx.get("age") == 30
        assert ctx.get("nonexistent") is None

    @pytest.mark.unit
    @pytest.mark.core
    def test_insert_immutability(self):
        """Test that insert returns new state without modifying original."""
        ctx1 = State({"name": "Alice"})
        ctx2 = ctx1.insert("age", 30)

        # Original should be unchanged
        assert ctx1.get("age") is None
        assert ctx1.get("name") == "Alice"

        # New state should have the insertion
        assert ctx2.get("age") == 30
        assert ctx2.get("name") == "Alice"

        # States should be different objects
        assert ctx1 is not ctx2

    @pytest.mark.unit
    @pytest.mark.core
    def test_merge_states(self):
        """Test merging two states."""
        ctx1 = State({"name": "Alice", "age": 30})
        ctx2 = State({"city": "Wonderland", "age": 25})  # age should be overridden

        merged = ctx1.merge(ctx2)

        assert merged.get("name") == "Alice"
        assert merged.get("city") == "Wonderland"
        assert merged.get("age") == 25  # from ctx2

        # Original states should be unchanged
        assert ctx1.get("age") == 30
        assert ctx2.get("city") == "Wonderland"

    @pytest.mark.unit
    @pytest.mark.core
    def test_to_dict(self):
        """Test converting state to dictionary."""
        data = {"name": "Alice", "age": 30}
        ctx = State(data)
        dict_result = ctx.to_dict()

        assert dict_result == data
        assert dict_result is not data  # Should be a copy

        # Modifying the dict shouldn't affect the state
        dict_result["new_key"] = "new_value"
        assert ctx.get("new_key") is None

    @pytest.mark.unit
    @pytest.mark.core
    def test_with_mutation(self):
        """Test converting to mutable state."""
        ctx = State({"name": "Alice"})
        mutable = ctx.with_mutation()

        assert isinstance(mutable, MutableState)
        assert mutable.get("name") == "Alice"

        # Original should be unchanged
        mutable.set("name", "Bob")
        assert ctx.get("name") == "Alice"
        assert mutable.get("name") == "Bob"

    @pytest.mark.unit
    @pytest.mark.core
    def test_repr(self):
        """Test string representation."""
        ctx = State({"name": "Alice"})
        repr_str = repr(ctx)
        assert "State" in repr_str
        assert "Alice" in repr_str

    @pytest.mark.unit
    @pytest.mark.core
    def test_get_with_default_value(self):
        """Test get() method with default parameter."""
        ctx = State({"name": "Alice", "age": 30})
        
        # Existing keys should ignore default
        assert ctx.get("name", "default") == "Alice"
        assert ctx.get("age", 0) == 30
        
        # Missing keys should return the default
        assert ctx.get("missing", "default_value") == "default_value"
        assert ctx.get("missing", 0) == 0
        assert ctx.get("missing", False) is False
        assert ctx.get("missing", []) == []
        assert ctx.get("missing", {}) == {}
        
        # Without default, should still return None
        assert ctx.get("missing") is None
        
    @pytest.mark.unit
    @pytest.mark.core
    def test_get_with_falsy_values(self):
        """Test that default parameter works correctly with falsy stored values."""
        # This tests that we're not using 'or' logic which would incorrectly
        # replace falsy values with the default
        ctx = State({
            "zero": 0,
            "false": False,
            "empty_string": "",
            "empty_list": [],
            "none": None
        })
        
        # All falsy values should be returned, not the default
        assert ctx.get("zero", 999) == 0
        assert ctx.get("false", True) is False
        assert ctx.get("empty_string", "default") == ""
        assert ctx.get("empty_list", ["default"]) == []
        
        # None value should still be returned (not default)
        assert ctx.get("none", "default") is None


class TestMutableState:
    """Test the mutable MutableState class."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_mutable_state_creation(self):
        """Test creating mutable state."""
        data = {"name": "Alice"}
        mutable = MutableState(data)
        assert mutable.get("name") == "Alice"

    @pytest.mark.unit
    @pytest.mark.core
    def test_set_value(self):
        """Test setting values in mutable state."""
        mutable = MutableState({})
        mutable.set("name", "Alice")
        mutable.set("age", 30)

        assert mutable.get("name") == "Alice"
        assert mutable.get("age") == 30

    @pytest.mark.unit
    @pytest.mark.core
    def test_to_immutable(self):
        """Test converting mutable state to immutable."""
        mutable = MutableState({"name": "Alice"})
        mutable.set("age", 30)

        immutable = mutable.to_immutable()

        assert isinstance(immutable, State)
        assert immutable.get("name") == "Alice"
        assert immutable.get("age") == 30

        # Changes to mutable shouldn't affect immutable
        mutable.set("name", "Bob")
        assert immutable.get("name") == "Alice"
        assert mutable.get("name") == "Bob"

    @pytest.mark.unit
    @pytest.mark.core
    def test_mutable_repr(self):
        """Test string representation of mutable state."""
        mutable = MutableState({"name": "Alice"})
        repr_str = repr(mutable)
        assert "MutableState" in repr_str
        assert "Alice" in repr_str

    @pytest.mark.unit
    @pytest.mark.core
    def test_mutable_get_with_default_value(self):
        """Test get() method with default parameter for mutable state."""
        mutable = MutableState({"name": "Alice", "age": 30})
        
        # Existing keys should ignore default
        assert mutable.get("name", "default") == "Alice"
        assert mutable.get("age", 0) == 30
        
        # Missing keys should return the default
        assert mutable.get("missing", "default_value") == "default_value"
        assert mutable.get("missing", 0) == 0
        assert mutable.get("missing", False) is False
        
        # Without default, should still return None
        assert mutable.get("missing") is None


class TestStateIntegration:
    """Integration tests for State and MutableState."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_round_trip_conversion(self):
        """Test converting between mutable and immutable states."""
        # Start with immutable
        ctx = State({"name": "Alice", "age": 30})

        # Convert to mutable and modify
        mutable = ctx.with_mutation()
        mutable.set("city", "Wonderland")
        mutable.set("age", 25)

        # Convert back to immutable
        final_ctx = mutable.to_immutable()

        assert final_ctx.get("name") == "Alice"
        assert final_ctx.get("city") == "Wonderland"
        assert final_ctx.get("age") == 25

        # Original should be unchanged
        assert ctx.get("city") is None
        assert ctx.get("age") == 30

    @pytest.mark.integration
    @pytest.mark.core
    def test_complex_data_structures(self):
        """Test with complex nested data structures."""
        complex_data = {
            "user": {"name": "Alice", "profile": {"age": 30, "city": "Wonderland"}},
            "items": ["apple", "banana", "cherry"],
            "metadata": {"created": "2023-01-01", "version": 1.0}
        }

        ctx = State(complex_data)
        dict_result = ctx.to_dict()

        assert dict_result == complex_data
        assert dict_result is not complex_data  # Should be a deep copy

        # Test immutability with nested structures
        ctx2 = ctx.insert("new_field", "new_value")
        assert ctx.get("new_field") is None
        assert ctx2.get("new_field") == "new_value"

        # Original nested data should be preserved
        assert ctx.get("user")["name"] == "Alice"
        assert ctx2.get("user")["name"] == "Alice"