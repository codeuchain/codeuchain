"""
Tests for Context Classes

Testing immutable Context and mutable MutableContext functionality.
"""

import pytest
from codeuchain.core.context import Context, MutableContext


class TestContext:
    """Test the immutable Context class."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_empty_context(self):
        """Test creating an empty context."""
        ctx = Context()
        assert ctx.get("nonexistent") is None
        assert ctx.to_dict() == {}

    @pytest.mark.unit
    @pytest.mark.core
    def test_context_with_data(self):
        """Test creating context with initial data."""
        data = {"name": "Alice", "age": 30}
        ctx = Context(data)
        assert ctx.get("name") == "Alice"
        assert ctx.get("age") == 30
        assert ctx.get("nonexistent") is None

    @pytest.mark.unit
    @pytest.mark.core
    def test_insert_immutability(self):
        """Test that insert returns new context without modifying original."""
        ctx1 = Context({"name": "Alice"})
        ctx2 = ctx1.insert("age", 30)

        # Original should be unchanged
        assert ctx1.get("age") is None
        assert ctx1.get("name") == "Alice"

        # New context should have the insertion
        assert ctx2.get("age") == 30
        assert ctx2.get("name") == "Alice"

        # Contexts should be different objects
        assert ctx1 is not ctx2

    @pytest.mark.unit
    @pytest.mark.core
    def test_merge_contexts(self):
        """Test merging two contexts."""
        ctx1 = Context({"name": "Alice", "age": 30})
        ctx2 = Context({"city": "Wonderland", "age": 25})  # age should be overridden

        merged = ctx1.merge(ctx2)

        assert merged.get("name") == "Alice"
        assert merged.get("city") == "Wonderland"
        assert merged.get("age") == 25  # from ctx2

        # Original contexts should be unchanged
        assert ctx1.get("age") == 30
        assert ctx2.get("city") == "Wonderland"

    @pytest.mark.unit
    @pytest.mark.core
    def test_to_dict(self):
        """Test converting context to dictionary."""
        data = {"name": "Alice", "age": 30}
        ctx = Context(data)
        dict_result = ctx.to_dict()

        assert dict_result == data
        assert dict_result is not data  # Should be a copy

        # Modifying the dict shouldn't affect the context
        dict_result["new_key"] = "new_value"
        assert ctx.get("new_key") is None

    @pytest.mark.unit
    @pytest.mark.core
    def test_with_mutation(self):
        """Test converting to mutable context."""
        ctx = Context({"name": "Alice"})
        mutable = ctx.with_mutation()

        assert isinstance(mutable, MutableContext)
        assert mutable.get("name") == "Alice"

        # Original should be unchanged
        mutable.set("name", "Bob")
        assert ctx.get("name") == "Alice"
        assert mutable.get("name") == "Bob"

    @pytest.mark.unit
    @pytest.mark.core
    def test_repr(self):
        """Test string representation."""
        ctx = Context({"name": "Alice"})
        repr_str = repr(ctx)
        assert "Context" in repr_str
        assert "Alice" in repr_str


class TestMutableContext:
    """Test the mutable MutableContext class."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_mutable_context_creation(self):
        """Test creating mutable context."""
        data = {"name": "Alice"}
        mutable = MutableContext(data)
        assert mutable.get("name") == "Alice"

    @pytest.mark.unit
    @pytest.mark.core
    def test_set_value(self):
        """Test setting values in mutable context."""
        mutable = MutableContext({})
        mutable.set("name", "Alice")
        mutable.set("age", 30)

        assert mutable.get("name") == "Alice"
        assert mutable.get("age") == 30

    @pytest.mark.unit
    @pytest.mark.core
    def test_to_immutable(self):
        """Test converting mutable context to immutable."""
        mutable = MutableContext({"name": "Alice"})
        mutable.set("age", 30)

        immutable = mutable.to_immutable()

        assert isinstance(immutable, Context)
        assert immutable.get("name") == "Alice"
        assert immutable.get("age") == 30

        # Changes to mutable shouldn't affect immutable
        mutable.set("name", "Bob")
        assert immutable.get("name") == "Alice"
        assert mutable.get("name") == "Bob"

    @pytest.mark.unit
    @pytest.mark.core
    def test_mutable_repr(self):
        """Test string representation of mutable context."""
        mutable = MutableContext({"name": "Alice"})
        repr_str = repr(mutable)
        assert "MutableContext" in repr_str
        assert "Alice" in repr_str


class TestContextIntegration:
    """Integration tests for Context and MutableContext."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_round_trip_conversion(self):
        """Test converting between mutable and immutable contexts."""
        # Start with immutable
        ctx = Context({"name": "Alice", "age": 30})

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

        ctx = Context(complex_data)
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