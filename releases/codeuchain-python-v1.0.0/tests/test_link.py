"""
Tests for Link Protocol

Testing the Link protocol with concrete implementations.
"""

import pytest
from codeuchain.core.context import Context
from codeuchain.core.link import Link


class TestLinkProtocol:
    """Test the Link protocol interface."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_link_is_protocol(self):
        """Test that Link is a protocol."""
        from typing import Protocol
        assert issubclass(Link, Protocol)


class SimpleProcessingLink:
    """Concrete Link implementation for testing."""

    def __init__(self, name: str = "test"):
        self.name = name

    async def call(self, ctx: Context) -> Context:
        """Simple processing: add a 'processed' field."""
        return ctx.insert("processed", True).insert("processor", self.name)


class DataTransformationLink:
    """Link that transforms data."""

    async def call(self, ctx: Context) -> Context:
        """Transform data by doubling numbers and uppercasing strings."""
        data = ctx.get("data")
        if isinstance(data, list):
            transformed = []
            for item in data:
                if isinstance(item, bool):
                    # Preserve booleans as-is
                    transformed.append(item)
                elif isinstance(item, (int, float)):
                    transformed.append(item * 2)
                elif isinstance(item, str):
                    transformed.append(item.upper())
                else:
                    transformed.append(item)
            return ctx.insert("transformed", transformed)
        return ctx.insert("transformed", data)


class ValidationLink:
    """Link that validates input data."""

    def __init__(self, required_fields: list):
        self.required_fields = required_fields

    async def call(self, ctx: Context) -> Context:
        """Validate required fields exist."""
        for field in self.required_fields:
            if ctx.get(field) is None:
                return ctx.insert("error", f"Missing required field: {field}")
        return ctx.insert("validated", True)


class FailingLink:
    """Link that always fails for testing error handling."""

    async def call(self, ctx: Context) -> Context:
        """Always raise an exception."""
        raise ValueError("Intentional failure for testing")


class TestSimpleProcessingLink:
    """Test the SimpleProcessingLink implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_simple_processing(self):
        """Test basic processing functionality."""
        link = SimpleProcessingLink("test_processor")

        async def run_test():
            ctx = Context({"input": "test_data"})
            result = await link.call(ctx)

            assert result.get("processed") is True
            assert result.get("processor") == "test_processor"
            assert result.get("input") == "test_data"  # Original data preserved

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_empty_context_processing(self):
        """Test processing with empty context."""
        link = SimpleProcessingLink()

        async def run_test():
            ctx = Context()
            result = await link.call(ctx)

            assert result.get("processed") is True
            assert result.get("processor") == "test"

        import asyncio
        asyncio.run(run_test())


class TestDataTransformationLink:
    """Test the DataTransformationLink implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_numeric_transformation(self):
        """Test transforming numeric data."""
        link = DataTransformationLink()

        async def run_test():
            ctx = Context({"data": [1, 2, 3, 4.5]})
            result = await link.call(ctx)

            transformed = result.get("transformed")
            assert transformed == [2, 4, 6, 9.0]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_string_transformation(self):
        """Test transforming string data."""
        link = DataTransformationLink()

        async def run_test():
            ctx = Context({"data": ["hello", "world"]})
            result = await link.call(ctx)

            transformed = result.get("transformed")
            assert transformed == ["HELLO", "WORLD"]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_mixed_data_transformation(self):
        """Test transforming mixed data types."""
        link = DataTransformationLink()

        async def run_test():
            ctx = Context({"data": ["hello", 42, True]})
            result = await link.call(ctx)

            transformed = result.get("transformed")
            assert transformed == ["HELLO", 84, True]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_non_list_data(self):
        """Test with non-list data."""
        link = DataTransformationLink()

        async def run_test():
            ctx = Context({"data": "single_value"})
            result = await link.call(ctx)

            assert result.get("transformed") == "single_value"

        import asyncio
        asyncio.run(run_test())


class TestValidationLink:
    """Test the ValidationLink implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_successful_validation(self):
        """Test validation with all required fields present."""
        link = ValidationLink(["name", "email"])

        async def run_test():
            ctx = Context({"name": "Alice", "email": "alice@example.com", "age": 30})
            result = await link.call(ctx)

            assert result.get("validated") is True
            assert result.get("error") is None

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_validation_failure(self):
        """Test validation with missing required fields."""
        link = ValidationLink(["name", "email"])

        async def run_test():
            ctx = Context({"name": "Alice"})  # Missing email
            result = await link.call(ctx)

            assert result.get("validated") is None
            assert result.get("error") == "Missing required field: email"

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_multiple_validation_failures(self):
        """Test validation reports first missing field."""
        link = ValidationLink(["name", "email", "phone"])

        async def run_test():
            ctx = Context({"email": "alice@example.com"})  # Missing name and phone
            result = await link.call(ctx)

            assert result.get("validated") is None
            assert result.get("error") == "Missing required field: name"

        import asyncio
        asyncio.run(run_test())


class TestFailingLink:
    """Test the FailingLink implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_always_fails(self):
        """Test that FailingLink always raises an exception."""
        link = FailingLink()

        async def run_test():
            ctx = Context({"data": "test"})
            with pytest.raises(ValueError, match="Intentional failure for testing"):
                await link.call(ctx)

        import asyncio
        asyncio.run(run_test())


class TestLinkIntegration:
    """Integration tests for Link implementations."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_link_chain_processing(self):
        """Test multiple links processing in sequence."""
        validation_link = ValidationLink(["data"])
        processing_link = SimpleProcessingLink("integrated_processor")

        async def run_test():
            # Start with valid data
            ctx = Context({"data": "test_input"})

            # First validate
            validated_ctx = await validation_link.call(ctx)
            assert validated_ctx.get("validated") is True

            # Then process
            final_ctx = await processing_link.call(validated_ctx)
            assert final_ctx.get("processed") is True
            assert final_ctx.get("processor") == "integrated_processor"
            assert final_ctx.get("data") == "test_input"  # Original preserved

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.integration
    @pytest.mark.core
    def test_link_error_handling(self):
        """Test error handling in link processing."""
        validation_link = ValidationLink(["required_field"])
        failing_link = FailingLink()

        async def run_test():
            # Test validation failure
            ctx = Context({"optional": "value"})  # Missing required_field
            result = await validation_link.call(ctx)
            assert result.get("error") == "Missing required field: required_field"

            # Test runtime failure
            ctx2 = Context({"data": "test"})
            with pytest.raises(ValueError):
                await failing_link.call(ctx2)

        import asyncio
        asyncio.run(run_test())