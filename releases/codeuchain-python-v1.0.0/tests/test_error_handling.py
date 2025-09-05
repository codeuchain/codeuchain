"""
Tests for Error Handling Utilities

Testing ErrorHandlingMixin and RetryLink utilities.
"""

import pytest
from typing import Dict, List, Callable, Tuple
from codeuchain.core.context import Context
from codeuchain.utils.error_handling import ErrorHandlingMixin, RetryLink
from .conftest import MockLink


class TestErrorHandlingMixin:
    """Test the ErrorHandlingMixin functionality."""

    @pytest.mark.unit
    @pytest.mark.utils
    def test_mixin_initialization(self):
        """Test that mixin initializes correctly."""
        mixin = ErrorHandlingMixin()
        assert hasattr(mixin, 'error_connections')
        assert isinstance(mixin.error_connections, list)
        assert len(mixin.error_connections) == 0

    @pytest.mark.unit
    @pytest.mark.utils
    def test_on_error_registration(self):
        """Test registering error handlers."""
        mixin = ErrorHandlingMixin()

        def error_condition(error: Exception) -> bool:
            return isinstance(error, ValueError)

        mixin.on_error("source_link", "handler_link", error_condition)

        assert len(mixin.error_connections) == 1
        source, handler, condition = mixin.error_connections[0]
        assert source == "source_link"
        assert handler == "handler_link"
        assert condition == error_condition

    @pytest.mark.unit
    @pytest.mark.utils
    def test_error_handler_execution(self):
        """Test that error handlers are executed correctly."""
        mixin = ErrorHandlingMixin()

        # Mock links dictionary
        links = {
            "error_handler": MockLink("mock", result_data={"error_handled": "processed"})
        }
        mixin.links = links  # type: ignore

        def value_error_condition(error: Exception) -> bool:
            return isinstance(error, ValueError)

        mixin.on_error("failing_link", "error_handler", value_error_condition)

        async def run_test():
            ctx = Context({"input": "test"})
            error = ValueError("Test error")

            result_ctx = await mixin._handle_error("failing_link", error, ctx)

            assert result_ctx is not None
            assert result_ctx.get("error_handled") == "processed"

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.utils
    def test_no_matching_error_handler(self):
        """Test behavior when no error handler matches."""
        mixin = ErrorHandlingMixin()

        def type_error_condition(error: Exception) -> bool:
            return isinstance(error, TypeError)

        mixin.on_error("failing_link", "error_handler", type_error_condition)

        async def run_test():
            ctx = Context({"input": "test"})
            error = ValueError("Test error")  # Different type than condition expects

            result_ctx = await mixin._handle_error("failing_link", error, ctx)

            assert result_ctx is None

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.utils
    def test_missing_error_handler_link(self):
        """Test behavior when error handler link doesn't exist."""
        mixin = ErrorHandlingMixin()

        def error_condition(error: Exception) -> bool:
            return True

        mixin.on_error("failing_link", "nonexistent_handler", error_condition)

        async def run_test():
            ctx = Context({"input": "test"})
            error = ValueError("Test error")

            result_ctx = await mixin._handle_error("failing_link", error, ctx)

            assert result_ctx is None

        import asyncio
        asyncio.run(run_test())


class TestRetryLink:
    """Test the RetryLink functionality."""

    @pytest.mark.unit
    @pytest.mark.utils
    def test_successful_first_attempt(self):
        """Test that successful execution doesn't retry."""
        inner_link = MockLink("success", result_data={"result": "success"})
        retry_link = RetryLink(inner_link, max_retries=3)

        async def run_test():
            ctx = Context({"input": "test"})
            result = await retry_link.call(ctx)

            assert result.get("result") == "success"
            assert result.get("input") == "test"

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.utils
    def test_retry_on_failure(self):
        """Test retry behavior when inner link fails."""
        call_count = 0

        class FailingThenSuccessLink:
            async def call(self, ctx: Context) -> Context:
                nonlocal call_count
                call_count += 1
                if call_count < 3:
                    raise ValueError(f"Attempt {call_count} failed")
                return ctx.insert("result", f"success_on_attempt_{call_count}")

        inner_link = FailingThenSuccessLink()
        retry_link = RetryLink(inner_link, max_retries=5)

        async def run_test():
            ctx = Context({"input": "test"})
            result = await retry_link.call(ctx)

            assert call_count == 3
            assert result.get("result") == "success_on_attempt_3"

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.utils
    def test_max_retries_exceeded(self):
        """Test behavior when max retries is exceeded."""
        call_count = 0

        class AlwaysFailingLink:
            async def call(self, ctx: Context) -> Context:
                nonlocal call_count
                call_count += 1
                raise ValueError(f"Attempt {call_count} failed")

        inner_link = AlwaysFailingLink()
        retry_link = RetryLink(inner_link, max_retries=2)

        async def run_test():
            ctx = Context({"input": "test"})
            result = await retry_link.call(ctx)

            assert call_count == 2  # Should try max_retries times
            assert result.get("error") == "Max retries: Attempt 2 failed"
            assert result.get("input") == "test"

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.utils
    def test_zero_max_retries(self):
        """Test behavior with zero max retries."""
        call_count = 0

        class FailingLink:
            async def call(self, ctx: Context) -> Context:
                nonlocal call_count
                call_count += 1
                raise ValueError("Failed")

        inner_link = FailingLink()
        retry_link = RetryLink(inner_link, max_retries=0)

        async def run_test():
            ctx = Context({"input": "test"})
            result = await retry_link.call(ctx)

            assert call_count == 1  # Should try once even with max_retries=0
            assert result.get("error") == "Max retries: Failed"
            assert result.get("input") == "test"

        import asyncio
        asyncio.run(run_test())


class TestErrorHandlingIntegration:
    """Integration tests for error handling functionality."""

    @pytest.mark.integration
    @pytest.mark.utils
    def test_retry_with_error_handling_chain(self):
        """Test combining retry logic with error handling."""
        # Create a chain-like structure with error handling
        class SimpleErrorHandlingChain(ErrorHandlingMixin):
            def __init__(self):
                super().__init__()
                self.links = {}

            def add_link(self, name: str, link):
                self.links[name] = link

            async def run_with_error_handling(self, link_name: str, ctx: Context) -> Context:
                link = self.links.get(link_name)
                if not link:
                    raise ValueError(f"Link {link_name} not found")

                try:
                    return await link.call(ctx)
                except Exception as e:
                    error_ctx = await self._handle_error(link_name, e, ctx)
                    if error_ctx:
                        return error_ctx
                    raise

        # Set up chain with error handling
        chain = SimpleErrorHandlingChain()

        # Add a retry link that will eventually succeed
        call_count = 0
        class IntermittentFailingLink:
            async def call(self, ctx: Context) -> Context:
                nonlocal call_count
                call_count += 1
                if call_count < 2:
                    raise ConnectionError("Temporary network error")
                return ctx.insert("result", "success")

        retry_link = RetryLink(IntermittentFailingLink(), max_retries=3)
        chain.add_link("unreliable_service", retry_link)

        # Add error handler
        class ErrorHandlerLink:
            async def call(self, ctx: Context) -> Context:
                return ctx.insert("error_handled", True).insert("fallback_result", "default")

        chain.add_link("error_handler", ErrorHandlerLink())

        # Register error handler for connection errors
        def connection_error_condition(error: Exception) -> bool:
            return isinstance(error, ConnectionError)

        chain.on_error("unreliable_service", "error_handler", connection_error_condition)

        async def run_test():
            ctx = Context({"input": "test"})
            result = await chain.run_with_error_handling("unreliable_service", ctx)

            # Should have succeeded on retry
            assert result.get("result") == "success"
            assert call_count == 2  # Failed once, succeeded on second try

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.integration
    @pytest.mark.utils
    def test_complex_error_handling_scenario(self):
        """Test complex error handling with multiple handlers and conditions."""
        mixin = ErrorHandlingMixin()

        # Mock different types of links
        links = {
            "validation_handler": MockLink("validation_error_handled", result_data={"result": "validation_error_handled"}),
            "network_handler": MockLink("network_error_handled", result_data={"result": "network_error_handled"}),
            "generic_handler": MockLink("generic_error_handled", result_data={"result": "generic_error_handled"})
        }
        mixin.links = links  # type: ignore

        # Register multiple error handlers with different conditions
        def validation_error_condition(error: Exception) -> bool:
            return "validation" in str(error).lower()

        def network_error_condition(error: Exception) -> bool:
            return isinstance(error, (ConnectionError, TimeoutError))

        def generic_error_condition(error: Exception) -> bool:
            return True  # Catch-all

        mixin.on_error("processor", "validation_handler", validation_error_condition)
        mixin.on_error("processor", "network_handler", network_error_condition)
        mixin.on_error("processor", "generic_handler", generic_error_condition)

        async def run_test():
            ctx = Context({"input": "test"})

            # Test validation error
            validation_error = ValueError("Validation failed: invalid input")
            result1 = await mixin._handle_error("processor", validation_error, ctx)
            assert result1 is not None
            assert result1.get("result") == "validation_error_handled"

            # Test network error
            network_error = ConnectionError("Network timeout")
            result2 = await mixin._handle_error("processor", network_error, ctx)
            assert result2 is not None
            assert result2.get("result") == "network_error_handled"

            # Test generic error
            generic_error = RuntimeError("Unexpected error")
            result3 = await mixin._handle_error("processor", generic_error, ctx)
            assert result3 is not None
            assert result3.get("result") == "generic_error_handled"

        import asyncio
        asyncio.run(run_test())