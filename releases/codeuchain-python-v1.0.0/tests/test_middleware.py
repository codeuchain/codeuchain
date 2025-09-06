"""
Tests for Middleware ABC

Testing the Middleware abstract base class with concrete implementations.
"""

import pytest
from abc import ABC
from codeuchain.core.context import Context
from codeuchain.core.link import Link
from codeuchain.core.middleware import Middleware


class TestMiddlewareProtocol:
    """Test the Middleware ABC interface."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_middleware_is_abc(self):
        """Test that Middleware is an abstract base class."""
        assert issubclass(Middleware, ABC)

    @pytest.mark.unit
    @pytest.mark.core
    def test_middleware_abstract_methods(self):
        """Test that Middleware has the expected abstract methods."""
        # Middleware should have before, after, and on_error methods
        assert hasattr(Middleware, 'before')
        assert hasattr(Middleware, 'after')
        assert hasattr(Middleware, 'on_error')


class LoggingMiddleware(Middleware):
    """Concrete middleware implementation for testing."""

    def __init__(self):
        self.before_calls = []
        self.after_calls = []
        self.error_calls = []

    async def before(self, link, ctx: Context) -> None:
        self.before_calls.append((link, ctx.get("step")))

    async def after(self, link, ctx: Context) -> None:
        self.after_calls.append((link, ctx.get("step")))

    async def on_error(self, link, error: Exception, ctx: Context) -> None:
        self.error_calls.append((link, str(error), ctx.get("step")))


class TimingMiddleware(Middleware):
    """Middleware that tracks execution timing."""

    def __init__(self):
        self.timings = {}
        self.start_times = {}

    async def before(self, link, ctx: Context) -> None:
        import time
        link_id = "chain" if link is None else id(link)
        self.start_times[link_id] = time.time()

    async def after(self, link, ctx: Context) -> None:
        import time
        link_id = "chain" if link is None else id(link)
        if link_id in self.start_times:
            duration = time.time() - self.start_times[link_id]
            self.timings[link_id] = duration

    async def on_error(self, link, error: Exception, ctx: Context) -> None:
        # Clean up timing on error
        link_id = "chain" if link is None else id(link)
        if link_id in self.start_times:
            del self.start_times[link_id]


class ValidationMiddleware(Middleware):
    """Middleware that validates context before and after processing."""

    def __init__(self):
        self.validation_errors = []

    async def before(self, link, ctx: Context) -> None:
        # Validate that context has required fields
        if ctx.get("required_field") is None:
            self.validation_errors.append("Missing required_field before processing")

    async def after(self, link, ctx: Context) -> None:
        # Validate that processing added expected fields
        if ctx.get("processed") is None:
            self.validation_errors.append("Missing processed field after processing")

    async def on_error(self, link, error: Exception, ctx: Context) -> None:
        self.validation_errors.append(f"Error occurred: {str(error)}")


class TestLoggingMiddleware:
    """Test the LoggingMiddleware implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_before_hook(self):
        """Test the before hook logging."""
        middleware = LoggingMiddleware()

        async def run_test():
            ctx = Context({"step": "init"})
            await middleware.before(None, ctx)

            assert len(middleware.before_calls) == 1
            assert middleware.before_calls[0] == (None, "init")

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_after_hook(self):
        """Test the after hook logging."""
        middleware = LoggingMiddleware()

        async def run_test():
            ctx = Context({"step": "complete"})
            await middleware.after(None, ctx)

            assert len(middleware.after_calls) == 1
            assert middleware.after_calls[0] == (None, "complete")

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_hook(self):
        """Test the error hook logging."""
        middleware = LoggingMiddleware()

        async def run_test():
            ctx = Context({"step": "error"})
            error = ValueError("Test error")
            await middleware.on_error(None, error, ctx)

            assert len(middleware.error_calls) == 1
            assert middleware.error_calls[0] == (None, "Test error", "error")

        import asyncio
        asyncio.run(run_test())


class TestTimingMiddleware:
    """Test the TimingMiddleware implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_timing_measurement(self):
        """Test that timing middleware measures execution time."""
        middleware = TimingMiddleware()

        async def run_test():
            import asyncio

            ctx = Context({"step": "test"})

            # Simulate before and after calls
            await middleware.before(None, ctx)
            await asyncio.sleep(0.01)  # Small delay
            await middleware.after(None, ctx)

            # Check that timing was recorded
            chain_id = "chain"  # None represents chain
            assert chain_id in middleware.timings
            assert middleware.timings[chain_id] >= 0.01  # Should be at least the sleep time

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_cleanup(self):
        """Test that timing is cleaned up on error."""
        middleware = TimingMiddleware()

        async def run_test():
            ctx = Context({"step": "test"})

            await middleware.before(None, ctx)
            chain_id = "chain"
            assert chain_id in middleware.start_times

            # Simulate error
            error = RuntimeError("Test error")
            await middleware.on_error(None, error, ctx)

            # Start time should be cleaned up
            assert chain_id not in middleware.start_times

        import asyncio
        asyncio.run(run_test())


class TestValidationMiddleware:
    """Test the ValidationMiddleware implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_successful_validation(self):
        """Test validation with valid context."""
        middleware = ValidationMiddleware()

        async def run_test():
            # Valid context with required fields
            ctx = Context({"required_field": "present", "processed": True})

            await middleware.before(None, ctx)
            await middleware.after(None, ctx)

            # Should have no validation errors
            assert len(middleware.validation_errors) == 0

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_validation_failure_before(self):
        """Test validation failure in before hook."""
        middleware = ValidationMiddleware()

        async def run_test():
            # Context missing required field
            ctx = Context({"other_field": "value"})

            await middleware.before(None, ctx)

            assert len(middleware.validation_errors) == 1
            assert "Missing required_field" in middleware.validation_errors[0]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_validation_failure_after(self):
        """Test validation failure in after hook."""
        middleware = ValidationMiddleware()

        async def run_test():
            # Context missing processed field
            ctx = Context({"required_field": "present"})

            await middleware.before(None, ctx)
            await middleware.after(None, ctx)

            assert len(middleware.validation_errors) == 1
            assert "Missing processed field" in middleware.validation_errors[0]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_logging(self):
        """Test error logging in validation middleware."""
        middleware = ValidationMiddleware()

        async def run_test():
            ctx = Context({"required_field": "present"})
            error = ValueError("Processing failed")

            await middleware.on_error(None, error, ctx)

            assert len(middleware.validation_errors) == 1
            assert "Error occurred: Processing failed" in middleware.validation_errors[0]

        import asyncio
        asyncio.run(run_test())


class TestMiddlewareIntegration:
    """Integration tests for middleware functionality."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_multiple_middleware_execution_order(self):
        """Test that multiple middleware execute in correct order."""
        middleware1 = LoggingMiddleware()
        middleware2 = LoggingMiddleware()

        async def run_test():
            ctx = Context({"step": "test"})

            # Execute before hooks
            await middleware1.before(None, ctx)
            await middleware2.before(None, ctx)

            # Execute after hooks
            await middleware1.after(None, ctx)
            await middleware2.after(None, ctx)

            # Check execution order
            assert len(middleware1.before_calls) == 1
            assert len(middleware2.before_calls) == 1
            assert len(middleware1.after_calls) == 1
            assert len(middleware2.after_calls) == 1

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.integration
    @pytest.mark.core
    def test_middleware_with_different_contexts(self):
        """Test middleware with different context states."""
        middleware = LoggingMiddleware()

        async def run_test():
            ctx1 = Context({"step": "start"})
            ctx2 = Context({"step": "middle"})
            ctx3 = Context({"step": "end"})

            await middleware.before(None, ctx1)
            await middleware.after(None, ctx2)
            await middleware.on_error(None, ValueError("test"), ctx3)

            # Check that different contexts were logged
            assert middleware.before_calls[0][1] == "start"
            assert middleware.after_calls[0][1] == "middle"
            assert middleware.error_calls[0][2] == "end"

        import asyncio
        asyncio.run(run_test())