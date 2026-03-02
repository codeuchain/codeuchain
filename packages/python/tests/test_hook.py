"""
Tests for Hook ABC

Testing the Hook abstract base class with concrete implementations.
"""

import pytest
from abc import ABC
from codeuchain.core.state import State
from codeuchain.core.link import Link
from codeuchain.core.hook import Hook


class TestHookProtocol:
    """Test the Hook ABC interface."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_hook_is_abc(self):
        """Test that Hook is an abstract base class."""
        assert issubclass(Hook, ABC)

    @pytest.mark.unit
    @pytest.mark.core
    def test_hook_abstract_methods(self):
        """Test that Hook has the expected abstract methods."""
        # Hook should have before, after, and on_error methods
        assert hasattr(Hook, 'before')
        assert hasattr(Hook, 'after')
        assert hasattr(Hook, 'on_error')


class LoggingHook(Hook):
    """Concrete hook implementation for testing."""

    def __init__(self):
        self.before_calls = []
        self.after_calls = []
        self.error_calls = []

    async def before(self, link, ctx: State) -> None:
        self.before_calls.append((link, ctx.get("step")))

    async def after(self, link, ctx: State) -> None:
        self.after_calls.append((link, ctx.get("step")))

    async def on_error(self, link, error: Exception, ctx: State) -> None:
        self.error_calls.append((link, str(error), ctx.get("step")))


class TimingHook(Hook):
    """Hook that tracks execution timing."""

    def __init__(self):
        self.timings = {}
        self.start_times = {}

    async def before(self, link, ctx: State) -> None:
        import time
        link_id = "chain" if link is None else id(link)
        self.start_times[link_id] = time.time()

    async def after(self, link, ctx: State) -> None:
        import time
        link_id = "chain" if link is None else id(link)
        if link_id in self.start_times:
            duration = time.time() - self.start_times[link_id]
            self.timings[link_id] = duration

    async def on_error(self, link, error: Exception, ctx: State) -> None:
        # Clean up timing on error
        link_id = "chain" if link is None else id(link)
        if link_id in self.start_times:
            del self.start_times[link_id]


class ValidationHook(Hook):
    """Hook that validates state before and after processing."""

    def __init__(self):
        self.validation_errors = []

    async def before(self, link, ctx: State) -> None:
        # Validate that state has required fields
        if ctx.get("required_field") is None:
            self.validation_errors.append("Missing required_field before processing")

    async def after(self, link, ctx: State) -> None:
        # Validate that processing added expected fields
        if ctx.get("processed") is None:
            self.validation_errors.append("Missing processed field after processing")

    async def on_error(self, link, error: Exception, ctx: State) -> None:
        self.validation_errors.append(f"Error occurred: {str(error)}")


class TestLoggingHook:
    """Test the LoggingHook implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_before_hook(self):
        """Test the before hook logging."""
        hook = LoggingHook()

        async def run_test():
            ctx = State({"step": "init"})
            await hook.before(None, ctx)

            assert len(hook.before_calls) == 1
            assert hook.before_calls[0] == (None, "init")

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_after_hook(self):
        """Test the after hook logging."""
        hook = LoggingHook()

        async def run_test():
            ctx = State({"step": "complete"})
            await hook.after(None, ctx)

            assert len(hook.after_calls) == 1
            assert hook.after_calls[0] == (None, "complete")

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_hook(self):
        """Test the error hook logging."""
        hook = LoggingHook()

        async def run_test():
            ctx = State({"step": "error"})
            error = ValueError("Test error")
            await hook.on_error(None, error, ctx)

            assert len(hook.error_calls) == 1
            assert hook.error_calls[0] == (None, "Test error", "error")

        import asyncio
        asyncio.run(run_test())


class TestTimingHook:
    """Test the TimingHook implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_timing_measurement(self):
        """Test that timing hook measures execution time."""
        hook = TimingHook()

        async def run_test():
            import asyncio

            ctx = State({"step": "test"})

            # Simulate before and after calls
            await hook.before(None, ctx)
            await asyncio.sleep(0.01)  # Small delay
            await hook.after(None, ctx)

            # Check that timing was recorded
            chain_id = "chain"  # None represents chain
            assert chain_id in hook.timings
            assert hook.timings[chain_id] >= 0.01  # Should be at least the sleep time

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_cleanup(self):
        """Test that timing is cleaned up on error."""
        hook = TimingHook()

        async def run_test():
            ctx = State({"step": "test"})

            await hook.before(None, ctx)
            chain_id = "chain"
            assert chain_id in hook.start_times

            # Simulate error
            error = RuntimeError("Test error")
            await hook.on_error(None, error, ctx)

            # Start time should be cleaned up
            assert chain_id not in hook.start_times

        import asyncio
        asyncio.run(run_test())


class TestValidationHook:
    """Test the ValidationHook implementation."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_successful_validation(self):
        """Test validation with valid state."""
        hook = ValidationHook()

        async def run_test():
            # Valid state with required fields
            ctx = State({"required_field": "present", "processed": True})

            await hook.before(None, ctx)
            await hook.after(None, ctx)

            # Should have no validation errors
            assert len(hook.validation_errors) == 0

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_validation_failure_before(self):
        """Test validation failure in before hook."""
        hook = ValidationHook()

        async def run_test():
            # State missing required field
            ctx = State({"other_field": "value"})

            await hook.before(None, ctx)

            assert len(hook.validation_errors) == 1
            assert "Missing required_field" in hook.validation_errors[0]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_validation_failure_after(self):
        """Test validation failure in after hook."""
        hook = ValidationHook()

        async def run_test():
            # State missing processed field
            ctx = State({"required_field": "present"})

            await hook.before(None, ctx)
            await hook.after(None, ctx)

            assert len(hook.validation_errors) == 1
            assert "Missing processed field" in hook.validation_errors[0]

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_error_logging(self):
        """Test error logging in validation hook."""
        hook = ValidationHook()

        async def run_test():
            ctx = State({"required_field": "present"})
            error = ValueError("Processing failed")

            await hook.on_error(None, error, ctx)

            assert len(hook.validation_errors) == 1
            assert "Error occurred: Processing failed" in hook.validation_errors[0]

        import asyncio
        asyncio.run(run_test())


class TestHookIntegration:
    """Integration tests for hook functionality."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_multiple_hook_execution_order(self):
        """Test that multiple hook execute in correct order."""
        hook1 = LoggingHook()
        hook2 = LoggingHook()

        async def run_test():
            ctx = State({"step": "test"})

            # Execute before hooks
            await hook1.before(None, ctx)
            await hook2.before(None, ctx)

            # Execute after hooks
            await hook1.after(None, ctx)
            await hook2.after(None, ctx)

            # Check execution order
            assert len(hook1.before_calls) == 1
            assert len(hook2.before_calls) == 1
            assert len(hook1.after_calls) == 1
            assert len(hook2.after_calls) == 1

        import asyncio
        asyncio.run(run_test())

    @pytest.mark.integration
    @pytest.mark.core
    def test_hook_with_different_states(self):
        """Test hook with different state states."""
        hook = LoggingHook()

        async def run_test():
            ctx1 = State({"step": "start"})
            ctx2 = State({"step": "middle"})
            ctx3 = State({"step": "end"})

            await hook.before(None, ctx1)
            await hook.after(None, ctx2)
            await hook.on_error(None, ValueError("test"), ctx3)

            # Check that different states were logged
            assert hook.before_calls[0][1] == "start"
            assert hook.after_calls[0][1] == "middle"
            assert hook.error_calls[0][2] == "end"

        import asyncio
        asyncio.run(run_test())