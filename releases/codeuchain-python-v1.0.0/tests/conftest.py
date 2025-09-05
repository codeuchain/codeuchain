"""
Pytest configuration and shared fixtures for CodeUChain Python tests.
"""

import pytest
import asyncio
from typing import Dict, Any, Optional, AsyncGenerator
from codeuchain.core.context import Context, MutableContext


@pytest.fixture
def sample_context() -> Context:
    """Fixture providing a sample context with test data."""
    return Context({
        "user_id": 123,
        "name": "Alice",
        "email": "alice@example.com",
        "active": True
    })


@pytest.fixture
def empty_context() -> Context:
    """Fixture providing an empty context."""
    return Context()


@pytest.fixture
def mutable_context() -> MutableContext:
    """Fixture providing a mutable context with test data."""
    return MutableContext({
        "counter": 0,
        "status": "init"
    })


@pytest.fixture
def event_loop():
    """Fixture providing an event loop for async tests."""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


@pytest.fixture
async def async_context() -> AsyncGenerator[Context, None]:
    """Async fixture providing a context for async tests."""
    ctx = Context({"async_test": True, "step": "setup"})
    yield ctx


class MockLink:
    """Mock Link implementation for testing."""

    def __init__(self, name: str = "mock", should_fail: bool = False, result_data: Optional[Dict[str, Any]] = None):
        self.name = name
        self.should_fail = should_fail
        self.result_data = result_data or {"processed": True}
        self.call_count = 0

    async def call(self, ctx: Context) -> Context:
        self.call_count += 1

        if self.should_fail:
            raise ValueError(f"Mock link {self.name} failed on call {self.call_count}")

        result_ctx = ctx
        for key, value in self.result_data.items():
            result_ctx = result_ctx.insert(key, value)

        return result_ctx.insert("link_name", self.name)


@pytest.fixture
def mock_link():
    """Fixture providing a basic mock link."""
    return MockLink("test_link")


@pytest.fixture
def failing_link():
    """Fixture providing a mock link that always fails."""
    return MockLink("failing_link", should_fail=True)


@pytest.fixture
def processing_link():
    """Fixture providing a mock link that adds processing results."""
    return MockLink("processor", result_data={"processed_data": "result", "status": "complete"})


# Test utilities
def run_async(coro):
    """Helper to run async functions in sync tests."""
    return asyncio.run(coro)


def assert_context_contains(ctx: Context, expected_data: dict):
    """Assert that context contains all expected key-value pairs."""
    for key, expected_value in expected_data.items():
        actual_value = ctx.get(key)
        assert actual_value == expected_value, f"Expected {key}={expected_value}, got {actual_value}"


def assert_context_immutable(original: Context, modified: Context):
    """Assert that original context was not modified when creating modified version."""
    # This is a basic check - in practice, you'd need deep comparison
    assert original is not modified, "Contexts should be different objects"