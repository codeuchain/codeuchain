"""
Tests for Chain Protocol

Testing the Chain protocol with concrete implementations.
"""

import pytest
import asyncio
from typing import Dict, List, Callable, Optional
from codeuchain.core.state import State
from codeuchain.core.link import Link
from codeuchain.core.chain import Chain
from codeuchain.core.hook import Hook


class LoggingHook(Hook):
    """Simple hook for testing that logs execution."""

    def __init__(self):
        super().__init__()
        self.log = []

    async def before(self, link: Optional[Link], ctx: State) -> None:
        link_name = "chain_start" if link is None else "unknown"
        if link is not None and hasattr(link, 'name'):
            link_name = getattr(link, 'name')
        self.log.append(f"before_{link_name}")

    async def after(self, link: Optional[Link], ctx: State) -> None:
        link_name = "chain_end" if link is None else "unknown"
        if link is not None and hasattr(link, 'name'):
            link_name = getattr(link, 'name')
        self.log.append(f"after_{link_name}")

    async def on_error(self, link: Optional[Link], error: Exception, ctx: State) -> None:
        link_name = "chain" if link is None else "unknown"
        if link is not None and hasattr(link, 'name'):
            link_name = getattr(link, 'name')
        self.log.append(f"error_{link_name}_{str(error)}")


class TestSimpleChain:
    """Test the Chain."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_empty_chain(self):
        """Test running an empty chain."""
        chain = Chain()

        async def run_test():
            ctx = State({"input": "test"})
            result = await chain.run(ctx)
            assert result.get("input") == "test"

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_single_link_chain(self):
        """Test chain with a single link."""
        chain = Chain()

        # Create a simple test link
        class TestLink:
            async def call(self, ctx):
                return ctx.insert("processed", True)

        chain.add_link(TestLink(), "test")

        async def run_test():
            ctx = State({"input": "test"})
            result = await chain.run(ctx)

            assert result.get("processed") is True
            assert result.get("input") == "test"

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_multiple_links_chain(self):
        """Test chain with multiple links."""
        chain = Chain()

        class Link1:
            async def call(self, ctx):
                return ctx.insert("step1", True)

        class Link2:
            async def call(self, ctx):
                return ctx.insert("step2", True)

        chain.add_link(Link1(), "link1")
        chain.add_link(Link2(), "link2")

        async def run_test():
            result = await chain.run(State())
            assert result.get("step1") is True
            assert result.get("step2") is True

        asyncio.run(run_test())


class TestConditionalChain:
    """Test the Chain with conditional execution."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_conditional_execution(self):
        """Test conditional link execution - skipped links must NOT run."""
        chain = Chain()

        class SuccessLink:
            async def call(self, ctx):
                return ctx.insert("success", True)

        class FailureLink:
            async def call(self, ctx):
                return ctx.insert("failure", True)

        chain.add_link(SuccessLink(), "validate")
        chain.add_link(SuccessLink(), "success_path")
        chain.add_link(FailureLink(), "failure_path")

        # Connect with conditions
        chain.connect("validate", "success_path", lambda ctx: ctx.get("success") is True)
        chain.connect("validate", "failure_path", lambda ctx: ctx.get("success") is not True)

        async def run_test():
            result = await chain.run(State())
            assert result.get("success") is True
            # failure_path predicate evaluates False, so failure link must NOT have run
            assert result.get("failure") is None

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_predicate_false_skips_link(self):
        """A link whose predicate is always False must be skipped entirely."""
        chain = Chain()
        executed = []

        class FirstLink:
            async def call(self, ctx):
                executed.append("first")
                return ctx.insert("first_ran", True)

        class GatedLink:
            async def call(self, ctx):
                executed.append("gated")
                return ctx.insert("gated_ran", True)

        chain.add_link(FirstLink(), "first")
        chain.add_link(GatedLink(), "gated")
        chain.connect("first", "gated", lambda ctx: False)

        async def run_test():
            result = await chain.run(State())
            assert "first" in executed
            assert "gated" not in executed
            assert result.get("first_ran") is True
            assert result.get("gated_ran") is None

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_predicate_true_executes_link(self):
        """A link whose predicate evaluates to True must execute."""
        chain = Chain()

        class FirstLink:
            async def call(self, ctx):
                return ctx.insert("value", 42)

        class GatedLink:
            async def call(self, ctx):
                return ctx.insert("gated_ran", True)

        chain.add_link(FirstLink(), "first")
        chain.add_link(GatedLink(), "gated")
        chain.connect("first", "gated", lambda ctx: ctx.get("value") == 42)

        async def run_test():
            result = await chain.run(State())
            assert result.get("gated_ran") is True

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_link_without_connections_always_runs(self):
        """Links with no incoming connections are always executed."""
        chain = Chain()

        class AlwaysLink:
            async def call(self, ctx):
                return ctx.insert("always_ran", True)

        chain.add_link(AlwaysLink(), "always")

        async def run_test():
            result = await chain.run(State())
            assert result.get("always_ran") is True

        asyncio.run(run_test())


class TestChainWithHook:
    """Test chains with hook."""

    @pytest.mark.unit
    @pytest.mark.core
    def test_hook_execution(self):
        """Test that hook hooks are called."""
        chain = Chain()
        hook = LoggingHook()

        class TestLink:
            def __init__(self, name):
                self.name = name
            async def call(self, ctx):
                return ctx

        chain.use_hook(hook)
        chain.add_link(TestLink("test_link"), "test")

        async def run_test():
            await chain.run(State())

            # Check that hook was called
            assert "before_chain_start" in hook.log
            assert "before_test_link" in hook.log
            assert "after_test_link" in hook.log
            assert "after_chain_end" in hook.log

        asyncio.run(run_test())

    @pytest.mark.unit
    @pytest.mark.core
    def test_hook_error_handling(self):
        """Test hook error handling."""
        chain = Chain()
        hook = LoggingHook()

        class FailingLink:
            async def call(self, ctx):
                raise ValueError("Test error")

        chain.use_hook(hook)
        chain.add_link(FailingLink(), "failing")

        async def run_test():
            with pytest.raises(ValueError):
                await chain.run(State())

            # Check error was logged
            assert any("error" in entry for entry in hook.log)

        asyncio.run(run_test())


class TestChainIntegration:
    """Integration tests for chain functionality."""

    @pytest.mark.integration
    @pytest.mark.core
    def test_complete_workflow(self):
        """Test a complete workflow with validation, processing, and hook."""
        chain = Chain()
        hook = LoggingHook()

        class ValidationLink:
            async def call(self, ctx):
                data = ctx.get("data")
                if not data:
                    raise ValueError("No data provided")
                return ctx.insert("validated", True)

        class ProcessingLink:
            async def call(self, ctx):
                data = ctx.get("data")
                processed = f"processed_{data}"
                return ctx.insert("result", processed)

        chain.use_hook(hook)
        chain.add_link(ValidationLink(), "validate")
        chain.add_link(ProcessingLink(), "process")

        async def run_test():
            ctx = State({"data": "test_input"})
            result = await chain.run(ctx)

            assert result.get("validated") is True
            assert result.get("result") == "processed_test_input"
            assert result.get("data") == "test_input"

            # Check hook execution
            assert len(hook.log) > 0

        asyncio.run(run_test())

    @pytest.mark.integration
    @pytest.mark.core
    def test_error_propagation(self):
        """Test error propagation through the chain."""
        chain = Chain()

        class FailingLink:
            async def call(self, ctx):
                raise RuntimeError("Processing failed")

        chain.add_link(FailingLink(), "fail")

        async def run_test():
            with pytest.raises(RuntimeError, match="Processing failed"):
                await chain.run(State({"input": "test"}))

        asyncio.run(run_test())