"""
Typed Tests for Opt-in Generics
Enhanced with comprehensive testing of generic type features.
"""

from typing import List, TypedDict, Optional

import pytest

from codeuchain.core import Chain, Context, Link


class InputData(TypedDict):
    numbers: List[int]
    operation: str


class OutputData(InputData):
    result: float


class SumLink(Link[InputData, OutputData]):
    async def call(self, ctx: Context[InputData]) -> Context[OutputData]:
        numbers = ctx.get("numbers") or []
        total = sum(numbers)
        # Use insert_as to evolve the type from InputData to OutputData
        return ctx.insert_as("result", float(total))  # type: ignore


class TestTypedBasics:
    @pytest.mark.unit
    def test_typed_context_creation(self):
        """Test creating a typed context."""
        data: InputData = {"numbers": [1, 2, 3], "operation": "sum"}
        ctx: Context[InputData] = Context(data)
        assert ctx.get("numbers") == [1, 2, 3]

    @pytest.mark.unit
    def test_typed_link_execution(self):
        """Test executing a typed link."""
        link = SumLink()
        input_data: InputData = {"numbers": [1, 2, 3, 4], "operation": "sum"}
        ctx: Context[InputData] = Context(input_data)

        import asyncio
        result_ctx = asyncio.run(link.call(ctx))
        assert result_ctx.get("result") == 10.0

    @pytest.mark.unit
    def test_typed_chain_execution(self):
        """Test executing a typed chain."""
        chain: Chain[InputData, OutputData] = Chain()
        chain.add_link(SumLink(), "sum")

        input_data: InputData = {"numbers": [2, 4, 6, 8], "operation": "stats"}
        ctx: Context[InputData] = Context(input_data)

        import asyncio
        result_ctx = asyncio.run(chain.run(ctx))
        assert result_ctx.get("result") == 20.0


class TestGenericTypeEvolution:
    """Test generic type evolution features."""

    @pytest.mark.unit
    def test_context_type_evolution(self):
        """Test that Context supports type evolution with insert_as."""

        class InitialData(TypedDict):
            name: str

        class EvolvedData(TypedDict):
            name: str
            age: int

        initial: InitialData = {"name": "Alice"}
        ctx: Context[InitialData] = Context(initial)

        # Evolve the context type
        evolved_ctx = ctx.insert_as("age", 30)

        # Verify the evolution worked
        assert evolved_ctx.get("name") == "Alice"
        assert evolved_ctx.get("age") == 30

    @pytest.mark.unit
    def test_generic_context_operations(self):
        """Test generic Context operations maintain type safety."""

        class TestData(TypedDict):
            value: int

        data: TestData = {"value": 42}
        ctx: Context[TestData] = Context(data)

        # Test get operation
        assert ctx.get("value") == 42
        assert ctx.get("missing") is None

        # Test insert operation
        new_ctx = ctx.insert("new_field", "test")
        assert new_ctx.get("value") == 42
        assert new_ctx.get("new_field") == "test"

        # Test merge operation
        other_data: TestData = {"value": 100}
        other_ctx: Context[TestData] = Context(other_data)
        merged_ctx = ctx.merge(other_ctx)
        assert merged_ctx.get("value") == 100  # other_ctx takes precedence

    @pytest.mark.unit
    def test_mutable_context_generic(self):
        """Test MutableContext with generic typing."""

        class TestData(TypedDict):
            counter: int

        data: TestData = {"counter": 0}
        mutable_ctx = Context(data).with_mutation()

        # Test mutable operations
        mutable_ctx.set("counter", 5)  # type: ignore
        assert mutable_ctx.get("counter") == 5

        # Test conversion back to immutable
        immutable_ctx = mutable_ctx.to_immutable()  # type: ignore
        assert immutable_ctx.get("counter") == 5


class TestTypedWorkflows:
    """Test complete typed workflows."""

    @pytest.mark.unit
    def test_typed_data_processing_pipeline(self):
        """Test a complete typed data processing pipeline."""

        class RawData(TypedDict):
            raw_values: List[str]

        class ParsedData(TypedDict):
            raw_values: List[str]
            parsed_numbers: List[int]

        class ProcessedData(TypedDict):
            raw_values: List[str]
            parsed_numbers: List[int]
            sum: int
            average: float

        class ParseLink(Link[RawData, ParsedData]):
            async def call(self, ctx: Context[RawData]) -> Context[ParsedData]:
                raw_values = ctx.get("raw_values") or []
                parsed_numbers = [int(x) for x in raw_values if x.isdigit()]
                return ctx.insert_as("parsed_numbers", parsed_numbers)  # type: ignore

        class ProcessLink(Link[ParsedData, ProcessedData]):
            async def call(self, ctx: Context[ParsedData]) -> Context[ProcessedData]:
                numbers = ctx.get("parsed_numbers") or []
                total = sum(numbers)
                avg = total / len(numbers) if numbers else 0.0
                return ctx.insert_as("sum", total).insert_as("average", avg)  # type: ignore

        # Create and execute the pipeline
        chain: Chain = Chain()  # Use untyped chain for flexibility
        chain.add_link(ParseLink(), "parse")
        chain.add_link(ProcessLink(), "process")

        input_data: RawData = {"raw_values": ["1", "2", "3", "4", "5"]}
        ctx: Context[RawData] = Context(input_data)

        import asyncio
        result_ctx = asyncio.run(chain.run(ctx))

        # Verify results
        assert result_ctx.get("parsed_numbers") == [1, 2, 3, 4, 5]
        assert result_ctx.get("sum") == 15
        assert result_ctx.get("average") == 3.0

    @pytest.mark.unit
    def test_typed_error_handling(self):
        """Test typed error handling in workflows."""

        class InputData(TypedDict):
            value: Optional[int]

        class OutputData(TypedDict):
            value: Optional[int]
            error: Optional[str]

        class ValidateLink(Link[InputData, OutputData]):
            async def call(self, ctx: Context[InputData]) -> Context[OutputData]:
                value = ctx.get("value")
                if value is None:
                    return ctx.insert_as("error", "Value is required")  # type: ignore
                if not isinstance(value, int):
                    return ctx.insert_as("error", "Value must be an integer")  # type: ignore
                if value < 0:
                    return ctx.insert_as("error", "Value must be non-negative")  # type: ignore
                return ctx.insert_as("error", None)  # type: ignore

        # Test valid input
        valid_input: InputData = {"value": 42}
        ctx: Context[InputData] = Context(valid_input)

        link = ValidateLink()
        import asyncio
        result_ctx = asyncio.run(link.call(ctx))
        assert result_ctx.get("error") is None

        # Test invalid input
        invalid_input: InputData = {"value": -1}
        ctx2: Context[InputData] = Context(invalid_input)
        result_ctx2 = asyncio.run(link.call(ctx2))
        assert result_ctx2.get("error") == "Value must be non-negative"


class TestBackwardCompatibility:
    """Test that generic enhancements don't break existing untyped code."""

    @pytest.mark.unit
    def test_untyped_context_still_works(self):
        """Test that untyped Context usage still works."""
        ctx = Context({"key": "value"})
        assert ctx.get("key") == "value"

        new_ctx = ctx.insert("new_key", "new_value")
        assert new_ctx.get("new_key") == "new_value"

    @pytest.mark.unit
    def test_mixed_typed_untyped_chains(self):
        """Test mixing typed and untyped components in chains."""

        class SimpleLink(Link):
            async def call(self, ctx: Context) -> Context:
                value = ctx.get("input") or 0
                return ctx.insert("output", value * 2)

        # Create a chain with mixed typing
        chain = Chain()  # Untyped chain
        chain.add_link(SimpleLink(), "double")

        ctx = Context({"input": 5})
        import asyncio
        result_ctx = asyncio.run(chain.run(ctx))
        assert result_ctx.get("output") == 10
