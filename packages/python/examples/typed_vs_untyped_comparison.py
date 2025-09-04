"""
CodeUChain: Typed vs Untyped Approaches Comparison

This example demonstrates the two ways to use CodeUChain:

1. UNTYPED (Default): Runtime-only with Dict[str, Any] - flexible but no static checking
2. TYPED (Opt-in): Static typing with TypedDict and generics - type safety with some ceremony

Both approaches accomplish the same work but with different trade-offs.
"""

import asyncio
from typing import List, TypedDict

from codeuchain.core import Chain, Context, Link

# =============================================================================
# SHARED BUSINESS LOGIC: Math processing functions
# =============================================================================

def calculate_sum(numbers: List[int]) -> float:
    """Calculate sum of numbers."""
    return float(sum(numbers))

def calculate_average(numbers: List[int]) -> float:
    """Calculate average of numbers."""
    return float(sum(numbers) / len(numbers)) if numbers else 0.0

def validate_numbers(data: dict) -> bool:
    """Validate that numbers field exists and is a list."""
    numbers = data.get("numbers")
    return isinstance(numbers, list) and len(numbers) > 0

# =============================================================================
# APPROACH 1: UNTYPED (Default CodeUChain Components)
# =============================================================================

class UntypedSumLink(Link):
    """
    Untyped link using default CodeUChain approach.

    - No type annotations on Context
    - Runtime Dict[str, Any] behavior
    - Flexible but no static type checking
    - Uses ctx.get() with runtime type checking
    """

    async def call(self, ctx: Context) -> Context:
        # Runtime validation - no static guarantees
        data = ctx.to_dict()
        if not validate_numbers(data):
            return ctx.insert("error", "Invalid or missing numbers")

        numbers = data["numbers"]  # We know this exists from validation
        result = calculate_sum(numbers)
        return ctx.insert("sum", result)


class UntypedAverageLink(Link):
    """
    Untyped link for calculating averages.

    - Depends on previous link's output
    - Runtime error handling
    - No static guarantees about data shape
    """

    async def call(self, ctx: Context) -> Context:
        # Check if we have numbers to work with
        data = ctx.to_dict()
        if not validate_numbers(data):
            return ctx.insert("error", "Invalid or missing numbers")

        numbers = data["numbers"]
        result = calculate_average(numbers)

        # Could also use the sum if it exists
        existing_sum = ctx.get("sum")
        if existing_sum is not None and isinstance(existing_sum, (int, float)):
            # Verify consistency
            calculated_sum = calculate_sum(numbers)
            if abs(existing_sum - calculated_sum) > 0.001:
                return ctx.insert("error", "Sum mismatch detected")

        return ctx.insert("average", result)


class UntypedStatsChain:
    """
    Untyped chain implementation.

    - No generic type parameters
    - Runtime composition
    - Flexible but error-prone
    """

    def __init__(self):
        self.chain = Chain()
        self.chain.add_link(UntypedSumLink(), "sum")
        self.chain.add_link(UntypedAverageLink(), "average")

        # Conditional connection - only calculate average if sum succeeded
        self.chain.connect("sum", "average", lambda ctx: ctx.get("error") is None)

    async def run(self, ctx: Context) -> Context:
        return await self.chain.run(ctx)


# =============================================================================
# APPROACH 2: TYPED (Opt-in Generics)
# =============================================================================

class MathInput(TypedDict):
    """Input data shape for math operations."""
    numbers: List[int]


class SumOutput(TypedDict):
    """Output after sum calculation."""
    numbers: List[int]
    sum: float


class StatsOutput(TypedDict):
    """Final output with all statistics."""
    numbers: List[int]
    sum: float
    average: float


class TypedSumLink(Link[MathInput, SumOutput]):
    """
    Typed link using opt-in generics.

    - Static type checking with TypedDict
    - Compile-time guarantees about data shape
    - Type-safe context operations
    - Clear input/output contracts
    """

    async def call(self, ctx: Context[MathInput]) -> Context[SumOutput]:
        # Static type checker knows ctx contains MathInput
        numbers = ctx.get("numbers")  # Type: List[int] | None

        if numbers is None or not numbers:
            # Type-safe error handling
            raise ValueError("Numbers list is required and cannot be empty")

        result = calculate_sum(numbers)
        # insert_as() allows type evolution without casting
        return ctx.insert_as("sum", result)


class TypedAverageLink(Link[SumOutput, StatsOutput]):
    """
    Typed link for calculating averages.

    - Input type guarantees sum field exists
    - Output type extends input with average
    - Static verification of data flow
    """

    async def call(self, ctx: Context[SumOutput]) -> Context[StatsOutput]:
        # Type checker knows we have SumOutput shape
        numbers = ctx.get("numbers")  # Guaranteed to be List[int]
        existing_sum = ctx.get("sum")  # Guaranteed to be float

        # Runtime validation (belt and suspenders)
        if not numbers:
            raise ValueError("Numbers list cannot be empty")

        # Calculate average
        calculated_avg = calculate_average(numbers)

        # Optional: Verify sum consistency
        calculated_sum = calculate_sum(numbers)
        if abs(existing_sum - calculated_sum) > 0.001:
            raise ValueError("Sum consistency check failed")

        return ctx.insert_as("average", calculated_avg)


class TypedStatsChain:
    """
    Typed chain with full type safety.

    - Generic type parameters for input/output
    - Static verification of link compatibility
    - Type-safe chain composition
    """

    def __init__(self):
        self.chain: Chain[MathInput, StatsOutput] = Chain()
        self.chain.add_link(TypedSumLink(), "sum")
        self.chain.add_link(TypedAverageLink(), "average")

    async def run(self, ctx: Context[MathInput]) -> Context[StatsOutput]:
        return await self.chain.run(ctx)


# =============================================================================
# DEMONSTRATION: Side-by-side comparison
# =============================================================================

async def demonstrate_both_approaches():
    """Demonstrate both typed and untyped approaches doing the same work."""

    print("=== CodeUChain: Typed vs Untyped Approaches ===\n")

    # Test data
    test_cases = [
        {"numbers": [1, 2, 3, 4, 5]},      # Normal case
        {"numbers": []},                   # Edge case: empty list
        {"numbers": [10, 20, 30]},        # Another normal case
    ]

    for i, test_data in enumerate(test_cases, 1):
        print(f"--- Test Case {i}: {test_data} ---")
        print()

        # =============================================================================
        # Approach 1: Untyped (Runtime-only)
        # =============================================================================

        print("🔄 UNTYPED APPROACH (Default CodeUChain):")
        print("   • No static type checking")
        print("   • Runtime Dict[str, Any] behavior")
        print("   • Flexible but error-prone")

        untyped_chain = UntypedStatsChain()
        untyped_ctx = Context(test_data)

        try:
            untyped_result = await untyped_chain.run(untyped_ctx)
            result_data = untyped_result.to_dict()
            print("   ✅ Success:")
            print(f"      Result: {result_data}")

            # Show what we got
            if "error" in result_data:
                print(f"      ⚠️  Error: {result_data['error']}")
            else:
                print(f"      📊 Sum: {result_data.get('sum', 'N/A')}")
                print(f"      📊 Average: {result_data.get('average', 'N/A')}")

        except Exception as e:
            print(f"   ❌ Runtime Error: {e}")

        print()

        # =============================================================================
        # Approach 2: Typed (Opt-in Generics)
        # =============================================================================

        print("�� TYPED APPROACH (Opt-in Generics):")
        print("   • Static type checking with TypedDict")
        print("   • Compile-time guarantees")
        print("   • Type-safe context evolution")

        # Only run typed approach for valid inputs (it will catch errors at type level)
        if test_data["numbers"]:  # Skip empty list for typed approach
            typed_chain = TypedStatsChain()
            typed_ctx = Context[MathInput](test_data)

            try:
                typed_result = await typed_chain.run(typed_ctx)
                result_data = typed_result.to_dict()
                print("   ✅ Success:")
                print(f"      Result: {result_data}")
                print(f"      📊 Sum: {result_data.get('sum')}")
                print(f"      📊 Average: {result_data.get('average')}")

            except Exception as e:
                print(f"   ❌ Error: {e}")
        else:
            print("   ⏭️  Skipped (empty list would cause typed validation error)")

        print("\n" + "="*60 + "\n")


# =============================================================================
# FEATURE COMPARISON SUMMARY
# =============================================================================

def print_feature_comparison():
    """Print a detailed comparison of both approaches."""

    print("=== FEATURE COMPARISON ===")
    print()

    features = [
        ("Type Safety", "Runtime only", "Compile-time with TypedDict"),
        ("Error Detection", "Runtime exceptions", "Static analysis + runtime"),
        ("IDE Support", "Basic autocomplete", "Full IntelliSense + refactoring"),
        ("Documentation", "Runtime behavior", "Explicit data contracts"),
        ("Flexibility", "Any data structure", "Defined TypedDict shapes"),
        ("Performance", "Same runtime cost", "Same runtime cost"),
        ("Learning Curve", "Easy to start", "Some typing ceremony"),
        ("Refactoring", "Error-prone", "Type-safe with IDE support"),
        ("Testing", "Runtime assertions", "Type contracts + runtime tests"),
        ("Maintenance", "Informal contracts", "Formal type specifications"),
    ]

    print("Feature".ljust(20) + "|" + "Untyped".ljust(20) + "|" + "Typed".ljust(25))
    print("-" * 67)

    for feature, untyped, typed in features:
        print(f"{feature:<20}|{untyped:<20}|{typed:<25}")

    print()
    print("=== RECOMMENDATIONS ===")
    print()
    print("Use UNTYPED when:")
    print("  • Prototyping or exploring ideas")
    print("  • Working with highly dynamic data")
    print("  • Team prefers runtime flexibility")
    print("  • Simple scripts or one-off tasks")
    print()
    print("Use TYPED when:")
    print("  • Building production systems")
    print("  • Working in larger teams")
    print("  • Data contracts are well-defined")
    print("  • Long-term maintenance is important")
    print("  • IDE support and refactoring matter")
    print()
    print("Both approaches work together - you can mix typed and untyped")
    print("components in the same chain based on your needs!")


if __name__ == "__main__":
    # Run the demonstration
    asyncio.run(demonstrate_both_approaches())

    # Print feature comparison
    print_feature_comparison()
