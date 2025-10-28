"""
CodeUChain Converter Example

Demonstrates converting between CodeUChain chains and traditional imperative code.
Shows the zero-overhead compilation feature.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import asyncio
from codeuchain.core import Context, Chain
from codeuchain.utils import ChainConverter


# Define example links
class ValidationLink:
    """Validates input data."""
    
    async def call(self, ctx: Context) -> Context:
        value = ctx.get('value')
        
        if value is None:
            return ctx.insert('valid', False).insert('error', 'Value is required')
        
        if not isinstance(value, (int, float)):
            return ctx.insert('valid', False).insert('error', 'Value must be a number')
        
        if value < 0:
            return ctx.insert('valid', False).insert('error', 'Value must be positive')
        
        return ctx.insert('valid', True)


class ProcessingLink:
    """Processes validated data."""
    
    async def call(self, ctx: Context) -> Context:
        if not ctx.get('valid'):
            return ctx  # Skip processing if invalid
        
        value = ctx.get('value')
        squared = value ** 2
        
        return ctx.insert('processed', squared).insert('original', value)


class FormattingLink:
    """Formats the result."""
    
    async def call(self, ctx: Context) -> Context:
        if not ctx.get('valid'):
            error = ctx.get('error')
            return ctx.insert('message', f'Error: {error}')
        
        original = ctx.get('original')
        processed = ctx.get('processed')
        
        message = f'The square of {original} is {processed}'
        return ctx.insert('message', message)


async def main():
    print("=" * 70)
    print("CodeUChain Converter Example: Zero-Overhead Compilation")
    print("=" * 70)
    print()
    
    # ===== Step 1: Create a CodeUChain =====
    print("Step 1: Creating a CodeUChain...")
    print("-" * 70)
    
    chain = Chain()
    chain.add_link(ValidationLink(), "validate")
    chain.add_link(ProcessingLink(), "process")
    chain.add_link(FormattingLink(), "format")
    
    print("✓ Chain created with 3 links: validate → process → format")
    print()
    
    # ===== Step 2: Test the original chain =====
    print("Step 2: Testing the original chain...")
    print("-" * 70)
    
    test_cases = [
        {'value': 5},
        {'value': 10},
        {'value': -3},
        {'value': 'invalid'},
    ]
    
    print("\nOriginal Chain Results:")
    for i, test_data in enumerate(test_cases):
        result = await chain.run(Context(test_data))
        print(f"  Test {i+1}: {test_data} → {result.get('message')}")
    print()
    
    # ===== Step 3: Convert chain to traditional code =====
    print("Step 3: Converting chain to zero-overhead traditional code...")
    print("-" * 70)
    
    converter = ChainConverter()
    traditional_code = converter.chain_to_code(chain, function_name="execute_pipeline")
    
    print("\nGenerated Code Preview (first 50 lines):")
    lines = traditional_code.split('\n')
    for line in lines[:50]:
        print(f"  {line}")
    
    if len(lines) > 50:
        print(f"  ... ({len(lines) - 50} more lines)")
    print()
    
    # ===== Step 4: Execute the generated code =====
    print("Step 4: Executing the generated traditional code...")
    print("-" * 70)
    
    # Prepare namespace for execution
    namespace = {
        'Context': Context,
        'ValidationLink': ValidationLink,
        'ProcessingLink': ProcessingLink,
        'FormattingLink': FormattingLink
    }
    
    # Execute the generated code
    exec(traditional_code, namespace)
    execute_pipeline = namespace['execute_pipeline']
    
    print("\nTraditional Code Results:")
    for i, test_data in enumerate(test_cases):
        result = await execute_pipeline(test_data)
        print(f"  Test {i+1}: {test_data} → {result.get('message')}")
    print()
    
    # ===== Step 5: Validate that both produce identical results =====
    print("Step 5: Validating that both versions produce identical results...")
    print("-" * 70)
    
    is_valid, errors = await converter.validate(chain, execute_pipeline, test_cases)
    
    if is_valid:
        print("✓ VALIDATION PASSED: Both versions produce identical results!")
        print("  The traditional code has ZERO overhead compared to the chain.")
    else:
        print("✗ VALIDATION FAILED: Results differ!")
        for error in errors:
            print(f"  {error}")
    print()
    
    # ===== Step 6: Generate optimized class =====
    print("Step 6: Generating optimized class for production use...")
    print("-" * 70)
    
    class_code = converter.generate_optimized_class(chain, class_name="OptimizedPipeline")
    
    print("\nGenerated Class Preview (first 40 lines):")
    class_lines = class_code.split('\n')
    for line in class_lines[:40]:
        print(f"  {line}")
    
    if len(class_lines) > 40:
        print(f"  ... ({len(class_lines) - 40} more lines)")
    print()
    
    # Execute and test the class
    namespace_class = {
        'Context': Context,
        'ValidationLink': ValidationLink,
        'ProcessingLink': ProcessingLink,
        'FormattingLink': FormattingLink
    }
    exec(class_code, namespace_class)
    OptimizedPipeline = namespace_class['OptimizedPipeline']
    
    pipeline = OptimizedPipeline()
    
    print("Testing the optimized class:")
    for i, test_data in enumerate(test_cases[:2]):  # Test first 2 cases
        result = await pipeline.execute(test_data)
        print(f"  Test {i+1}: {test_data} → {result.get('message')}")
    print()
    
    # ===== Summary =====
    print("=" * 70)
    print("Summary: Benefits of Zero-Overhead Conversion")
    print("=" * 70)
    print()
    print("1. Development: Use CodeUChain for modularity and maintainability")
    print("2. Production: Convert to traditional code for maximum performance")
    print("3. Validation: Ensure both versions produce identical results")
    print("4. Flexibility: Convert back and forth as needed")
    print()
    print("Performance Comparison:")
    print("  • Chain execution: includes orchestration overhead")
    print("  • Traditional code: direct function calls (zero overhead)")
    print("  • Optimized class: reusable instance (minimal overhead)")
    print()
    print("Use Cases:")
    print("  • API endpoints: Use optimized class for consistent performance")
    print("  • Batch processing: Use traditional code for maximum throughput")
    print("  • Development: Use chain for easy testing and modification")
    print()
    print("=" * 70)


if __name__ == "__main__":
    asyncio.run(main())
