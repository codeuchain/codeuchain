"""
Performance Comparison: CodeUChain vs Traditional Code

Benchmarks the performance difference between CodeUChain chains
and the equivalent zero-overhead traditional code.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import asyncio
import time
from codeuchain.core import Context, Chain
from codeuchain.utils import ChainConverter


# Test Links
class ComputeLink:
    """Performs a computation."""
    async def call(self, ctx: Context) -> Context:
        value = ctx.get('value') or 0
        result = value * 2 + 10
        return ctx.insert('value', result)


class ValidateLink:
    """Validates the result."""
    async def call(self, ctx: Context) -> Context:
        value = ctx.get('value') or 0
        is_valid = value > 0
        return ctx.insert('valid', is_valid)


class FormatLink:
    """Formats the result."""
    async def call(self, ctx: Context) -> Context:
        value = ctx.get('value') or 0
        formatted = f"Result: {value}"
        return ctx.insert('formatted', formatted)


async def benchmark_chain(chain: Chain, test_data: list, iterations: int) -> float:
    """Benchmark CodeUChain execution."""
    start = time.time()
    
    for _ in range(iterations):
        for data in test_data:
            await chain.run(Context(data))
    
    end = time.time()
    return end - start


async def benchmark_traditional(func, test_data: list, iterations: int) -> float:
    """Benchmark traditional code execution."""
    start = time.time()
    
    for _ in range(iterations):
        for data in test_data:
            await func(data)
    
    end = time.time()
    return end - start


async def benchmark_optimized_class(pipeline_class, test_data: list, iterations: int) -> float:
    """Benchmark optimized class execution."""
    pipeline = pipeline_class()
    start = time.time()
    
    for _ in range(iterations):
        for data in test_data:
            await pipeline.execute(data)
    
    end = time.time()
    return end - start


async def main():
    print("=" * 70)
    print("Performance Comparison: CodeUChain vs Traditional Code")
    print("=" * 70)
    print()
    
    # Setup
    print("Setting up benchmark...")
    print("-" * 70)
    
    # Create chain
    chain = Chain()
    chain.add_link(ComputeLink(), "compute")
    chain.add_link(ValidateLink(), "validate")
    chain.add_link(FormatLink(), "format")
    
    # Generate traditional code
    converter = ChainConverter()
    traditional_code = converter.chain_to_code(chain)
    
    # Execute traditional code
    namespace = {
        'Context': Context,
        'ComputeLink': ComputeLink,
        'ValidateLink': ValidateLink,
        'FormatLink': FormatLink
    }
    exec(traditional_code, namespace)
    execute_pipeline = namespace['execute_pipeline']
    
    # Generate optimized class
    class_code = converter.generate_optimized_class(chain, "OptimizedPipeline")
    exec(class_code, namespace)
    OptimizedPipeline = namespace['OptimizedPipeline']
    
    # Test data
    test_data = [
        {'value': 1},
        {'value': 5},
        {'value': 10},
        {'value': 20},
        {'value': 50},
    ]
    
    # Warm-up
    print("Warming up...")
    for _ in range(10):
        await chain.run(Context(test_data[0]))
        await execute_pipeline(test_data[0])
    
    print("✓ Setup complete")
    print()
    
    # Benchmark parameters
    iterations = 100
    total_operations = iterations * len(test_data)
    
    print(f"Running benchmarks...")
    print(f"  • Test data: {len(test_data)} items")
    print(f"  • Iterations: {iterations}")
    print(f"  • Total operations: {total_operations}")
    print("-" * 70)
    print()
    
    # Benchmark 1: Traditional Code (baseline)
    print("1. Benchmarking Traditional Code (baseline)...")
    traditional_time = await benchmark_traditional(execute_pipeline, test_data, iterations)
    print(f"   ✓ Completed in {traditional_time:.4f}s")
    print()
    
    # Benchmark 2: Optimized Class
    print("2. Benchmarking Optimized Class...")
    optimized_time = await benchmark_optimized_class(OptimizedPipeline, test_data, iterations)
    print(f"   ✓ Completed in {optimized_time:.4f}s")
    print()
    
    # Benchmark 3: CodeUChain
    print("3. Benchmarking CodeUChain...")
    chain_time = await benchmark_chain(chain, test_data, iterations)
    print(f"   ✓ Completed in {chain_time:.4f}s")
    print()
    
    # Results
    print("=" * 70)
    print("Results")
    print("=" * 70)
    print()
    
    # Calculate overhead
    traditional_overhead = 0.0
    optimized_overhead = ((optimized_time - traditional_time) / traditional_time) * 100
    chain_overhead = ((chain_time - traditional_time) / traditional_time) * 100
    
    # Operations per second
    traditional_ops = total_operations / traditional_time
    optimized_ops = total_operations / optimized_time
    chain_ops = total_operations / chain_time
    
    print(f"{'Approach':<20} {'Time (s)':<12} {'Overhead':<12} {'Ops/sec':<12}")
    print("-" * 70)
    print(f"{'Traditional Code':<20} {traditional_time:>10.4f}  {traditional_overhead:>10.1f}%  {traditional_ops:>10.0f}")
    print(f"{'Optimized Class':<20} {optimized_time:>10.4f}  {optimized_overhead:>10.1f}%  {optimized_ops:>10.0f}")
    print(f"{'CodeUChain':<20} {chain_time:>10.4f}  {chain_overhead:>10.1f}%  {chain_ops:>10.0f}")
    print()
    
    # Analysis
    print("=" * 70)
    print("Analysis")
    print("=" * 70)
    print()
    
    print("Performance Characteristics:")
    print(f"  • Traditional Code: {traditional_ops:.0f} ops/sec (baseline)")
    print(f"  • Optimized Class: {optimized_ops:.0f} ops/sec ({optimized_overhead:+.1f}% vs baseline)")
    print(f"  • CodeUChain: {chain_ops:.0f} ops/sec ({chain_overhead:+.1f}% vs baseline)")
    print()
    
    print("When to Use Each Approach:")
    print()
    print("Traditional Code:")
    print("  ✓ Production API endpoints")
    print("  ✓ High-throughput batch processing")
    print("  ✓ Performance-critical sections")
    print("  ✓ Absolute maximum performance needed")
    print()
    
    print("Optimized Class:")
    print("  ✓ Production services (reusable instances)")
    print("  ✓ Microservices with consistent workloads")
    print("  ✓ Good balance of performance and maintainability")
    print(f"  ✓ Only {optimized_overhead:.1f}% slower than traditional code")
    print()
    
    print("CodeUChain:")
    print("  ✓ Development and prototyping")
    print("  ✓ Complex conditional flows")
    print("  ✓ Need for middleware (logging, metrics)")
    print("  ✓ Frequent logic changes")
    print("  ✓ Testing and debugging")
    print()
    
    # Recommendations
    print("=" * 70)
    print("Recommendations")
    print("=" * 70)
    print()
    
    print("Development Workflow:")
    print("  1. Develop using CodeUChain for modularity and clarity")
    print("  2. Test thoroughly with chain-based tests")
    print("  3. Convert to optimized class for production deployment")
    print("  4. Validate both produce identical results")
    print("  5. Monitor performance in production")
    print()
    
    print(f"In this benchmark, converting from CodeUChain to traditional code")
    print(f"resulted in a {chain_overhead - optimized_overhead:.1f}% performance improvement.")
    print()
    
    print("=" * 70)


if __name__ == "__main__":
    asyncio.run(main())
