"""
Simple Example: Math Chain Processing

Demonstrates modular chain processing with math links and middleware.
Shows the new modular structure: core protocols, component implementations.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import asyncio
from codeuchain.core import Context
from components.chains import BasicChain
from components.links import MathLink
from components.middleware import LoggingMiddleware


async def main():
    # Set up the chain using component implementations
    chain = BasicChain()
    chain.add_link("sum", MathLink("sum"))
    chain.add_link("mean", MathLink("mean"))
    chain.connect("sum", "mean", lambda ctx: ctx.get("result") is not None)
    chain.use_middleware(LoggingMiddleware())
    
    # Run with initial context
    ctx = Context({"numbers": [1, 2, 3, 4, 5]})
    result = await chain.run(ctx)
    
    print(f"Final result: {result.get('result')}")  # Mean: 3.0
    print(f"Full context: {result.to_dict()}")  # Shows all data


if __name__ == "__main__":
    asyncio.run(main())