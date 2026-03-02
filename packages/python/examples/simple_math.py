"""
Simple Example: Math Chain Processing

Demonstrates modular chain processing with math links and hook.
Shows the new modular structure: core protocols, component implementations.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import asyncio
from codeuchain.core import State
from components.chains import BasicChain
from components.links import MathLink
from components.hook import LoggingHook


async def main():
    # Set up the chain using component implementations
    chain = BasicChain()
    chain.add_link("sum", MathLink("sum"))
    chain.add_link("mean", MathLink("mean"))
    chain.connect("sum", "mean", lambda ctx: ctx.get("result") is not None)
    chain.use_hook(LoggingHook())
    
    # Run with initial state
    ctx = State({"numbers": [1, 2, 3, 4, 5]})
    result = await chain.run(ctx)
    
    print(f"Final result: {result.get('result')}")  # Mean: 3.0
    print(f"Full state: {result.to_dict()}")  # Shows all data


if __name__ == "__main__":
    asyncio.run(main())