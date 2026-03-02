"""
Typed Example: Opt-in state typing with TypedDict

This example demonstrates how to opt in to state typing using `State[MyShape]`,
`Link[InShape, OutShape]`, and `Chain[InShape, OutShape]` so static checkers can
validate link compatibility and state contents.
"""
from typing import TypedDict, List

import asyncio

from codeuchain.core import State
from codeuchain.core import Chain
from codeuchain.core import Link


class InputShape(TypedDict):
    numbers: List[int]


class OutputShape(TypedDict):
    result: float


class SumLink(Link[InputShape, OutputShape]):
    async def call(self, ctx: State[InputShape]) -> State[OutputShape]:
        numbers = ctx.get("numbers") or []
        total = sum(numbers)
        return ctx.insert("result", total / len(numbers) if numbers else 0.0)


async def main() -> None:
    chain: Chain[InputShape, OutputShape] = Chain()
    chain.add_link(SumLink(), "sum")

    ctx = State[InputShape]({"numbers": [1, 2, 3]})
    result_ctx = await chain.run(ctx)
    result: State[OutputShape] = result_ctx  # Type assertion for static checking

    print(result.get("result"))


if __name__ == "__main__":
    asyncio.run(main())
