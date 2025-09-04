"""
Typed Example: Opt-in context typing with TypedDict

This example demonstrates how to opt in to context typing using `Context[MyShape]`,
`Link[InShape, OutShape]`, and `Chain[InShape, OutShape]` so static checkers can
validate link compatibility and context contents.
"""
from typing import TypedDict, List

import asyncio

from codeuchain.core import Context
from codeuchain.core import Chain
from codeuchain.core import Link


class InputShape(TypedDict):
    numbers: List[int]


class OutputShape(TypedDict):
    result: float


class SumLink(Link[InputShape, OutputShape]):
    async def call(self, ctx: Context[InputShape]) -> Context[OutputShape]:
        numbers = ctx.get("numbers") or []
        total = sum(numbers)
        return ctx.insert("result", total / len(numbers) if numbers else 0.0)


async def main() -> None:
    chain: Chain[InputShape, OutputShape] = Chain()
    chain.add_link(SumLink(), "sum")

    ctx = Context[InputShape]({"numbers": [1, 2, 3]})
    result_ctx = await chain.run(ctx)
    result: Context[OutputShape] = result_ctx  # Type assertion for static checking

    print(result.get("result"))


if __name__ == "__main__":
    asyncio.run(main())
