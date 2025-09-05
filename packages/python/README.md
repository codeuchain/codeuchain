# CodeUChain Python: Agape-Optimized Implementation

With selfless love, CodeUChain chains your code as links, observes with middleware, and flows through forgiving contexts.

## 📦 Installation

```bash
pip install codeuchain
```

**Zero external dependencies** - pure Python!

## 🤖 LLM Support

This package supports the [llm.txt standard](https://codeuchain.github.io/codeuchain/python/llm.txt) for easy AI/LLM integration. See [llm-full.txt](https://codeuchain.github.io/codeuchain/python/llm-full.txt) for comprehensive documentation.

## Features
- **Context:** Immutable by default, mutable for flexibility—embracing Python's dynamism.
- **Link:** Selfless processors, async and ecosystem-rich.
- **Chain:** Harmonious connectors with conditional flows.
- **Middleware:** Gentle enhancers, optional and forgiving.
- **Error Handling:** Compassionate routing and retries.
- **Typed Features:** Optional static typing with TypedDict and generics for type safety.

## Quick Start
```python
import asyncio
from codeuchain import Context, Chain, MathLink, LoggingMiddleware

async def main():
    chain = Chain()
    chain.add_link("math", MathLink("sum"))
    chain.use_middleware(LoggingMiddleware())
    
    ctx = Context({"numbers": [1, 2, 3]})
    result = await chain.run(ctx)
    print(result.get("result"))  # 6

asyncio.run(main())
```

## Typed Features (Optional)

CodeUChain supports optional static typing for enhanced type safety and better IDE support:

### Basic Typed Usage
```python
from typing import TypedDict
from codeuchain import Context, Link, Chain

class InputData(TypedDict):
    numbers: list[int]
    operation: str

class OutputData(InputData):
    result: float

class SumLink(Link[InputData, OutputData]):
    async def call(self, ctx: Context[InputData]) -> Context[OutputData]:
        numbers = ctx.get("numbers") or []
        total = sum(numbers)
        return ctx.insert_as("result", float(total))

# Usage
async def main():
    chain: Chain[InputData, OutputData] = Chain()
    chain.add_link(SumLink(), "sum")
    
    data: InputData = {"numbers": [1, 2, 3], "operation": "sum"}
    ctx: Context[InputData] = Context(data)
    
    result: Context[OutputData] = await chain.run(ctx)
    print(result.get("result"))  # 6.0

asyncio.run(main())
```

### Type Evolution with insert_as()

The `insert_as()` method enables clean type evolution without casting:

```python
class UserInput(TypedDict):
    name: str
    email: str

class UserWithProfile(TypedDict):
    name: str
    email: str
    age: int
    preferences: dict

# Clean type evolution
ctx = Context[UserInput]({"name": "Alice", "email": "alice@example.com"})
evolved_ctx = (
    ctx
    .insert_as("age", 30)
    .insert_as("preferences", {"theme": "dark"})
)
```

### Choosing Between Typed and Untyped

**Use Untyped (Default):**
- Prototyping and exploration
- Dynamic data structures
- Simple scripts
- Maximum flexibility

**Use Typed (Optional):**
- Production systems
- Complex workflows
- Team collaboration
- Long-term maintenance
- Enhanced IDE support

Both approaches work together—you can mix typed and untyped components in the same chain!

## HTTP Examples

Need HTTP functionality? See `examples/http_examples/` for implementations:

### Built-in HTTP (Zero Dependencies)
```python
# Copy from examples/http_examples/http_links.py
from your_project.simple_http import SimpleHttpLink
link = SimpleHttpLink("https://api.example.com/data")
```

### Advanced HTTP (aiohttp)
```python
# Requires: pip install aiohttp
from your_project.aio_http import AioHttpLink
link = AioHttpLink("https://api.example.com/data", method="POST")
```

## Examples

See the `examples/` directory for comprehensive demonstrations:

- `typed_vs_untyped_comparison.py` - Side-by-side comparison of approaches
- `typed_workflow_patterns.py` - Common patterns for typed workflows
- `insert_as_method_demo.py` - Type evolution demonstrations
- `simple_math.py` - Basic untyped usage

## Agape Philosophy
Optimized for Python's prototyping soul—forgiving, ecosystem-integrated, academic-friendly. Start fresh, chain with love.
