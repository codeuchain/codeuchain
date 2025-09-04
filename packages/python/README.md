# CodeUChain Python: Agape-Optimized Implementation

With selfless love, CodeUChain chains your code as links, observes with middleware, and flows through forgiving contexts.

## Features
- **Context:** Immutable by default, mutable for flexibility—embracing Python's dynamism.
- **Link:** Selfless processors, async and ecosystem-rich.
- **Chain:** Harmonious connectors with conditional flows.
- **Middleware:** Gentle enhancers, optional and forgiving.
- **Error Handling:** Compassionate routing and retries.

## Installation
```bash
pip install -e .
```
**Zero external dependencies** - pure Python!

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

## Agape Philosophy
Optimized for Python's prototyping soul—forgiving, ecosystem-integrated, academic-friendly. Start fresh, chain with love.
