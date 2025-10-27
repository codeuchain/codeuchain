---
applyTo: 'packages/python/**'
---

# Python Development Instructions

## 🐍 Python-Specific Guidelines

The Python implementation is the **reference implementation** for CodeUChain. Use this as the source of truth for understanding core concepts.

## Project Structure

```
packages/python/
├── codeuchain/
│   ├── core/          # Core framework (Context, Link, Chain)
│   ├── middleware/    # Middleware implementations
│   └── utils/         # Utility functions
├── tests/            # Test suite
├── examples/         # Example implementations
└── docs/            # Python-specific documentation
```

## Development Setup

### Prerequisites
- Python 3.8 or higher
- pip or poetry

### Installation
```bash
cd packages/python
pip install -e ".[dev]"  # Install in development mode with dev dependencies
```

### Running Tests
```bash
cd packages/python
pytest                    # Run all tests
pytest -v                # Verbose output
pytest tests/test_typed.py  # Run specific test file
pytest -k "test_name"    # Run tests matching pattern
```

### Running Linters
```bash
cd packages/python
black codeuchain/        # Format code
flake8 codeuchain/       # Check style
mypy codeuchain/         # Type checking
```

## Code Style

### Imports
```python
# Standard library
import os
from typing import Any, Dict

# Third-party
import pytest

# Local
from codeuchain.core import Context, Link, Chain
```

### Type Hints
- **Required** for all public APIs
- Use Python 3.10+ style generics (`Context[T]` not `Context[TypeVar('T')]`)
- Use `Any` sparingly, prefer specific types

### Docstrings
```python
def process_data(ctx: Context[InputData]) -> Context[OutputData]:
    """Process input data and transform context.
    
    Args:
        ctx: Context containing input data with 'numbers' field
        
    Returns:
        Context with added 'result' field containing sum
        
    Examples:
        >>> ctx = Context[InputData]({"numbers": [1, 2, 3]})
        >>> result = process_data(ctx)
        >>> result.get("result")
        6
    """
    pass
```

## Testing Patterns

### Basic Test Structure
```python
import pytest
from codeuchain.core import Context, Link

class TestMyLink:
    def test_basic_functionality(self):
        # Arrange
        ctx = Context({"input": "value"})
        link = MyLink()
        
        # Act
        result = link.call(ctx)
        
        # Assert
        assert result.get("output") == "expected"
```

### Async Tests
```python
import pytest

@pytest.mark.asyncio
async def test_async_link():
    ctx = Context({"input": "value"})
    link = AsyncLink()
    
    result = await link.call(ctx)
    
    assert result.get("output") == "expected"
```

### Type Tests
```python
from typing import TypedDict

class InputData(TypedDict):
    numbers: list[int]

class OutputData(TypedDict):
    result: float

def test_typed_link():
    # Test with explicit types
    ctx = Context[InputData]({"numbers": [1, 2, 3]})
    link = SumLink()
    
    result = await link.call(ctx)
    
    assert isinstance(result, Context)
    assert result.get("result") == 6.0
```

## Common Patterns

### Creating a Simple Link
```python
from codeuchain.core import Link, Context
from typing import Any

class MyLink(Link):
    async def call(self, ctx: Context) -> Context:
        # Get value from context
        value = ctx.get("input_key")
        
        # Process value
        result = self.process(value)
        
        # Return new context with result
        return ctx.insert("output_key", result)
    
    def process(self, value: Any) -> Any:
        # Your processing logic
        return value
```

### Creating a Generic Link
```python
from codeuchain.core import Link, Context
from typing import TypedDict

class InputData(TypedDict):
    numbers: list[int]

class OutputData(TypedDict):
    result: float

class SumLink(Link[InputData, OutputData]):
    async def call(self, ctx: Context[InputData]) -> Context[OutputData]:
        numbers = ctx.get("numbers")
        total = sum(numbers)
        return ctx.insert_as("result", float(total))
```

### Creating a Chain
```python
from codeuchain.core import Chain

# Simple chain
chain = Chain([
    Link1(),
    Link2(),
    Link3()
])

# Execute chain
initial_ctx = Context({"input": "data"})
result = await chain.execute(initial_ctx)
```

### Using Middleware
```python
from codeuchain.middleware import LoggingMiddleware

chain = Chain([
    Link1(),
    Link2()
], middleware=[
    LoggingMiddleware()
])
```

## Type Evolution

### Using insert() - Preserves Type
```python
ctx: Context[InputData] = Context[InputData]({"numbers": [1, 2, 3]})
# Type is still Context[InputData]
ctx2 = ctx.insert("extra", "value")
```

### Using insert_as() - Changes Type
```python
ctx: Context[InputData] = Context[InputData]({"numbers": [1, 2, 3]})
# Type is now Context[Any] or Context[OutputData]
ctx2: Context[Any] = ctx.insert_as("result", 6.0)
```

## Error Handling

### Link-Level Errors
```python
class MyLink(Link):
    async def call(self, ctx: Context) -> Context:
        try:
            result = self.risky_operation(ctx)
            return ctx.insert("result", result)
        except ValueError as e:
            # Handle specific error
            return ctx.insert("error", str(e))
```

### Chain-Level Errors
```python
try:
    result = await chain.execute(ctx)
except ChainExecutionError as e:
    # Handle chain execution error
    print(f"Chain failed: {e}")
```

## Best Practices

### Context Management
- **Immutable**: Always create new Context instances
- **Clear Keys**: Use descriptive key names
- **Type Safety**: Use typed contexts for better IDE support

### Link Design
- **Single Responsibility**: Each link does one thing well
- **Stateless**: Links should not maintain state between calls
- **Composable**: Design links to work well in chains

### Performance
- **Lazy Evaluation**: Don't compute values until needed
- **Async by Default**: Use async/await for I/O operations
- **Minimal Copying**: Context uses efficient internal structure

## Debugging

### Print Context State
```python
ctx = Context({"key": "value"})
print(ctx.to_dict())  # View all context data
```

### Debug Link Execution
```python
class DebugLink(Link):
    async def call(self, ctx: Context) -> Context:
        print(f"Before: {ctx.to_dict()}")
        result = await self.process(ctx)
        print(f"After: {result.to_dict()}")
        return result
```

### Using Python Debugger
```python
import pdb

class MyLink(Link):
    async def call(self, ctx: Context) -> Context:
        pdb.set_trace()  # Breakpoint
        return ctx.insert("result", "value")
```

## Common Issues

### Import Errors
- Ensure you've installed the package: `pip install -e .`
- Check your Python path includes the package

### Type Errors
- Run `mypy` to check type annotations
- Ensure you're using Python 3.10+ for modern generic syntax

### Test Failures
- Run tests in isolation: `pytest tests/test_file.py::test_name`
- Check async test markers: `@pytest.mark.asyncio`

## Reference

- **Core Implementation**: `codeuchain/core/`
- **Test Examples**: `tests/test_typed.py`
- **Type Specification**: `docs/TYPED_FEATURES_SPECIFICATION.md`
- **Python Documentation**: `packages/python/README.md`
