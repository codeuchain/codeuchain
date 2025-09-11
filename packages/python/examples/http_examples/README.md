# HTTP Link Examples

These are **example implementations** showing how to add HTTP functionality to CodeUChain. They are **NOT included** in the core package build.

## Why Separate?

The core CodeUChain package maintains **zero external dependencies** for maximum portability. HTTP functionality is completely optional and left to users to implement based on their needs.

## Available Examples

### `SimpleHttpLink`
- Uses Python's built-in `urllib` (zero dependencies)
- Good for simple GET requests
- Synchronous HTTP wrapped in async executor

### `AioHttpLink`
- Uses `aiohttp` library (requires: `pip install aiohttp`)
- Full async HTTP support
- Advanced features like custom headers, POST requests

## Usage

```python
# Copy the implementation you need to your project
from your_project.http_link import SimpleHttpLink

# Use in your chains
chain = BasicChain()
chain.add_link("api", SimpleHttpLink("https://api.example.com/data"))
```

## Design Approach

This approach follows the **principle of minimal coupling**:
- Core library stays pure and portable
- Users have full control over HTTP implementations
- Easy to swap between different HTTP libraries
- No forced dependencies on specific ecosystems

## Testing

Run the example:
```bash
python examples/http_examples/http_links.py
```