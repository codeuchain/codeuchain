"""
HTTP Link Examples - Not included in core package

These examples show how users can implement HTTP functionality
using the Link protocol. Copy and modify these for your projects!

The core CodeUChain package has ZERO external dependencies.
"""

from typing import Optional
import asyncio
import json
from urllib.request import urlopen, Request
from urllib.error import URLError
from codeuchain.core.context import Context
from codeuchain.core.link import Link


class SimpleHttpLink(Link):
    """
    Simple HTTP GET link using Python standard library.

    Usage:
        link = SimpleHttpLink("https://api.example.com/data")
        result = await link.call(context)
        data = result.get("response")
    """

    def __init__(self, url: str, headers: Optional[dict] = None):
        self.url = url
        self.headers = headers or {}

    async def call(self, ctx: Context) -> Context:
        def sync_request():
            try:
                req = Request(self.url, headers=self.headers)
                with urlopen(req) as response:
                    data = json.loads(response.read().decode('utf-8'))
                    return ctx.insert("response", data)
            except URLError as e:
                return ctx.insert("error", str(e))
            except json.JSONDecodeError as e:
                return ctx.insert("error", f"Invalid JSON response: {e}")

        # Run sync HTTP in thread pool to avoid blocking
        loop = asyncio.get_event_loop()
        result_ctx = await loop.run_in_executor(None, sync_request)
        return result_ctx


class AioHttpLink(Link):
    """
    Advanced HTTP link using aiohttp (requires: pip install aiohttp).

    Usage:
        link = AioHttpLink("https://api.example.com/data", method="POST")
        result = await link.call(context)
        data = result.get("response")
    """

    def __init__(self, url: str, method: str = "GET", headers: Optional[dict] = None):
        self.url = url
        self.method = method
        self.headers = headers or {}

    async def call(self, ctx: Context) -> Context:
        try:
            import aiohttp  # type: ignore
        except ImportError:
            return ctx.insert("error", "aiohttp not installed. Run: pip install aiohttp")

        try:
            async with aiohttp.ClientSession() as session:
                async with session.request(
                    self.method,
                    self.url,
                    headers=self.headers
                ) as resp:
                    if resp.content_type == 'application/json':
                        data = await resp.json()
                    else:
                        data = await resp.text()
                    return ctx.insert("response", data)
        except Exception as e:
            return ctx.insert("error", str(e))


# Example usage
async def example_usage():
    """Example of using HTTP links in a chain."""

    from components.chains import BasicChain
    from components.middleware import LoggingMiddleware

    # Create a chain with HTTP functionality
    chain = BasicChain()
    chain.add_link("api", SimpleHttpLink("https://jsonplaceholder.typicode.com/todos/1"))
    chain.use_middleware(LoggingMiddleware())

    # Run the chain
    ctx = Context({})
    result = await chain.run(ctx)

    print(f"Response: {result.get('response')}")
    print(f"Error: {result.get('error')}")


if __name__ == "__main__":
    asyncio.run(example_usage())