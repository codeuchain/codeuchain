"""
Demonstration of Context.get() with Default Values

This example shows how the default parameter enhances Context usability
by following Python's standard dict.get() behavior.
"""

from codeuchain.core import Context, MutableContext


def main():
    print("=" * 60)
    print("Context.get() Default Parameter Demo")
    print("=" * 60)
    
    # Create a context with some data
    ctx = Context({
        "user_name": "Alice",
        "timeout": 30,
        "retries": 3,
        "enabled": True
    })
    
    print("\n1. Basic Usage - Existing Keys")
    print("-" * 60)
    print(f"user_name (exists): {ctx.get('user_name', 'Anonymous')}")
    print(f"timeout (exists): {ctx.get('timeout', 60)}")
    
    print("\n2. Default Values - Missing Keys")
    print("-" * 60)
    print(f"page_size (missing): {ctx.get('page_size', 10)}")
    print(f"theme (missing): {ctx.get('theme', 'light')}")
    print(f"max_connections (missing): {ctx.get('max_connections', 100)}")
    
    print("\n3. Different Default Types")
    print("-" * 60)
    print(f"items (missing, list): {ctx.get('items', [])}")
    print(f"metadata (missing, dict): {ctx.get('metadata', {})}")
    print(f"count (missing, int): {ctx.get('count', 0)}")
    print(f"active (missing, bool): {ctx.get('active', False)}")
    
    print("\n4. Working with Falsy Values")
    print("-" * 60)
    ctx_with_falsy = Context({
        "zero": 0,
        "false": False,
        "empty_string": "",
        "empty_list": []
    })
    
    # These should return the actual falsy values, not the default
    print(f"zero (0): {ctx_with_falsy.get('zero', 999)}")
    print(f"false (False): {ctx_with_falsy.get('false', True)}")
    print(f"empty_string (''): '{ctx_with_falsy.get('empty_string', 'default')}'")
    print(f"empty_list ([]): {ctx_with_falsy.get('empty_list', ['default'])}")
    
    print("\n5. Configuration Pattern")
    print("-" * 60)
    config_ctx = Context({
        "api_key": "secret123",
        "endpoint": "https://api.example.com"
    })
    
    # Use defaults for optional configuration
    api_key = config_ctx.get("api_key", "")
    endpoint = config_ctx.get("endpoint", "https://localhost")
    timeout = config_ctx.get("timeout", 30)
    retry_count = config_ctx.get("retry_count", 3)
    debug = config_ctx.get("debug", False)
    
    print(f"API Key: {api_key}")
    print(f"Endpoint: {endpoint}")
    print(f"Timeout: {timeout}s (default)")
    print(f"Retry Count: {retry_count} (default)")
    print(f"Debug Mode: {debug} (default)")
    
    print("\n6. Mutable Context with Defaults")
    print("-" * 60)
    mutable_ctx = MutableContext({"counter": 10})
    
    print(f"counter (exists): {mutable_ctx.get('counter', 0)}")
    print(f"max (missing): {mutable_ctx.get('max', 100)}")
    
    # Set the missing value
    mutable_ctx.set('max', 50)
    print(f"max (after set): {mutable_ctx.get('max', 100)}")
    
    print("\n7. Comparison: Before vs After")
    print("-" * 60)
    print("Before (workarounds needed):")
    print("  value = ctx.get('key') or 'default'  # ❌ Fails with falsy values")
    print("  value = ctx.get('key') if ctx.get('key') is not None else 'default'  # ❌ Verbose")
    print()
    print("After (Pythonic):")
    print("  value = ctx.get('key', 'default')  # ✅ Clean, clear, standard")
    
    print("\n" + "=" * 60)
    print("Demo Complete!")
    print("=" * 60)


if __name__ == "__main__":
    main()
