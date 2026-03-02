using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Example 1: Simple Generic State with Type Safety
/// </summary>
public class GenericStateExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== Generic State Example ===\n");

        // Create strongly-typed state
        var state = State<int>.Create(new Dictionary<string, int>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        Console.WriteLine($"Initial state: {state}");

        // Type-safe operations
        var a = state.Get("a"); // Returns int, not object
        var b = state.Get("b"); // Returns int, not object

        var newState = state
            .Insert("sum", a + b)
            .Insert("product", a * b);

        Console.WriteLine($"After operations: {newState}");

        // Compile-time type safety
        var sum = newState.Get("sum"); // Guaranteed to be int
        var product = newState.Get("product"); // Guaranteed to be int

        Console.WriteLine($"Sum: {sum}, Product: {product}\n");
    }
}

/// <summary>
/// Example 2: Generic Links with Input/Output Types
/// </summary>
public class GenericLinkExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== Generic Link Example ===\n");

        // Define strongly-typed processing steps
        var addLink = new AddLink();
        var multiplyLink = new MultiplyLink();

        // Execute with type safety
        var input = 5;
        var result1 = await addLink.CallAsync(input);
        var result2 = await multiplyLink.CallAsync(result1);

        Console.WriteLine($"Input: {input}");
        Console.WriteLine($"After AddLink: {result1}");
        Console.WriteLine($"After MultiplyLink: {result2}\n");
    }
}

/// <summary>
/// Example 3: Generic Chain with Full Type Safety
/// </summary>
public class GenericChainExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== Generic Chain Example ===\n");

        var chain = new Chain<object>()
            .AddLink("validate", new ValidationLink())
            .AddLink("process", new ProcessingLink())
            .AddLink("format", new FormattingLink());

        var input = State<object>.Create(new Dictionary<string, object>
        {
            ["data"] = "hello world",
            ["count"] = 42
        });

        Console.WriteLine($"Input: {input}");

        var result = await chain.RunAsync(input);

        Console.WriteLine($"Result: {result}\n");
    }
}

/// <summary>
/// Example 4: Advanced Generic Pipeline with Constraints
/// </summary>
public class AdvancedGenericExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== Advanced Generic Pipeline ===\n");

        // Pipeline with numeric constraints
        var numericPipeline = new NumericPipeline()
            .AddStep("double", new DoubleStep())
            .AddStep("square", new SquareStep())
            .AddStep("negate", new NegateStep());

        var input = 3.0;
        var result = await numericPipeline.ExecuteAsync(input);

        Console.WriteLine($"Input: {input}");
        Console.WriteLine($"Pipeline result: {result}");
        Console.WriteLine($"Expected: {-(3.0 * 2 * 3.0 * 2)}\n");
    }
}

// Generic Link Implementations
public class AddLink : ILink<int, int>
{
    public async Task<int> CallAsync(int input)
    {
        return input + 10; // Add 10 to any int
    }
}

public class MultiplyLink : ILink<int, int>
{
    public async Task<int> CallAsync(int input)
    {
        return input * 2; // Double any int
    }
}

// Generic State Links
public class ValidationLink : IStateLink<object>
{
    public async Task<State<object>> CallAsync(State<object> state)
    {
        // Validate data exists
        if (!state.ContainsKey("data"))
        {
            throw new InvalidOperationException("Missing data key");
        }
        return state.Insert("validated", true);
    }
}

public class ProcessingLink : IStateLink<object>
{
    public async Task<State<object>> CallAsync(State<object> state)
    {
        var data = state.Get("data")?.ToString() ?? "";
        var processed = data.ToUpper();
        return state.Insert("processed", processed);
    }
}

public class FormattingLink : IStateLink<object>
{
    public async Task<State<object>> CallAsync(State<object> state)
    {
        var processed = state.Get("processed")?.ToString() ?? "";
        var formatted = $"[{processed}]";
        return state.Insert("formatted", formatted);
    }
}

/// <summary>
/// Example 6: Simplified Sync/Async - Zero Extra Syntax
/// Demonstrates the simplest possible sync/async handling.
/// </summary>
public class SimplifiedSyncAsyncExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== Simplified Sync/Async Example ===\n");

        // Just write normal sync/async methods - no extra interfaces or adapters needed!
        var chain = new Chain()
            .AddLink("sync-validate", new SyncValidator())     // Sync method
            .AddLink("async-process", new AsyncProcessor())    // Async method  
            .AddLink("sync-format", new SyncFormatter())       // Sync method
            .UseHook(new SimpleLogger());                // Works with both

        var input = State.Create(new Dictionary<string, object>
        {
            ["data"] = "hello world",
            ["count"] = 42
        });

        Console.WriteLine($"Input: {input}");

        // Option 1: Run synchronously (blocks on async operations)
        Console.WriteLine("\n--- Synchronous Execution ---");
        var syncResult = chain.RunSync(input);
        Console.WriteLine($"Sync Result: {syncResult}");

        // Option 2: Run asynchronously (handles everything natively)
        Console.WriteLine("\n--- Asynchronous Execution ---");
        var asyncResult = await chain.RunAsync(input);
        Console.WriteLine($"Async Result: {asyncResult}");

        Console.WriteLine("\n✅ Zero-extra-syntax sync/async handling!\n");
    }
}

// Just normal classes - no special interfaces needed!
public class SyncValidator : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        // Normal sync method - just return the result directly
        Console.WriteLine("🔍 Sync validation: Checking data...");
        if (!state.ContainsKey("data"))
        {
            throw new InvalidOperationException("Missing data key");
        }
        return ValueTask.FromResult(state.Insert("validated", true));
    }
}

public class AsyncProcessor : ILink
{
    public async ValueTask<State> ProcessAsync(State state)
    {
        // Normal async method - just use await
        Console.WriteLine("⚡ Async processing: Processing data...");
        await Task.Delay(100); // Simulate async work
        var data = state.Get("data")?.ToString() ?? "";
        var processed = data.ToUpper();
        return state.Insert("processed", processed);
    }
}

public class SyncFormatter : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        // Normal sync method
        Console.WriteLine("📝 Sync formatting: Formatting result...");
        var data = state.Get("data")?.ToString() ?? "";
        var formatted = $"[{data.ToUpper()}]";
        return ValueTask.FromResult(state.Insert("formatted", formatted));
    }
}

public class SimpleLogger : IHook
{
    public ValueTask<State> BeforeAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"▶️  Starting: {linkName}");
        return ValueTask.FromResult(state);
    }

    public ValueTask<State> AfterAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"✅ Completed: {linkName}");
        return ValueTask.FromResult(state);
    }

    public ValueTask<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"❌ Error in {linkName}: {exception.Message}");
        return ValueTask.FromResult(state);
    }
}

/// <summary>
/// Program entry point demonstrating unified sync/async patterns.
/// </summary>
public class UnifiedExampleProgram
{
    public static async Task Main(string[] args)
    {
        Console.WriteLine("=== CodeUChain C# Unified Sync/Async Examples ===\n");

        // Run all examples
        await GenericStateExample.RunAsync();
        await GenericLinkExample.RunAsync();
        await GenericChainExample.RunAsync();
        await AdvancedGenericExample.RunAsync();
        await SimplifiedSyncAsyncExample.RunAsync();

        Console.WriteLine("🎉 All examples completed successfully!");
    }
}