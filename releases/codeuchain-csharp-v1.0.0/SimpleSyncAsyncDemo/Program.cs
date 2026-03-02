using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Demonstration of the simplified sync/async CodeUChain API.
/// Zero extra syntax - just write normal sync/async methods!
/// </summary>
public class SimpleSyncAsyncDemo
{
    public static async Task Main(string[] args)
    {
        Console.WriteLine("=== Simplified Sync/Async CodeUChain Demo ===\n");

        // Just write normal sync/async methods - no special interfaces needed!
        var chain = new Chain()
            .AddLink("sync-validate", new SyncValidator())     // Normal sync method
            .AddLink("async-process", new AsyncProcessor())    // Normal async method
            .AddLink("sync-format", new SyncFormatter())       // Normal sync method
            .UseHook(new SimpleLogger());                // Works with both

        var input = State.Create(new Dictionary<string, object>
        {
            ["data"] = "hello world",
            ["count"] = 42
        });

        Console.WriteLine($"Input: {input}\n");

        // Option 1: Run synchronously (blocks on async operations)
        Console.WriteLine("--- Synchronous Execution ---");
        var syncResult = chain.RunSync(input);
        Console.WriteLine($"Sync Result: {syncResult}\n");

        // Option 2: Run asynchronously (handles everything natively)
        Console.WriteLine("--- Asynchronous Execution ---");
        var asyncResult = await chain.RunAsync(input);
        Console.WriteLine($"Async Result: {asyncResult}\n");

        Console.WriteLine("✅ Zero-extra-syntax sync/async handling works perfectly!");
    }
}

// Just normal classes - no special interfaces or base classes needed!
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
