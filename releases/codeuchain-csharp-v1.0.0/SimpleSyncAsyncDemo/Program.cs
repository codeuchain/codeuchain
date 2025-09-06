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
            .UseMiddleware(new SimpleLogger());                // Works with both

        var input = Context.Create(new Dictionary<string, object>
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
    public ValueTask<Context> ProcessAsync(Context context)
    {
        // Normal sync method - just return the result directly
        Console.WriteLine("🔍 Sync validation: Checking data...");
        if (!context.ContainsKey("data"))
        {
            throw new InvalidOperationException("Missing data key");
        }
        return ValueTask.FromResult(context.Insert("validated", true));
    }
}

public class AsyncProcessor : ILink
{
    public async ValueTask<Context> ProcessAsync(Context context)
    {
        // Normal async method - just use await
        Console.WriteLine("⚡ Async processing: Processing data...");
        await Task.Delay(100); // Simulate async work
        var data = context.Get("data")?.ToString() ?? "";
        var processed = data.ToUpper();
        return context.Insert("processed", processed);
    }
}

public class SyncFormatter : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        // Normal sync method
        Console.WriteLine("📝 Sync formatting: Formatting result...");
        var data = context.Get("data")?.ToString() ?? "";
        var formatted = $"[{data.ToUpper()}]";
        return ValueTask.FromResult(context.Insert("formatted", formatted));
    }
}

public class SimpleLogger : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"▶️  Starting: {linkName}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"✅ Completed: {linkName}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"❌ Error in {linkName}: {exception.Message}");
        return ValueTask.FromResult(context);
    }
}
