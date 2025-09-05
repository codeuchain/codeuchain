using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading.Tasks;

/// <summary>
/// Performance comparison between sync and async execution.
/// </summary>
public class PerformanceComparison
{
    public static async Task Main(string[] args)
    {
        await RunComparisonAsync();
    }

    public static async Task RunComparisonAsync()
    {
        Console.WriteLine("=== CodeUChain C# Performance Comparison ===\n");

        const int iterations = 10000;

        // Setup async chain
        var asyncChain = new Chain();
        asyncChain = asyncChain.AddLink("add", new FastAddLink());
        asyncChain = asyncChain.AddLink("multiply", new FastMultiplyLink());

        // Setup sync chain
        var syncChain = new SyncChain();
        syncChain = syncChain.AddLink("add", new SyncAddLink());
        syncChain = syncChain.AddLink("multiply", new SyncMultiplyLink());

        var input = Context.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        // Warm up both
        Console.WriteLine("Warming up...");
        for (int i = 0; i < 100; i++)
        {
            await asyncChain.RunAsync(input);
            syncChain.Run(input);
        }

        // Measure async performance
        Console.WriteLine($"Running {iterations} async iterations...");
        var asyncStopwatch = Stopwatch.StartNew();
        for (int i = 0; i < iterations; i++)
        {
            await asyncChain.RunAsync(input);
        }
        asyncStopwatch.Stop();

        // Measure sync performance
        Console.WriteLine($"Running {iterations} sync iterations...");
        var syncStopwatch = Stopwatch.StartNew();
        for (int i = 0; i < iterations; i++)
        {
            syncChain.Run(input);
        }
        syncStopwatch.Stop();

        // Results
        var asyncTime = asyncStopwatch.ElapsedMilliseconds;
        var syncTime = syncStopwatch.ElapsedMilliseconds;

        Console.WriteLine($"\nResults:");
        Console.WriteLine($"Async execution: {asyncTime}ms");
        Console.WriteLine($"Sync execution:  {syncTime}ms");
        Console.WriteLine($"Difference:      {asyncTime - syncTime}ms");
        Console.WriteLine($"Async overhead:  {((double)(asyncTime - syncTime) / Math.Max(syncTime, 1) * 100):F2}%");

        // Per-iteration analysis
        Console.WriteLine($"\nPer-iteration analysis:");
        Console.WriteLine($"Async time/iter: {asyncTime / (double)iterations:F3}ms");
        Console.WriteLine($"Sync time/iter:  {syncTime / (double)iterations:F3}ms");
        Console.WriteLine($"Overhead/iter:   {(asyncTime - syncTime) / (double)iterations:F3}ms");

        // Memory and allocation analysis
        Console.WriteLine($"\nPerformance Insights:");
        if (asyncTime > syncTime)
        {
            Console.WriteLine($"• Async has {(asyncTime - syncTime) / (double)syncTime * 100:F1}% overhead");
            Console.WriteLine("• Primary costs: Task allocation, async state machine, thread pool transitions");
        }
        else
        {
            Console.WriteLine("• Async is actually faster (likely due to thread pool optimizations)");
        }

        Console.WriteLine("\nRecommendations:");
        Console.WriteLine("• For CPU-bound work: Use sync interfaces to avoid overhead");
        Console.WriteLine("• For I/O-bound work: Use async interfaces for scalability");
        Console.WriteLine("• For mixed workloads: Consider hybrid approach");
    }
}

/// <summary>
/// Fast synchronous link implementations for performance testing.
/// </summary>
public class FastAddLink : ILink
{
    public async Task<Context> CallAsync(Context context)
    {
        var a = context.Get<int>("a");
        var b = context.Get<int>("b");
        return context.Insert("sum", a + b);
    }
}

public class FastMultiplyLink : ILink
{
    public async Task<Context> CallAsync(Context context)
    {
        var sum = context.Get<int>("sum");
        return context.Insert("result", sum * 2);
    }
}