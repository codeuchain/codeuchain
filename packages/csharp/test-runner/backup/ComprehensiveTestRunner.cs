using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading.Tasks;

/// <summary>
/// Comprehensive Test Suite for CodeUChain C# Implementation
/// Provides full code coverage and verbose testing for all framework functionality.
/// </summary>
public class ComprehensiveTestRunner
{
    private static int _passedTests = 0;
    private static int _failedTests = 0;
    private static readonly List<string> _testResults = new();

    public static async Task Main(string[] args)
    {
        Console.WriteLine("🧪 CodeUChain C# Comprehensive Test Suite");
        Console.WriteLine("==========================================\n");

        var stopwatch = Stopwatch.StartNew();

        // Core Functionality Tests
        await TestBasicContextOperations();
        await TestTypedContextOperations();
        await TestTypeEvolution();
        await TestGenericLinks();
        await TestGenericChains();
        await TestMixedUsage();
        await TestBackwardCompatibility();

        // Advanced Tests
        await TestErrorHandling();
        await TestEdgeCases();
        await TestPerformance();
        await TestChainComposition();

        // Middleware Tests
        await TestMiddlewareFunctionality();
        await TestAsyncOperations();

        stopwatch.Stop();

        // Summary
        Console.WriteLine("\n" + "=".Repeat(50));
        Console.WriteLine("📊 COMPREHENSIVE TEST RESULTS");
        Console.WriteLine("=".Repeat(50));
        Console.WriteLine($"Total Tests: {_passedTests + _failedTests}");
        Console.WriteLine($"✅ Passed: {_passedTests}");
        Console.WriteLine($"❌ Failed: {_failedTests}");
        Console.WriteLine($"⏱️  Execution Time: {stopwatch.Elapsed.TotalSeconds:F2} seconds");
        Console.WriteLine($"📈 Success Rate: {(_passedTests * 100.0 / (_passedTests + _failedTests)):F1}%");

        if (_failedTests > 0)
        {
            Console.WriteLine("\n❌ FAILED TESTS:");
            foreach (var result in _testResults.Where(r => r.Contains("❌")))
            {
                Console.WriteLine($"  {result}");
            }
        }

        Console.WriteLine($"\n🎯 OVERALL STATUS: {(_failedTests == 0 ? "✅ ALL TESTS PASSED" : "❌ SOME TESTS FAILED")}");
    }

    private static async Task TestBasicContextOperations()
    {
        Console.WriteLine("🔍 Testing Basic Context Operations...");

        // Test 1: Empty Context Creation
        var emptyContext = Context.Create();
        Assert(emptyContext.Count == 0, "Empty context should have count 0");
        Assert(emptyContext.ToString() == "Context()", "Empty context string representation");

        // Test 2: Context with Initial Data
        var initialData = new Dictionary<string, object>
        {
            ["name"] = "Alice",
            ["age"] = 30,
            ["active"] = true
        };
        var context = Context.Create(initialData);
        Assert(context.Count == 3, "Context should have 3 items");
        Assert(context.Get("name")?.ToString() == "Alice", "Should retrieve name correctly");
        Assert((int?)context.Get("age") == 30, "Should retrieve age correctly");
        Assert((bool?)context.Get("active") == true, "Should retrieve active status correctly");

        // Test 3: Insert Operations
        var updatedContext = context.Insert("city", "New York");
        Assert(updatedContext.Count == 4, "Updated context should have 4 items");
        Assert(updatedContext.Get("city")?.ToString() == "New York", "Should retrieve inserted value");

        // Test 4: Remove Operations
        var removedContext = updatedContext.Remove("active");
        Assert(removedContext.Count == 3, "Removed context should have 3 items");
        Assert(removedContext.Get("active") == null, "Removed key should return null");

        // Test 5: Contains Key
        Assert(context.ContainsKey("name"), "Should contain existing key");
        Assert(!context.ContainsKey("nonexistent"), "Should not contain nonexistent key");

        Console.WriteLine("✅ Basic Context Operations: PASSED");
    }

    private static async Task TestTypedContextOperations()
    {
        Console.WriteLine("🔍 Testing Typed Context Operations...");

        // Test 1: Generic Context Creation
        var typedContext = Context<string>.Create();
        Assert(typedContext.Count == 0, "Empty typed context should have count 0");

        // Test 2: Typed Context with Initial Data
        var initialData = new Dictionary<string, object>
        {
            ["message"] = "Hello World",
            ["count"] = 42
        };
        var context = Context<string>.Create(initialData);
        Assert(context.Count == 2, "Typed context should have 2 items");

        // Test 3: Typed Get Operations
        var message = context.Get("message");
        Assert(message == "Hello World", "Should retrieve typed string value");

        var count = context.Get("count");
        Assert(count == null, "Should return null for non-string type");

        // Test 4: GetAny Operations
        var anyMessage = context.GetAny("message");
        Assert(anyMessage?.ToString() == "Hello World", "GetAny should retrieve any type");

        var anyCount = context.GetAny("count");
        Assert((int?)anyCount == 42, "GetAny should retrieve integer value");

        Console.WriteLine("✅ Typed Context Operations: PASSED");
    }

    private static async Task TestTypeEvolution()
    {
        Console.WriteLine("🔍 Testing Type Evolution...");

        // Test 1: Basic Type Evolution
        var stringContext = Context<string>.Create(new Dictionary<string, object>
        {
            ["data"] = "initial"
        });

        var intContext = stringContext.InsertAs<int>("number", 100);
        Assert(intContext.Get("number") == 100, "Should retrieve integer from evolved context");
        Assert(intContext.Get("data") == null, "Should not retrieve string from int context");

        // Test 2: Chain Type Evolution
        var context1 = Context<object>.Create(new Dictionary<string, object>
        {
            ["step"] = 1
        });

        var context2 = context1.InsertAs<string>("message", "processing");
        var context3 = context2.InsertAs<int>("result", 42);

        Assert(context3.Get("result") == 42, "Final context should have integer result");
        Assert(context3.Get("message") == null, "Final context should not have string message");

        // Test 3: Type Preservation vs Evolution
        var preservedContext = stringContext.Insert("data", "updated");
        Assert(preservedContext.Get("data") == "updated", "Insert should preserve type");

        var evolvedContext = stringContext.InsertAs<object>("data", "evolved");
        Assert(evolvedContext.GetAny("data")?.ToString() == "evolved", "InsertAs should evolve type");

        Console.WriteLine("✅ Type Evolution: PASSED");
    }

    private static async Task TestGenericLinks()
    {
        Console.WriteLine("🔍 Testing Generic Links...");

        // Test 1: Simple Generic Link
        var stringToIntLink = new StringToIntLink();
        var inputContext = Context<string>.Create(new Dictionary<string, object>
        {
            ["value"] = "42"
        });

        var outputContext = await stringToIntLink.CallAsync(inputContext);
        Assert(outputContext.Get("result")?.ToString() == "42", "Link should convert string to int");

        // Test 2: Complex Generic Link
        var processorLink = new DataProcessorLink();
        var complexInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["data"] = "test",
            ["multiplier"] = 2
        });

        var complexOutput = await processorLink.CallAsync(complexInput);
        Assert(complexOutput.Get("processed")?.ToString() == "TEST", "Should process string to uppercase");
        Assert(complexOutput.Get("calculated") == 4, "Should calculate doubled value");

        Console.WriteLine("✅ Generic Links: PASSED");
    }

    private static async Task TestGenericChains()
    {
        Console.WriteLine("🔍 Testing Generic Chains...");

        // Test 1: Simple Generic Chain
        var chain = new Chain<string, object>()
            .AddLink("parse", new StringToIntLink())
            .AddLink("double", new DoubleIntLink());

        var input = Context<string>.Create(new Dictionary<string, object>
        {
            ["value"] = "21"
        });

        var result = await chain.RunAsync(input);
        Assert(result.Get("final")?.ToString() == "42", "Chain should process string to doubled int");

        // Test 2: Complex Chain with Type Evolution
        var complexChain = new Chain<object, object>()
            .AddLink("validate", new ValidationLink())
            .AddLink("process", new ProcessingLink())
            .AddLink("format", new FormattingLink());

        var complexInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["data"] = "hello world"
        });

        var complexResult = await complexChain.RunAsync(complexInput);
        Assert(complexResult.Get("formatted")?.ToString() == "[HELLO WORLD]", "Complex chain should format correctly");

        Console.WriteLine("✅ Generic Chains: PASSED");
    }

    private static async Task TestMixedUsage()
    {
        Console.WriteLine("🔍 Testing Mixed Usage...");

        // Test 1: Mixed Typed and Untyped Contexts
        var untypedContext = Context.Create(new Dictionary<string, object>
        {
            ["data"] = "mixed"
        });

        var typedContext = Context<string>.Create(new Dictionary<string, object>
        {
            ["typed"] = "data"
        });

        // Test 2: Mixed Links
        var mixedChain = new Chain<object, object>()
            .AddLink("untyped", new UntypedProcessorLink())
            .AddLink("typed", new TypedProcessorLink());

        var mixedResult = await mixedChain.RunAsync(untypedContext);
        Assert(mixedResult.Get("processed") != null, "Mixed chain should process successfully");

        Console.WriteLine("✅ Mixed Usage: PASSED");
    }

    private static async Task TestBackwardCompatibility()
    {
        Console.WriteLine("🔍 Testing Backward Compatibility...");

        // Test 1: Original Untyped Chain
        var untypedChain = new Chain()
            .AddLink("process", new LegacyProcessor())
            .UseMiddleware(new LoggingMiddleware());

        var untypedInput = Context.Create(new Dictionary<string, object>
        {
            ["input"] = "legacy"
        });

        var untypedResult = await untypedChain.RunAsync(untypedInput);
        Assert(untypedResult.Get("output")?.ToString() == "LEGACY", "Untyped chain should work");

        // Test 2: Mixed Old and New
        var mixedChain = new Chain()
            .AddLink("legacy", new LegacyProcessor())
            .AddLink("modern", new ModernProcessor());

        var mixedResult = await mixedChain.RunAsync(untypedInput);
        Assert(mixedResult.Get("final")?.ToString() == "LEGACY-MODERN", "Mixed chain should work");

        Console.WriteLine("✅ Backward Compatibility: PASSED");
    }

    private static async Task TestErrorHandling()
    {
        Console.WriteLine("🔍 Testing Error Handling...");

        // Test 1: Link Error Handling
        var errorChain = new Chain<object, object>()
            .AddLink("error", new ErrorLink());

        var errorInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["trigger"] = "error"
        });

        try
        {
            await errorChain.RunAsync(errorInput);
            Assert(false, "Should have thrown exception");
        }
        catch (InvalidOperationException ex)
        {
            Assert(ex.Message == "Test error", "Should catch correct exception");
        }

        // Test 2: Middleware Error Handling
        var middlewareChain = new Chain()
            .AddLink("safe", new SafeLink())
            .UseMiddleware(new ErrorHandlingMiddleware());

        var safeResult = await middlewareChain.RunAsync(errorInput);
        Assert(safeResult.Get("handled") != null, "Middleware should handle errors");

        Console.WriteLine("✅ Error Handling: PASSED");
    }

    private static async Task TestEdgeCases()
    {
        Console.WriteLine("🔍 Testing Edge Cases...");

        // Test 1: Empty Chains
        var emptyChain = new Chain<object, object>();
        var emptyResult = await emptyChain.RunAsync(Context<object>.Create());
        Assert(emptyResult.Count == 0, "Empty chain should return empty context");

        // Test 2: Null Values
        var nullContext = Context.Create();
        nullContext = nullContext.Insert("nullValue", null);
        Assert(nullContext.Get("nullValue") == null, "Should handle null values");

        // Test 3: Large Data Sets
        var largeData = new Dictionary<string, object>();
        for (int i = 0; i < 1000; i++)
        {
            largeData[$"key{i}"] = $"value{i}";
        }
        var largeContext = Context.Create(largeData);
        Assert(largeContext.Count == 1000, "Should handle large datasets");

        // Test 4: Special Characters in Keys
        var specialContext = Context.Create();
        specialContext = specialContext.Insert("key with spaces", "value");
        specialContext = specialContext.Insert("key-with-dashes", "value");
        specialContext = specialContext.Insert("key_with_underscores", "value");

        Assert(specialContext.ContainsKey("key with spaces"), "Should handle spaces in keys");
        Assert(specialContext.ContainsKey("key-with-dashes"), "Should handle dashes in keys");
        Assert(specialContext.ContainsKey("key_with_underscores"), "Should handle underscores in keys");

        Console.WriteLine("✅ Edge Cases: PASSED");
    }

    private static async Task TestPerformance()
    {
        Console.WriteLine("🔍 Testing Performance...");

        var stopwatch = new Stopwatch();

        // Test 1: Chain Performance
        var perfChain = new Chain<object, object>()
            .AddLink("step1", new PerformanceLink())
            .AddLink("step2", new PerformanceLink())
            .AddLink("step3", new PerformanceLink());

        var perfInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["iterations"] = 100
        });

        stopwatch.Start();
        var perfResult = await perfChain.RunAsync(perfInput);
        stopwatch.Stop();

        var executionTime = stopwatch.Elapsed.TotalMilliseconds;
        Assert(executionTime < 1000, $"Chain should execute quickly, took {executionTime}ms");
        Assert(perfResult.Get("total") == 300, "Should accumulate results correctly");

        Console.WriteLine($"✅ Performance: PASSED ({executionTime:F2}ms)");
    }

    private static async Task TestChainComposition()
    {
        Console.WriteLine("🔍 Testing Chain Composition...");

        // Test 1: Nested Chains
        var innerChain = new Chain<object, object>()
            .AddLink("double", new DoubleValueLink());

        var outerChain = new Chain<object, object>()
            .AddLink("convert", new ObjectToStringLink())
            .AddLink("process", new NestedChainLink(innerChain))
            .AddLink("format", new StringToObjectLink());

        var nestedInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["value"] = "10"
        });

        var nestedResult = await outerChain.RunAsync(nestedInput);
        Assert(nestedResult.Get("result")?.ToString() == "40", "Nested chain should work correctly");

        Console.WriteLine("✅ Chain Composition: PASSED");
    }

    private static async Task TestMiddlewareFunctionality()
    {
        Console.WriteLine("🔍 Testing Middleware Functionality...");

        // Test 1: Basic Middleware
        var middlewareChain = new Chain()
            .AddLink("process", new SimpleLink())
            .UseMiddleware(new TimingMiddleware())
            .UseMiddleware(new LoggingMiddleware());

        var middlewareInput = Context.Create(new Dictionary<string, object>
        {
            ["input"] = "test"
        });

        var middlewareResult = await middlewareChain.RunAsync(middlewareInput);
        Assert(middlewareResult.Get("processed")?.ToString() == "TEST", "Middleware chain should work");

        Console.WriteLine("✅ Middleware Functionality: PASSED");
    }

    private static async Task TestAsyncOperations()
    {
        Console.WriteLine("🔍 Testing Async Operations...");

        // Test 1: Async Links
        var asyncChain = new Chain()
            .AddLink("async1", new AsyncDelayLink())
            .AddLink("async2", new AsyncDelayLink());

        var asyncInput = Context.Create(new Dictionary<string, object>
        {
            ["delay"] = 10
        });

        var stopwatch = Stopwatch.StartNew();
        var asyncResult = await asyncChain.RunAsync(asyncInput);
        stopwatch.Stop();

        // Should complete in ~20ms (2 delays of 10ms each)
        Assert(stopwatch.Elapsed.TotalMilliseconds < 100, "Async operations should be efficient");
        Assert(asyncResult.Get("completed") == true, "Async chain should complete successfully");

        Console.WriteLine($"✅ Async Operations: PASSED ({stopwatch.Elapsed.TotalMilliseconds:F2}ms)");
    }

    private static void Assert(bool condition, string message)
    {
        if (condition)
        {
            _passedTests++;
            _testResults.Add($"✅ {message}");
        }
        else
        {
            _failedTests++;
            _testResults.Add($"❌ {message}");
            Console.WriteLine($"❌ ASSERTION FAILED: {message}");
        }
    }
}

// Test Link Implementations
public class StringToIntLink : IContextLink<string, object>
{
    public async Task<Context<object>> CallAsync(Context<string> context)
    {
        var value = context.Get("value");
        if (int.TryParse(value, out int result))
        {
            return Context<object>.Create(new Dictionary<string, object>
            {
                ["result"] = result.ToString()
            });
        }
        throw new InvalidOperationException("Cannot parse to int");
    }
}

public class DoubleIntLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var valueStr = context.Get("result")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return Context<object>.Create(new Dictionary<string, object>
            {
                ["final"] = (value * 2).ToString()
            });
        }
        return context;
    }
}

public class DataProcessorLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var data = context.Get("data")?.ToString() ?? "";
        var multiplier = (int?)context.Get("multiplier") ?? 1;

        return Context<object>.Create(new Dictionary<string, object>
        {
            ["processed"] = data.ToUpper(),
            ["calculated"] = multiplier * 2
        });
    }
}

public class ValidationLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        if (!context.ContainsKey("data"))
            throw new InvalidOperationException("Missing data");

        return context.Insert("validated", true);
    }
}

public class ProcessingLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var data = context.Get("data")?.ToString() ?? "";
        return context.Insert("processed", data.ToUpper());
    }
}

public class FormattingLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var processed = context.Get("processed")?.ToString() ?? "";
        return context.Insert("formatted", $"[{processed}]");
    }
}

public class UntypedProcessorLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        return context.Insert("untyped", "processed");
    }
}

public class TypedProcessorLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        return context.Insert("typed", "processed");
    }
}

public class LegacyProcessor : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var input = context.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("output", input.ToUpper()));
    }
}

public class ModernProcessor : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var output = context.Get("output")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("final", $"{output}-MODERN"));
    }
}

public class LoggingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        Console.WriteLine($"[LOG] Starting: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        Console.WriteLine($"[LOG] Completed: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        Console.WriteLine($"[LOG] Error in {link?.GetType().Name ?? "Chain"}: {exception.Message}");
        return ValueTask.FromResult(context);
    }
}

public class ErrorLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        if (context.Get("trigger")?.ToString() == "error")
            throw new InvalidOperationException("Test error");

        return context;
    }
}

public class SafeLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        return ValueTask.FromResult(context.Insert("safe", "processed"));
    }
}

public class ErrorHandlingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context) => ValueTask.FromResult(context);
    public ValueTask<Context> AfterAsync(ILink? link, Context context) => ValueTask.FromResult(context);

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        return ValueTask.FromResult(context.Insert("handled", true).Insert("error", exception.Message));
    }
}

public class PerformanceLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var iterations = (int?)context.Get("iterations") ?? 10;
        var total = (int?)context.Get("total") ?? 0;

        // Simulate some processing
        for (int i = 0; i < iterations; i++)
        {
            total += 1;
            await Task.Delay(1); // Small delay to simulate work
        }

        return Context<object>.Create(new Dictionary<string, object>
        {
            ["total"] = total
        });
    }
}

public class DoubleValueLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var valueStr = context.Get("result")?.ToString() ?? context.Get("value")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return Context<object>.Create(new Dictionary<string, object>
            {
                ["result"] = (value * 2).ToString()
            });
        }
        return context;
    }
}

public class ObjectToStringLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var value = context.Get("value")?.ToString() ?? "0";
        return Context<object>.Create(new Dictionary<string, object>
        {
            ["string"] = value
        });
    }
}

public class NestedChainLink : IContextLink<object, object>
{
    private readonly Chain<object, object> _innerChain;

    public NestedChainLink(Chain<object, object> innerChain)
    {
        _innerChain = innerChain;
    }

    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        return await _innerChain.RunAsync(context);
    }
}

public class StringToObjectLink : IContextLink<object, object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var value = context.Get("result")?.ToString() ?? "0";
        return Context<object>.Create(new Dictionary<string, object>
        {
            ["final"] = value
        });
    }
}

public class SimpleLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var input = context.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("processed", input.ToUpper()));
    }
}

public class TimingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        return ValueTask.FromResult(context.Insert("start", DateTime.Now));
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        var start = (DateTime?)context.Get("start");
        if (start.HasValue)
        {
            var duration = DateTime.Now - start.Value;
            return ValueTask.FromResult(context.Insert("duration", duration.TotalMilliseconds));
        }
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        return ValueTask.FromResult(context);
    }
}

public class AsyncDelayLink : ILink
{
    public async ValueTask<Context> ProcessAsync(Context context)
    {
        var delay = (int?)context.Get("delay") ?? 100;
        await Task.Delay(delay);
        return context.Insert("delayed", true);
    }
}