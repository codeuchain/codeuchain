using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Simple test runner for CodeUChain typed features.
/// Validates the implementation without complex build dependencies.
/// </summary>
public class TypedFeaturesTestRunner
{
    public static async Task Main(string[] args)
    {
        Console.WriteLine("🧪 CodeUChain C# Typed Features Test Runner\n");

        var results = new List<(string TestName, bool Passed, string Message)>();

        // Test 1: Basic Generic Context
        results.Add(await TestGenericContext());

        // Test 2: Type Evolution with InsertAs
        results.Add(await TestTypeEvolution());

        // Test 3: Generic Link Interface
        results.Add(await TestGenericLink());

        // Test 4: Generic Chain Execution
        results.Add(await TestGenericChain());

        // Test 5: Mixed Typed/Untyped Usage
        results.Add(await TestMixedUsage());

        // Test 6: Backward Compatibility
        results.Add(await TestBackwardCompatibility());

        // Display Results
        Console.WriteLine("📊 Test Results:\n");

        int passed = 0;
        int failed = 0;

        foreach (var (testName, passedTest, message) in results)
        {
            var status = passedTest ? "✅ PASS" : "❌ FAIL";
            Console.WriteLine($"{status} {testName}");
            if (!passedTest)
            {
                Console.WriteLine($"   {message}");
            }

            if (passedTest) passed++;
            else failed++;
        }

        Console.WriteLine($"\n📈 Summary: {passed} passed, {failed} failed");

        if (failed == 0)
        {
            Console.WriteLine("🎉 All tests passed! Typed features implementation is working correctly.");
        }
        else
        {
            Console.WriteLine("⚠️  Some tests failed. Please review the implementation.");
        }
    }

    private static async Task<(string, bool, string)> TestGenericContext()
    {
        try
        {
            // Test basic generic context creation
            var context = Context<TestData>.Create(new Dictionary<string, object>
            {
                ["value"] = 42
            });

            // Test typed access
            var value = context.GetAny("value") as int?;
            if (value != 42)
            {
                return ("Generic Context", false, "Failed to retrieve typed value");
            }

            // Test insertion
            var newContext = context.Insert("result", "success");
            var result = newContext.GetAny("result") as string;
            if (result != "success")
            {
                return ("Generic Context", false, "Failed to insert value");
            }

            return ("Generic Context", true, "All basic operations work");
        }
        catch (Exception ex)
        {
            return ("Generic Context", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestTypeEvolution()
    {
        try
        {
            // Start with one type
            var inputContext = Context<InputData>.Create(new Dictionary<string, object>
            {
                ["numbers"] = new List<int> { 1, 2, 3 }
            });

            // Evolve to another type using InsertAs
            var outputContext = inputContext.InsertAs<OutputData>("sum", 6);

            // Verify type evolution
            if (!(outputContext is Context<OutputData>))
            {
                return ("Type Evolution", false, "Type evolution failed");
            }

            // Verify data preservation
            var numbers = outputContext.GetAny("numbers") as List<int>;
            var sum = outputContext.GetAny("sum") as int?;

            if (numbers == null || sum != 6)
            {
                return ("Type Evolution", false, "Data not preserved during type evolution");
            }

            return ("Type Evolution", true, "Type evolution works correctly");
        }
        catch (Exception ex)
        {
            return ("Type Evolution", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestGenericLink()
    {
        try
        {
            var link = new TestGenericLink();
            var inputContext = Context<TestData>.Create(new Dictionary<string, object>
            {
                ["input"] = "test"
            });

            var resultContext = await link.CallAsync(inputContext);

            var output = resultContext.GetAny("output") as string;
            if (output != "test_processed")
            {
                return ("Generic Link", false, "Link processing failed");
            }

            return ("Generic Link", true, "Generic link interface works");
        }
        catch (Exception ex)
        {
            return ("Generic Link", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestGenericChain()
    {
        try
        {
            var chain = new Chain<InputData, OutputData>()
                .AddLink("process", new DirectMathLink());

            var input = Context<InputData>.Create(new Dictionary<string, object>
            {
                ["a"] = 3,
                ["b"] = 4
            });

            var result = await chain.RunAsync(input);

            var final = result.GetAny("result") as int?;
            if (final != 14) // (3 + 4) * 2 = 14
            {
                return ("Generic Chain", false, $"Expected 14, got {final}");
            }

            return ("Generic Chain", true, "Generic chain execution works");
        }
        catch (Exception ex)
        {
            return ("Generic Chain", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestMixedUsage()
    {
        try
        {
            // Start with untyped processing
            var untypedChain = new Chain()
                .AddLink("parse", new UntypedParseLink());

            var untypedInput = Context.Create(new Dictionary<string, object>
            {
                ["data"] = "1,2,3"
            });

            var untypedResult = await untypedChain.RunAsync(untypedInput);

            // Convert to typed context
            var parsedData = untypedResult.Get("parsed") as List<int> ?? new List<int>();
            var typedContext = Context<InputData>.Create(new Dictionary<string, object>
            {
                ["numbers"] = parsedData
            });

            // Continue with typed processing
            var typedChain = new Chain<InputData, OutputData>()
                .AddLink("sum", new DirectSumLink());

            var finalResult = await typedChain.RunAsync(typedContext);
            var sum = finalResult.GetAny("sum") as int?;

            if (sum != 6)
            {
                return ("Mixed Usage", false, $"Expected sum 6, got {sum}");
            }

            return ("Mixed Usage", true, "Mixed typed/untyped usage works");
        }
        catch (Exception ex)
        {
            return ("Mixed Usage", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestBackwardCompatibility()
    {
        try
        {
            // Test that existing untyped code still works
            var chain = new Chain()
                .AddLink("add", new UntypedAddLink())
                .AddLink("multiply", new UntypedMultiplyLink());

            var input = Context.Create(new Dictionary<string, object>
            {
                ["a"] = 5,
                ["b"] = 3
            });

            var result = await chain.RunAsync(input);

            var final = result.Get<int>("result");
            if (final != 16) // (5 + 3) * 2 = 16
            {
                return ("Backward Compatibility", false, $"Expected 16, got {final}");
            }

            return ("Backward Compatibility", true, "Existing untyped code works unchanged");
        }
        catch (Exception ex)
        {
            return ("Backward Compatibility", false, $"Exception: {ex.Message}");
        }
    }
}

// Test data classes
public class TestData { }
public class InputData { }
public class OutputData { }

// Test implementations
public class TestGenericLink : IContextLink<TestData, TestData>
{
    public Task<Context<TestData>> CallAsync(Context<TestData> context)
    {
        var input = context.GetAny("input")?.ToString() ?? "";
        return Task.FromResult(context.Insert("output", input + "_processed"));
    }
}

public class SumLink : IContextLink<InputData, ProcessingData>
{
    public Task<Context<ProcessingData>> CallAsync(Context<InputData> context)
    {
        var a = context.GetAny("a") as int? ?? 0;
        var b = context.GetAny("b") as int? ?? 0;
        return Task.FromResult(Context<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["sum"] = a + b
        }));
    }
}

public class DirectMathLink : IContextLink<InputData, OutputData>
{
    public Task<Context<OutputData>> CallAsync(Context<InputData> context)
    {
        var a = context.GetAny("a") as int? ?? 0;
        var b = context.GetAny("b") as int? ?? 0;
        return Task.FromResult(Context<OutputData>.Create(new Dictionary<string, object>
        {
            ["result"] = (a + b) * 2
        }));
    }
}

public class DirectSumLink : IContextLink<InputData, OutputData>
{
    public Task<Context<OutputData>> CallAsync(Context<InputData> context)
    {
        var numbers = context.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();
        return Task.FromResult(Context<OutputData>.Create(new Dictionary<string, object>
        {
            ["sum"] = sum
        }));
    }
}

public class ProcessingData { }

public class UntypedParseLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var data = context.Get<string>("data");
        var parsed = data?.Split(',').Select(int.Parse).ToList() ?? new List<int>();
        return ValueTask.FromResult(context.Insert("parsed", parsed));
    }
}

public class UntypedAddLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var a = context.Get<int>("a");
        var b = context.Get<int>("b");
        return ValueTask.FromResult(context.Insert("sum", a + b));
    }
}

public class UntypedMultiplyLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var sum = context.Get<int>("sum");
        return ValueTask.FromResult(context.Insert("result", sum * 2));
    }
}