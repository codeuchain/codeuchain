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

        // Test 1: Basic Generic State
        results.Add(await TestGenericState());

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

    private static async Task<(string, bool, string)> TestGenericState()
    {
        try
        {
            // Test basic generic state creation
            var state = State<TestData>.Create(new Dictionary<string, object>
            {
                ["value"] = 42
            });

            // Test typed access
            var value = state.GetAny("value") as int?;
            if (value != 42)
            {
                return ("Generic State", false, "Failed to retrieve typed value");
            }

            // Test insertion
            var newState = state.Insert("result", "success");
            var result = newState.GetAny("result") as string;
            if (result != "success")
            {
                return ("Generic State", false, "Failed to insert value");
            }

            return ("Generic State", true, "All basic operations work");
        }
        catch (Exception ex)
        {
            return ("Generic State", false, $"Exception: {ex.Message}");
        }
    }

    private static async Task<(string, bool, string)> TestTypeEvolution()
    {
        try
        {
            // Start with one type
            var inputState = State<InputData>.Create(new Dictionary<string, object>
            {
                ["numbers"] = new List<int> { 1, 2, 3 }
            });

            // Evolve to another type using InsertAs
            var outputState = inputState.InsertAs<OutputData>("sum", 6);

            // Verify type evolution
            if (!(outputState is State<OutputData>))
            {
                return ("Type Evolution", false, "Type evolution failed");
            }

            // Verify data preservation
            var numbers = outputState.GetAny("numbers") as List<int>;
            var sum = outputState.GetAny("sum") as int?;

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
            var inputState = State<TestData>.Create(new Dictionary<string, object>
            {
                ["input"] = "test"
            });

            var resultState = await link.CallAsync(inputState);

            var output = resultState.GetAny("output") as string;
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

            var input = State<InputData>.Create(new Dictionary<string, object>
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

            var untypedInput = State.Create(new Dictionary<string, object>
            {
                ["data"] = "1,2,3"
            });

            var untypedResult = await untypedChain.RunAsync(untypedInput);

            // Convert to typed state
            var parsedData = untypedResult.Get("parsed") as List<int> ?? new List<int>();
            var typedState = State<InputData>.Create(new Dictionary<string, object>
            {
                ["numbers"] = parsedData
            });

            // Continue with typed processing
            var typedChain = new Chain<InputData, OutputData>()
                .AddLink("sum", new DirectSumLink());

            var finalResult = await typedChain.RunAsync(typedState);
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

            var input = State.Create(new Dictionary<string, object>
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
public class TestGenericLink : IStateLink<TestData, TestData>
{
    public Task<State<TestData>> CallAsync(State<TestData> state)
    {
        var input = state.GetAny("input")?.ToString() ?? "";
        return Task.FromResult(state.Insert("output", input + "_processed"));
    }
}

public class SumLink : IStateLink<InputData, ProcessingData>
{
    public Task<State<ProcessingData>> CallAsync(State<InputData> state)
    {
        var a = state.GetAny("a") as int? ?? 0;
        var b = state.GetAny("b") as int? ?? 0;
        return Task.FromResult(State<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["sum"] = a + b
        }));
    }
}

public class DirectMathLink : IStateLink<InputData, OutputData>
{
    public Task<State<OutputData>> CallAsync(State<InputData> state)
    {
        var a = state.GetAny("a") as int? ?? 0;
        var b = state.GetAny("b") as int? ?? 0;
        return Task.FromResult(State<OutputData>.Create(new Dictionary<string, object>
        {
            ["result"] = (a + b) * 2
        }));
    }
}

public class DirectSumLink : IStateLink<InputData, OutputData>
{
    public Task<State<OutputData>> CallAsync(State<InputData> state)
    {
        var numbers = state.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();
        return Task.FromResult(State<OutputData>.Create(new Dictionary<string, object>
        {
            ["sum"] = sum
        }));
    }
}

public class ProcessingData { }

public class UntypedParseLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var data = state.Get<string>("data");
        var parsed = data?.Split(',').Select(int.Parse).ToList() ?? new List<int>();
        return ValueTask.FromResult(state.Insert("parsed", parsed));
    }
}

public class UntypedAddLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var a = state.Get<int>("a");
        var b = state.Get<int>("b");
        return ValueTask.FromResult(state.Insert("sum", a + b));
    }
}

public class UntypedMultiplyLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var sum = state.Get<int>("sum");
        return ValueTask.FromResult(state.Insert("result", sum * 2));
    }
}