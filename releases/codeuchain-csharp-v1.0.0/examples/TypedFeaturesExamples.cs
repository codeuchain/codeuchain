using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Comprehensive examples demonstrating CodeUChain's typed features.
/// Shows the universal Link[Input, Output] pattern and type evolution.
/// </summary>
public class TypedFeaturesExamples
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== CodeUChain C# Typed Features Examples ===\n");

        await RunTypedVsUntypedComparison();
        await RunTypeEvolutionExample();
        await RunGenericLinkExample();
        await RunMixedUsageExample();
    }

    /// <summary>
    /// Example 1: Side-by-side comparison of typed vs untyped approaches
    /// </summary>
    private static async Task RunTypedVsUntypedComparison()
    {
        Console.WriteLine("=== 1. Typed vs Untyped Comparison ===\n");

        // Untyped approach (existing CodeUChain style)
        Console.WriteLine("--- Untyped Approach ---");
        var untypedChain = new Chain()
            .AddLink("add", new UntypedAddLink())
            .AddLink("multiply", new UntypedMultiplyLink());

        var untypedInput = State.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var untypedResult = await untypedChain.RunAsync(untypedInput);
        Console.WriteLine($"Untyped Result: {untypedResult}");

        // Typed approach (new opt-in feature)
        Console.WriteLine("\n--- Typed Approach ---");
        var typedChain = new Chain<InputData, OutputData>()
            .AddLink("add", new TypedAddLink())
            .AddLink("multiply", new TypedMultiplyLink());

        var typedInput = State<InputData>.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var typedResult = await typedChain.RunAsync(typedInput);
        Console.WriteLine($"Typed Result: {typedResult}");

        Console.WriteLine("\n✅ Both approaches work identically at runtime!\n");
    }

    /// <summary>
    /// Example 2: Type evolution using InsertAs<U>()
    /// </summary>
    private static async Task RunTypeEvolutionExample()
    {
        Console.WriteLine("=== 2. Type Evolution with InsertAs<U>() ===\n");

        // Start with InputData state
        var inputState = State<InputData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = new List<int> { 1, 2, 3 }
        });

        Console.WriteLine($"Initial state: {inputState}");

        // Type evolution: Transform to ProcessingData without casting
        var processingState = inputState.InsertAs<ProcessingData>("sum", 6);
        Console.WriteLine($"After type evolution: {processingState}");

        // Further evolution: Transform to OutputData
        var outputState = processingState.InsertAs<OutputData>("result", 12.0);
        Console.WriteLine($"Final state: {outputState}");

        Console.WriteLine("\n✅ Clean type evolution without explicit casting!\n");
    }

    /// <summary>
    /// Example 3: Generic Link[Input, Output] pattern
    /// </summary>
    private static async Task RunGenericLinkExample()
    {
        Console.WriteLine("=== 3. Generic Link[Input, Output] Pattern ===\n");

        // Create a processing pipeline with clear type transformations
        var processingChain = new Chain<InputData, OutputData>()
            .AddLink("validate", new ValidationLink())
            .AddLink("calculate", new CalculationLink())
            .AddLink("format", new FormattingLink());

        var input = State<InputData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = new List<int> { 1, 2, 3, 4, 5 }
        });

        Console.WriteLine($"Input: {input}");

        var result = await processingChain.RunAsync(input);
        Console.WriteLine($"Result: {result}");

        Console.WriteLine("\n✅ Type-safe pipeline with clear input/output contracts!\n");
    }

    /// <summary>
    /// Example 4: Mixed usage - typed and untyped components together
    /// </summary>
    private static async Task RunMixedUsageExample()
    {
        Console.WriteLine("=== 4. Mixed Usage - Typed and Untyped Together ===\n");

        // Start with untyped chain
        var untypedChain = new Chain()
            .AddLink("parse", new UntypedParseLink())
            .AddLink("validate", new UntypedValidateLink());

        var untypedInput = State.Create(new Dictionary<string, object>
        {
            ["rawData"] = "1,2,3,4,5"
        });

        var untypedResult = await untypedChain.RunAsync(untypedInput);
        Console.WriteLine($"Untyped processing result: {untypedResult}");

        // Convert to typed state for further processing
        var typedState = State<InputData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = untypedResult.Get("parsedNumbers")
        });

        // Continue with typed processing
        var typedChain = new Chain<InputData, OutputData>()
            .AddLink("calculate", new CalculationLink())
            .AddLink("format", new FormattingLink());

        var finalResult = await typedChain.RunAsync(typedState);
        Console.WriteLine($"Final typed result: {finalResult}");

        Console.WriteLine("\n✅ Seamless transition between typed and untyped code!\n");
    }
}

// Data shape classes for type safety
public class InputData { }
public class ProcessingData { }
public class OutputData { }

// Untyped link implementations (existing CodeUChain style)
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

public class UntypedParseLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var rawData = state.Get<string>("rawData");
        var numbers = rawData?.Split(',').Select(int.Parse).ToList();
        return ValueTask.FromResult(state.Insert("parsedNumbers", numbers));
    }
}

public class UntypedValidateLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var numbers = state.Get<List<int>>("parsedNumbers");
        if (numbers == null || !numbers.Any())
        {
            throw new InvalidOperationException("No numbers to process");
        }
        return ValueTask.FromResult(state.Insert("validated", true));
    }
}

// Typed link implementations (new opt-in feature)
public class TypedAddLink : IStateLink<InputData, ProcessingData>
{
    public async Task<State<ProcessingData>> CallAsync(State<InputData> state)
    {
        // Type-safe access to input data
        var numbers = state.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();

        // Return new state with evolved type
        return State<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = numbers,
            ["sum"] = sum
        });
    }
}

public class TypedMultiplyLink : IStateLink<ProcessingData, OutputData>
{
    public async Task<State<OutputData>> CallAsync(State<ProcessingData> state)
    {
        var sum = state.GetAny("sum") as int? ?? 0;
        var result = sum * 2;

        return State<OutputData>.Create(new Dictionary<string, object>
        {
            ["sum"] = sum,
            ["result"] = result
        });
    }
}

public class ValidationLink : IStateLink<InputData, InputData>
{
    public async Task<State<InputData>> CallAsync(State<InputData> state)
    {
        var numbers = state.GetAny("numbers") as List<int>;
        if (numbers == null || !numbers.Any())
        {
            throw new InvalidOperationException("Input must contain numbers");
        }
        return state.Insert("validated", true);
    }
}

public class CalculationLink : IStateLink<InputData, ProcessingData>
{
    public async Task<State<ProcessingData>> CallAsync(State<InputData> state)
    {
        var numbers = state.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();
        var average = numbers.Average();
        var count = numbers.Count;

        return State<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = numbers,
            ["sum"] = sum,
            ["average"] = average,
            ["count"] = count
        });
    }
}

public class FormattingLink : IStateLink<ProcessingData, OutputData>
{
    public async Task<State<OutputData>> CallAsync(State<ProcessingData> state)
    {
        var numbers = state.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = state.GetAny("sum") as int? ?? 0;
        var average = state.GetAny("average") as double? ?? 0.0;
        var count = state.GetAny("count") as int? ?? 0;

        var formatted = $"Processed {count} numbers: {string.Join(", ", numbers)} = Sum: {sum}, Avg: {average:F2}";

        return State<OutputData>.Create(new Dictionary<string, object>
        {
            ["formatted"] = formatted,
            ["summary"] = new { sum, average, count }
        });
    }
}

/// <summary>
/// Program entry point for typed features examples.
/// </summary>
public class TypedFeaturesProgram
{
    public static async Task Main(string[] args)
    {
        Console.WriteLine("🎯 CodeUChain C# Typed Features Demonstration\n");

        try
        {
            await TypedFeaturesExamples.RunAsync();
            Console.WriteLine("🎉 All typed features examples completed successfully!");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"❌ Error: {ex.Message}");
        }
    }
}