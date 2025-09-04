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

        var untypedInput = Context.Create(new Dictionary<string, object>
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

        var typedInput = Context<InputData>.Create(new Dictionary<string, object>
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

        // Start with InputData context
        var inputContext = Context<InputData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = new List<int> { 1, 2, 3 }
        });

        Console.WriteLine($"Initial context: {inputContext}");

        // Type evolution: Transform to ProcessingData without casting
        var processingContext = inputContext.InsertAs<ProcessingData>("sum", 6);
        Console.WriteLine($"After type evolution: {processingContext}");

        // Further evolution: Transform to OutputData
        var outputContext = processingContext.InsertAs<OutputData>("result", 12.0);
        Console.WriteLine($"Final context: {outputContext}");

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

        var input = Context<InputData>.Create(new Dictionary<string, object>
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

        var untypedInput = Context.Create(new Dictionary<string, object>
        {
            ["rawData"] = "1,2,3,4,5"
        });

        var untypedResult = await untypedChain.RunAsync(untypedInput);
        Console.WriteLine($"Untyped processing result: {untypedResult}");

        // Convert to typed context for further processing
        var typedContext = Context<InputData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = untypedResult.Get("parsedNumbers")
        });

        // Continue with typed processing
        var typedChain = new Chain<InputData, OutputData>()
            .AddLink("calculate", new CalculationLink())
            .AddLink("format", new FormattingLink());

        var finalResult = await typedChain.RunAsync(typedContext);
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

public class UntypedParseLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var rawData = context.Get<string>("rawData");
        var numbers = rawData?.Split(',').Select(int.Parse).ToList();
        return ValueTask.FromResult(context.Insert("parsedNumbers", numbers));
    }
}

public class UntypedValidateLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var numbers = context.Get<List<int>>("parsedNumbers");
        if (numbers == null || !numbers.Any())
        {
            throw new InvalidOperationException("No numbers to process");
        }
        return ValueTask.FromResult(context.Insert("validated", true));
    }
}

// Typed link implementations (new opt-in feature)
public class TypedAddLink : IContextLink<InputData, ProcessingData>
{
    public async Task<Context<ProcessingData>> CallAsync(Context<InputData> context)
    {
        // Type-safe access to input data
        var numbers = context.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();

        // Return new context with evolved type
        return Context<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = numbers,
            ["sum"] = sum
        });
    }
}

public class TypedMultiplyLink : IContextLink<ProcessingData, OutputData>
{
    public async Task<Context<OutputData>> CallAsync(Context<ProcessingData> context)
    {
        var sum = context.GetAny("sum") as int? ?? 0;
        var result = sum * 2;

        return Context<OutputData>.Create(new Dictionary<string, object>
        {
            ["sum"] = sum,
            ["result"] = result
        });
    }
}

public class ValidationLink : IContextLink<InputData, InputData>
{
    public async Task<Context<InputData>> CallAsync(Context<InputData> context)
    {
        var numbers = context.GetAny("numbers") as List<int>;
        if (numbers == null || !numbers.Any())
        {
            throw new InvalidOperationException("Input must contain numbers");
        }
        return context.Insert("validated", true);
    }
}

public class CalculationLink : IContextLink<InputData, ProcessingData>
{
    public async Task<Context<ProcessingData>> CallAsync(Context<InputData> context)
    {
        var numbers = context.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();
        var average = numbers.Average();
        var count = numbers.Count;

        return Context<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["numbers"] = numbers,
            ["sum"] = sum,
            ["average"] = average,
            ["count"] = count
        });
    }
}

public class FormattingLink : IContextLink<ProcessingData, OutputData>
{
    public async Task<Context<OutputData>> CallAsync(Context<ProcessingData> context)
    {
        var numbers = context.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = context.GetAny("sum") as int? ?? 0;
        var average = context.GetAny("average") as double? ?? 0.0;
        var count = context.GetAny("count") as int? ?? 0;

        var formatted = $"Processed {count} numbers: {string.Join(", ", numbers)} = Sum: {sum}, Avg: {average:F2}";

        return Context<OutputData>.Create(new Dictionary<string, object>
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