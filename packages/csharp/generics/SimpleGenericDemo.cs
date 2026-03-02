using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Simple demonstration of generic patterns in CodeUChain.
/// </summary>
public class SimpleGenericDemo
{
    public static async Task Main(string[] args)
    {
        Console.WriteLine("=== CodeUChain C# Generic Patterns ===\n");

        // Pattern 1: Strongly-typed State (using object for compatibility)
        Console.WriteLine("1. Strongly-Typed State:");
        var state = State<object>.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var resultState = state
            .Insert("sum", (int)state.Get("a")! + (int)state.Get("b")!)
            .Insert("product", (int)state.Get("a")! * (int)state.Get("b")!);

        Console.WriteLine($"State: {resultState}");
        Console.WriteLine($"Sum: {resultState.Get("sum")}, Product: {resultState.Get("product")}\n");

        // Pattern 2: Generic Pipeline
        Console.WriteLine("2. Generic Pipeline:");
        var pipeline = new GenericPipeline<int, string>()
            .AddStep(new IntDoubler())
            .AddStep(new IntFormatter());

        var pipelineResult = pipeline.Execute(5);
        Console.WriteLine($"Pipeline: 5 -> {pipelineResult}\n");

        // Pattern 3: Type-Safe Chain
        Console.WriteLine("3. Type-Safe Chain:");
        var chain = new Chain<object, object>()
            .AddLink("process", new GenericProcessor())
            .AddLink("format", new GenericFormatter());

        var chainInput = State<object>.Create(new Dictionary<string, object>
        {
            ["data"] = "hello"
        });

        var chainResult = await chain.RunAsync(chainInput);
        Console.WriteLine($"Chain result: {chainResult}\n");

        Console.WriteLine("✅ Generic patterns demonstrated successfully!");
    }
}

// Generic Pipeline Implementation
public interface IPipelineStep<TIn, TOut>
{
    TOut Execute(TIn input);
}

public class GenericPipeline<TIn, TOut>
{
    private readonly List<object> _steps = new();
    private readonly List<Type> _inputTypes = new();
    private readonly List<Type> _outputTypes = new();

    public GenericPipeline<TIn, TOut> AddStep<TStepIn, TStepOut>(IPipelineStep<TStepIn, TStepOut> step)
    {
        _steps.Add(step);
        _inputTypes.Add(typeof(TStepIn));
        _outputTypes.Add(typeof(TStepOut));
        return this;
    }

    public TOut Execute(TIn input)
    {
        object current = input!;
        for (int i = 0; i < _steps.Count; i++)
        {
            var step = _steps[i];
            var inputType = _inputTypes[i];
            var outputType = _outputTypes[i];

            // Use reflection to invoke the Execute method with proper types
            var executeMethod = step.GetType().GetMethod("Execute");
            if (executeMethod != null)
            {
                // Convert current to the expected input type
                var convertedInput = Convert.ChangeType(current, inputType);
                current = executeMethod.Invoke(step, new[] { convertedInput })!;
            }
        }
        return (TOut)current;
    }
}

public class IntDoubler : IPipelineStep<int, int>
{
    public int Execute(int input) => input * 2;
}

public class IntFormatter : IPipelineStep<int, string>
{
    public string Execute(int input) => $"[{input}]";
}

// Generic Chain Links
public class GenericProcessor : IStateLink<object, object>
{
    public Task<State<object>> CallAsync(State<object> state)
    {
        var data = state.Get("data")?.ToString() ?? "";
        return Task.FromResult(state.Insert("processed", data.ToUpper()));
    }
}

public class GenericFormatter : IStateLink<object, object>
{
    public Task<State<object>> CallAsync(State<object> state)
    {
        var processed = state.Get("processed")?.ToString() ?? "";
        return Task.FromResult(state.Insert("formatted", $"[{processed}]"));
    }
}