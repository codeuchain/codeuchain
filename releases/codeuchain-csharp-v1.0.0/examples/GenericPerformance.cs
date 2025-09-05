using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading.Tasks;

/// <summary>
/// Performance comparison: Generic vs Non-Generic
/// </summary>
public class GenericPerformanceComparison
{
    public static async Task RunComparisonAsync()
    {
        Console.WriteLine("=== Generic vs Non-Generic Performance ===\n");

        const int iterations = 100000;

        // Setup generic chain
        var genericChain = new Chain<object>()
            .AddLink("add", new GenericAddLink())
            .AddLink("multiply", new GenericMultiplyLink());

        // Setup non-generic chain
        var nonGenericChain = new Chain()
            .AddLink("add", new NonGenericAddLink())
            .AddLink("multiply", new NonGenericMultiplyLink());

        var genericInput = Context<object>.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var nonGenericInput = Context.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        // Warm up
        Console.WriteLine("Warming up...");
        for (int i = 0; i < 1000; i++)
        {
            await genericChain.RunAsync(genericInput);
            await nonGenericChain.RunAsync(nonGenericInput);
        }

        // Measure generic performance
        Console.WriteLine($"Running {iterations} generic iterations...");
        var genericStopwatch = Stopwatch.StartNew();
        for (int i = 0; i < iterations; i++)
        {
            await genericChain.RunAsync(genericInput);
        }
        genericStopwatch.Stop();

        // Measure non-generic performance
        Console.WriteLine($"Running {iterations} non-generic iterations...");
        var nonGenericStopwatch = Stopwatch.StartNew();
        for (int i = 0; i < iterations; i++)
        {
            await nonGenericChain.RunAsync(nonGenericInput);
        }
        nonGenericStopwatch.Stop();

        // Results
        var genericTime = genericStopwatch.ElapsedMilliseconds;
        var nonGenericTime = nonGenericStopwatch.ElapsedMilliseconds;

        Console.WriteLine($"\nResults:");
        Console.WriteLine($"Generic execution:     {genericTime}ms");
        Console.WriteLine($"Non-generic execution: {nonGenericTime}ms");
        Console.WriteLine($"Difference:            {genericTime - nonGenericTime}ms");

        if (genericTime < nonGenericTime)
        {
            Console.WriteLine($"Generic is {((double)nonGenericTime / genericTime - 1) * 100:F1}% faster");
        }
        else
        {
            Console.WriteLine($"Generic has {((double)genericTime / nonGenericTime - 1) * 100:F1}% overhead");
        }

        Console.WriteLine($"\nPer-iteration analysis:");
        Console.WriteLine($"Generic time/iter:     {genericTime / (double)iterations:F4}ms");
        Console.WriteLine($"Non-generic time/iter: {nonGenericTime / (double)iterations:F4}ms");
    }
}

// Generic implementations
public class GenericAddLink : IContextLink<object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var a = (int)context.Get("a")!;
        var b = (int)context.Get("b")!;
        return context.Insert("sum", a + b);
    }
}

public class GenericMultiplyLink : IContextLink<object>
{
    public async Task<Context<object>> CallAsync(Context<object> context)
    {
        var sum = (int)context.Get("sum")!;
        return context.Insert("result", sum * 2);
    }
}

// Non-generic implementations
public class NonGenericAddLink : ILink
{
    public async Task<Context> CallAsync(Context context)
    {
        var a = (int)context.Get("a")!;
        var b = (int)context.Get("b")!;
        return context.Insert("sum", a + b);
    }
}

public class NonGenericMultiplyLink : ILink
{
    public async Task<Context> CallAsync(Context context)
    {
        var sum = (int)context.Get("sum")!;
        return context.Insert("result", sum * 2);
    }
}

/// <summary>
/// Advanced Generic Patterns
/// </summary>
public class AdvancedGenericPatterns
{
    public static void DemonstratePatterns()
    {
        Console.WriteLine("=== Advanced Generic Patterns ===\n");

        // Pattern 1: Strongly-typed pipeline
        var intPipeline = new TypedPipeline<int, string>()
            .AddStep(new IntToStringStep())
            .AddStep(new StringFormatterStep());

        var intResult = intPipeline.Execute(42);
        Console.WriteLine($"Int pipeline: 42 -> {intResult}");

        // Pattern 2: Generic result aggregation
        var aggregator = new ResultAggregator<int>();
        aggregator.AddResult(10);
        aggregator.AddResult(20);
        aggregator.AddResult(30);

        Console.WriteLine($"Sum: {aggregator.Sum}");
        Console.WriteLine($"Average: {aggregator.Average}");
        Console.WriteLine($"Count: {aggregator.Count}\n");

        // Pattern 3: Generic validation pipeline
        var validator = new ValidationPipeline<User>()
            .AddRule(new AgeValidationRule())
            .AddRule(new EmailValidationRule());

        var user = new User { Name = "John", Age = 25, Email = "john@example.com" };
        var validationResult = validator.Validate(user);

        Console.WriteLine($"User validation: {(validationResult.IsValid ? "Valid" : "Invalid")}");
        if (!validationResult.IsValid)
        {
            foreach (var error in validationResult.Errors)
            {
                Console.WriteLine($"  - {error}");
            }
        }
    }
}

// Pattern 1: Typed Pipeline
public interface IPipelineStep<TIn, TOut>
{
    TOut Execute(TIn input);
}

public class TypedPipeline<TIn, TOut>
{
    private readonly List<object> _steps = new();

    public TypedPipeline<TIn, TIntermediate> AddStep<TIntermediate>(IPipelineStep<TIn, TIntermediate> step)
    {
        _steps.Add(step);
        return new TypedPipeline<TIn, TIntermediate>();
    }

    public TOut Execute(TIn input)
    {
        object result = input!;
        foreach (var step in _steps)
        {
            var method = step.GetType().GetMethod("Execute");
            result = method!.Invoke(step, new[] { result })!;
        }
        return (TOut)result;
    }
}

public class IntToStringStep : IPipelineStep<int, string>
{
    public string Execute(int input) => input.ToString();
}

public class StringFormatterStep : IPipelineStep<string, string>
{
    public string Execute(string input) => $"[{input}]";
}

// Pattern 2: Generic Result Aggregator
public class ResultAggregator<T> where T : struct, IConvertible
{
    private readonly List<T> _results = new();

    public void AddResult(T result) => _results.Add(result);

    public int Count => _results.Count;

    public T Sum => (T)Convert.ChangeType(_results.Sum(x => Convert.ToDouble(x)), typeof(T));

    public double Average => _results.Count > 0 ? _results.Average(x => Convert.ToDouble(x)) : 0;
}

// Pattern 3: Generic Validation
public class ValidationResult
{
    public bool IsValid { get; set; } = true;
    public List<string> Errors { get; } = new();
}

public interface IValidationRule<T>
{
    ValidationResult Validate(T entity);
}

public class ValidationPipeline<T>
{
    private readonly List<IValidationRule<T>> _rules = new();

    public ValidationPipeline<T> AddRule(IValidationRule<T> rule)
    {
        _rules.Add(rule);
        return this;
    }

    public ValidationResult Validate(T entity)
    {
        var result = new ValidationResult();

        foreach (var rule in _rules)
        {
            var ruleResult = rule.Validate(entity);
            if (!ruleResult.IsValid)
            {
                result.IsValid = false;
                result.Errors.AddRange(ruleResult.Errors);
            }
        }

        return result;
    }
}

public class User
{
    public string Name { get; set; } = "";
    public int Age { get; set; }
    public string Email { get; set; } = "";
}

public class AgeValidationRule : IValidationRule<User>
{
    public ValidationResult Validate(User user)
    {
        var result = new ValidationResult();
        if (user.Age < 18)
        {
            result.IsValid = false;
            result.Errors.Add("User must be 18 or older");
        }
        return result;
    }
}

public class EmailValidationRule : IValidationRule<User>
{
    public ValidationResult Validate(User user)
    {
        var result = new ValidationResult();
        if (string.IsNullOrEmpty(user.Email) || !user.Email.Contains("@"))
        {
            result.IsValid = false;
            result.Errors.Add("Valid email is required");
        }
        return result;
    }
}