using System;
using System.Collections.Generic;
using System.Threading.Tasks;

/// <summary>
/// Example demonstrating a math processing chain with hook.
/// </summary>
public class MathProcessingExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== CodeUChain C# Math Processing Example ===\n");

        var chain = new Chain();

        // Link that adds two numbers
        var addLink = new AddLink();

        // Link that multiplies result by 2
        var multiplyLink = new MultiplyLink();

        chain = chain.AddLink("add", addLink);
        chain = chain.AddLink("multiply", multiplyLink);

        // Add logging hook
        var loggingHook = new LoggingHook();
        chain = chain.UseHook(loggingHook);

        // Prepare input data
        var data = new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        };

        var input = State.Create(data);
        Console.WriteLine($"Input: {input}");

        try
        {
            var result = await chain.RunAsync(input);
            Console.WriteLine($"Result: {result}");

            // Verify the chain worked: (3 + 4) * 2 = 14
            var a = result.Get<int>("a");
            var b = result.Get<int>("b");
            var sum = result.Get<int>("sum");
            var finalResult = result.Get<int>("result");

            Console.WriteLine($"\nVerification:");
            Console.WriteLine($"a = {a}, b = {b}");
            Console.WriteLine($"sum = {sum} (expected: 7)");
            Console.WriteLine($"result = {finalResult} (expected: 14)");

            if (sum == 7 && finalResult == 14)
            {
                Console.WriteLine("\n✅ Chain execution successful!");
            }
            else
            {
                Console.WriteLine("\n❌ Chain execution failed!");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"❌ Error during chain execution: {ex.Message}");
        }
    }
}

/// <summary>
/// Link that adds two numbers from the state.
/// </summary>
public class AddLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        var a = state.Get<int>("a");
        var b = state.Get<int>("b");

        if (state.ContainsKey("a") && state.ContainsKey("b"))
        {
            var sum = a + b;
            return state.Insert("sum", sum);
        }

        return state;
    }
}

/// <summary>
/// Link that multiplies the sum by 2.
/// </summary>
public class MultiplyLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        var sum = state.Get<int>("sum");

        if (state.ContainsKey("sum"))
        {
            var result = sum * 2;
            return state.Insert("result", result);
        }

        return state;
    }
}

/// <summary>
/// Hook that logs execution flow.
/// </summary>
public class LoggingHook : IHook
{
    public Task<State> BeforeAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> AfterAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return Task.FromResult(state);
    }
}

/// <summary>
/// Program entry point for the example.
/// </summary>
public class MathProcessingExample
{
    public static async Task RunAsync()
    {
        Console.WriteLine("=== CodeUChain C# Math Processing Example ===\n");

        var chain = new Chain();

        // Link that adds two numbers
        var addLink = new AddLink();

        // Link that multiplies result by 2
        var multiplyLink = new MultiplyLink();

        chain = chain.AddLink("add", addLink);
        chain = chain.AddLink("multiply", multiplyLink);

        // Add logging hook
        var loggingHook = new LoggingHook();
        chain = chain.UseHook(loggingHook);

        // Prepare input data
        var data = new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        };

        var input = State.Create(data);
        Console.WriteLine($"Input: {input}");

        try
        {
            var result = await chain.RunAsync(input);
            Console.WriteLine($"Result: {result}");

            // Verify the chain worked: (3 + 4) * 2 = 14
            var a = result.Get<int>("a");
            var b = result.Get<int>("b");
            var sum = result.Get<int>("sum");
            var finalResult = result.Get<int>("result");

            Console.WriteLine($"\nVerification:");
            Console.WriteLine($"a = {a}, b = {b}");
            Console.WriteLine($"sum = {sum} (expected: 7)");
            Console.WriteLine($"result = {finalResult} (expected: 14)");

            if (sum == 7 && finalResult == 14)
            {
                Console.WriteLine("\n✅ Chain execution successful!");
            }
            else
            {
                Console.WriteLine("\n❌ Chain execution failed!");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"❌ Error during chain execution: {ex.Message}");
        }
    }
}

/// <summary>
/// Link that adds two numbers from the state.
/// </summary>
public class AddLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        var a = state.Get<int>("a");
        var b = state.Get<int>("b");

        if (state.ContainsKey("a") && state.ContainsKey("b"))
        {
            var sum = a + b;
            return state.Insert("sum", sum);
        }

        return state;
    }
}

/// <summary>
/// Link that multiplies the sum by 2.
/// </summary>
public class MultiplyLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        var sum = state.Get<int>("sum");

        if (state.ContainsKey("sum"))
        {
            var result = sum * 2;
            return state.Insert("result", result);
        }

        return state;
    }
}

/// <summary>
/// Hook that logs execution flow.
/// </summary>
public class LoggingHook : IHook
{
    public Task<State> BeforeAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> AfterAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return Task.FromResult(state);
    }
}