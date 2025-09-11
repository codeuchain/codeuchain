using Xunit;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace CodeUChain.Tests;

/// <summary>
/// Tests for the Context class.
/// </summary>
public class ContextTests
{
    [Fact]
    public void Create_Empty_ShouldReturnEmptyContext()
    {
        var context = Context.Create();
        Assert.Equal(0, context.Count);
        Assert.Empty(context.Keys);
    }

    [Fact]
    public void Create_WithData_ShouldContainData()
    {
        var data = new Dictionary<string, object> { ["key"] = "value" };
        var context = Context.Create(data);

        Assert.Equal(1, context.Count);
        Assert.Equal("value", context.Get("key"));
    }

    [Fact]
    public void Insert_ShouldReturnNewContextWithValue()
    {
        var context = Context.Create();
        var newContext = context.Insert("key", "value");

        Assert.Equal(0, context.Count);
        Assert.Equal(1, newContext.Count);
        Assert.Equal("value", newContext.Get("key"));
    }

    [Fact]
    public void Get_Typed_ShouldReturnCorrectType()
    {
        var context = Context.Create();
        var newContext = context.Insert("number", 42);

        Assert.Equal(42, newContext.Get<int>("number"));
        Assert.Equal(0, newContext.Get<int>("nonexistent"));
    }

    [Fact]
    public void Remove_ShouldReturnNewContextWithoutKey()
    {
        var context = Context.Create().Insert("key", "value");
        var newContext = context.Remove("key");

        Assert.Equal(1, context.Count);
        Assert.Equal(0, newContext.Count);
        Assert.Null(newContext.Get("key"));
    }

    [Fact]
    public void ContainsKey_ShouldReturnCorrectResult()
    {
        var context = Context.Create().Insert("key", "value");

        Assert.True(context.ContainsKey("key"));
        Assert.False(context.ContainsKey("nonexistent"));
    }
}

/// <summary>
/// Tests for the Chain class.
/// </summary>
public class ChainTests
{
    [Fact]
    public async Task RunAsync_EmptyChain_ShouldReturnOriginalContext()
    {
        var chain = new Chain();
        var context = Context.Create().Insert("test", "value");

        var result = await chain.RunAsync(context);

        Assert.Equal("value", result.Get("test"));
    }

    [Fact]
    public async Task RunAsync_WithLinks_ShouldExecuteLinks()
    {
        var chain = new Chain();
        var testLink = new TestLink();
        chain = chain.AddLink("test", testLink);

        var context = Context.Create().Insert("input", "test");
        var result = await chain.RunAsync(context);

        Assert.Equal("processed", result.Get("output"));
    }

    [Fact]
    public async Task RunAsync_WithMiddleware_ShouldExecuteMiddleware()
    {
        var chain = new Chain();
        var testLink = new TestLink();
        var testMiddleware = new TestMiddleware();

        chain = chain.AddLink("test", testLink);
        chain = chain.UseMiddleware(testMiddleware);

        var context = Context.Create().Insert("input", "test");
        var result = await chain.RunAsync(context);

        Assert.True(testMiddleware.BeforeCalled);
        Assert.True(testMiddleware.AfterCalled);
    }

    [Fact]
    public async Task RunAsync_LinkThrowsException_ShouldExecuteErrorMiddleware()
    {
        var chain = new Chain();
        var failingLink = new FailingLink();
        var errorMiddleware = new ErrorMiddleware();

        chain = chain.AddLink("failing", failingLink);
        chain = chain.UseMiddleware(errorMiddleware);

        var context = Context.Create();

        // The chain should handle the exception and call error middleware
        var result = await chain.RunAsync(context);

        // Verify that error middleware was called
        Assert.True(errorMiddleware.ErrorCalled);
    }
}

/// <summary>
/// Integration tests for the complete chain functionality.
/// </summary>
public class IntegrationTests
{
    [Fact]
    public async Task MathProcessingChain_ShouldWorkCorrectly()
    {
        var chain = new Chain();

        var addLink = new AddLink();
        var multiplyLink = new MultiplyLink();

        chain = chain.AddLink("add", addLink);
        chain = chain.AddLink("multiply", multiplyLink);

        var data = new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        };

        var input = Context.Create(data);
        var result = await chain.RunAsync(input);

        Assert.Equal(3, result.Get<int>("a"));
        Assert.Equal(4, result.Get<int>("b"));
        Assert.Equal(7, result.Get<int>("sum"));
        Assert.Equal(14, result.Get<int>("result"));
    }

    [Fact]
    public async Task ChainWithLoggingMiddleware_ShouldExecuteWithoutErrors()
    {
        var chain = new Chain();

        var addLink = new AddLink();
        var multiplyLink = new MultiplyLink();
        var loggingMiddleware = new LoggingMiddleware();

        chain = chain.AddLink("add", addLink);
        chain = chain.AddLink("multiply", multiplyLink);
        chain = chain.UseMiddleware(loggingMiddleware);

        var data = new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        };

        var input = Context.Create(data);
        var result = await chain.RunAsync(input);

        Assert.Equal(14, result.Get<int>("result"));
    }
}

/// <summary>
/// Test implementations of links and middleware.
/// </summary>
public class TestLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var result = context.Insert("output", "processed");
        return ValueTask.FromResult(result);
    }
}

public class FailingLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        throw new Exception("Test error");
    }
}

public class TestMiddleware : IMiddleware
{
    public bool BeforeCalled { get; private set; }
    public bool AfterCalled { get; private set; }

    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        BeforeCalled = true;
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        AfterCalled = true;
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        return ValueTask.FromResult(context);
    }
}

public class ErrorMiddleware : IMiddleware
{
    public bool ErrorCalled { get; private set; }

    public ValueTask<Context> BeforeAsync(ILink? link, Context context) => ValueTask.FromResult(context);
    public ValueTask<Context> AfterAsync(ILink? link, Context context) => ValueTask.FromResult(context);

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        ErrorCalled = true;
        return ValueTask.FromResult(context);
    }
}

public class LoggingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return ValueTask.FromResult(context);
    }
}

public class AddLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var a = context.Get<int>("a");
        var b = context.Get<int>("b");

        if (context.ContainsKey("a") && context.ContainsKey("b"))
        {
            var sum = a + b;
            return ValueTask.FromResult(context.Insert("sum", sum));
        }

        return ValueTask.FromResult(context);
    }
}

public class MultiplyLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var sum = context.Get<int>("sum");

        if (context.ContainsKey("sum"))
        {
            var result = sum * 2;
            return ValueTask.FromResult(context.Insert("result", result));
        }

        return ValueTask.FromResult(context);
    }
}