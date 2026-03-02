using Xunit;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace CodeUChain.Tests;

/// <summary>
/// Tests for the State class.
/// </summary>
public class StateTests
{
    [Fact]
    public void Create_Empty_ShouldReturnEmptyState()
    {
        var state = State.Create();
        Assert.Equal(0, state.Count);
        Assert.Empty(state.Keys);
    }

    [Fact]
    public void Create_WithData_ShouldContainData()
    {
        var data = new Dictionary<string, object> { ["key"] = "value" };
        var state = State.Create(data);

        Assert.Equal(1, state.Count);
        Assert.Equal("value", state.Get("key"));
    }

    [Fact]
    public void Insert_ShouldReturnNewStateWithValue()
    {
        var state = State.Create();
        var newState = state.Insert("key", "value");

        Assert.Equal(0, state.Count);
        Assert.Equal(1, newState.Count);
        Assert.Equal("value", newState.Get("key"));
    }

    [Fact]
    public void Get_Typed_ShouldReturnCorrectType()
    {
        var state = State.Create();
        var newState = state.Insert("number", 42);

        Assert.Equal(42, newState.Get<int>("number"));
        Assert.Equal(0, newState.Get<int>("nonexistent"));
    }

    [Fact]
    public void Remove_ShouldReturnNewStateWithoutKey()
    {
        var state = State.Create().Insert("key", "value");
        var newState = state.Remove("key");

        Assert.Equal(1, state.Count);
        Assert.Equal(0, newState.Count);
        Assert.Null(newState.Get("key"));
    }

    [Fact]
    public void ContainsKey_ShouldReturnCorrectResult()
    {
        var state = State.Create().Insert("key", "value");

        Assert.True(state.ContainsKey("key"));
        Assert.False(state.ContainsKey("nonexistent"));
    }
}

/// <summary>
/// Tests for the Chain class.
/// </summary>
public class ChainTests
{
    [Fact]
    public async Task RunAsync_EmptyChain_ShouldReturnOriginalState()
    {
        var chain = new Chain();
        var state = State.Create().Insert("test", "value");

        var result = await chain.RunAsync(state);

        Assert.Equal("value", result.Get("test"));
    }

    [Fact]
    public async Task RunAsync_WithLinks_ShouldExecuteLinks()
    {
        var chain = new Chain();
        var testLink = new TestLink();
        chain = chain.AddLink("test", testLink);

        var state = State.Create().Insert("input", "test");
        var result = await chain.RunAsync(state);

        Assert.Equal("processed", result.Get("output"));
    }

    [Fact]
    public async Task RunAsync_WithHook_ShouldExecuteHook()
    {
        var chain = new Chain();
        var testLink = new TestLink();
        var testHook = new TestHook();

        chain = chain.AddLink("test", testLink);
        chain = chain.UseHook(testHook);

        var state = State.Create().Insert("input", "test");
        var result = await chain.RunAsync(state);

        Assert.True(testHook.BeforeCalled);
        Assert.True(testHook.AfterCalled);
    }

    [Fact]
    public async Task RunAsync_LinkThrowsException_ShouldExecuteErrorHook()
    {
        var chain = new Chain();
        var failingLink = new FailingLink();
        var errorHook = new ErrorHook();

        chain = chain.AddLink("failing", failingLink);
        chain = chain.UseHook(errorHook);

        var state = State.Create();

        await Assert.ThrowsAsync<Exception>(() => chain.RunAsync(state));
        Assert.True(errorHook.ErrorCalled);
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

        var input = State.Create(data);
        var result = await chain.RunAsync(input);

        Assert.Equal(3, result.Get<int>("a"));
        Assert.Equal(4, result.Get<int>("b"));
        Assert.Equal(7, result.Get<int>("sum"));
        Assert.Equal(14, result.Get<int>("result"));
    }

    [Fact]
    public async Task ChainWithLoggingHook_ShouldExecuteWithoutErrors()
    {
        var chain = new Chain();

        var addLink = new AddLink();
        var multiplyLink = new MultiplyLink();
        var loggingHook = new LoggingHook();

        chain = chain.AddLink("add", addLink);
        chain = chain.AddLink("multiply", multiplyLink);
        chain = chain.UseHook(loggingHook);

        var data = new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        };

        var input = State.Create(data);
        var result = await chain.RunAsync(input);

        Assert.Equal(14, result.Get<int>("result"));
    }
}

/// <summary>
/// Test implementations of links and hook.
/// </summary>
public class TestLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        return state.Insert("output", "processed");
    }
}

public class FailingLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        throw new Exception("Test error");
    }
}

public class TestHook : IHook
{
    public bool BeforeCalled { get; private set; }
    public bool AfterCalled { get; private set; }

    public Task<State> BeforeAsync(ILink? link, State state)
    {
        BeforeCalled = true;
        return Task.FromResult(state);
    }

    public Task<State> AfterAsync(ILink? link, State state)
    {
        AfterCalled = true;
        return Task.FromResult(state);
    }

    public Task<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        return Task.FromResult(state);
    }
}

public class ErrorHook : IHook
{
    public bool ErrorCalled { get; private set; }

    public Task<State> BeforeAsync(ILink? link, State state) => Task.FromResult(state);
    public Task<State> AfterAsync(ILink? link, State state) => Task.FromResult(state);

    public Task<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        ErrorCalled = true;
        return Task.FromResult(state);
    }
}

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