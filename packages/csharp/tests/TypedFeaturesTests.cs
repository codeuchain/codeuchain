using Xunit;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace CodeUChain.Tests;

/// <summary>
/// Tests for generic State<T> with type evolution.
/// </summary>
public class GenericStateTests
{
    [Fact]
    public void Create_GenericEmpty_ShouldReturnEmptyState()
    {
        var state = State<TestData>.Create();
        Assert.Equal(0, state.Count);
        Assert.Empty(state.Keys);
    }

    [Fact]
    public void Create_GenericWithData_ShouldContainData()
    {
        var data = new Dictionary<string, object> { ["key"] = "value" };
        var state = State<TestData>.Create(data);

        Assert.Equal(1, state.Count);
        Assert.Equal("value", state.GetAny("key"));
    }

    [Fact]
    public void Insert_Generic_ShouldReturnNewStateWithValue()
    {
        var state = State<TestData>.Create();
        var newState = state.Insert("key", "value");

        Assert.Equal(0, state.Count);
        Assert.Equal(1, newState.Count);
        Assert.Equal("value", newState.GetAny("key"));
    }

    [Fact]
    public void InsertAs_TypeEvolution_ShouldReturnStateOfNewType()
    {
        var originalState = State<TestData>.Create();
        var evolvedState = originalState.InsertAs<ProcessingData>("result", 42);

        // Verify the type evolution worked
        Assert.IsType<State<ProcessingData>>(evolvedState);
        Assert.Equal(42, evolvedState.GetAny("result"));
    }

    [Fact]
    public void Get_TypedGeneric_ShouldReturnCorrectType()
    {
        var state = State<TestData>.Create();
        var newState = state.Insert("number", 42);

        // Get as typed value
        var number = newState.GetAny("number") as int?;
        Assert.Equal(42, number);
    }

    [Fact]
    public void Remove_Generic_ShouldReturnNewStateWithoutKey()
    {
        var state = State<TestData>.Create().Insert("key", "value");
        var newState = state.Remove("key");

        Assert.Equal(1, state.Count);
        Assert.Equal(0, newState.Count);
        Assert.Null(newState.GetAny("key"));
    }
}

/// <summary>
/// Tests for generic ILink<TInput, TOutput> interface.
/// </summary>
public class GenericLinkTests
{
    [Fact]
    public async Task GenericLink_ProcessAsync_ShouldTransformStateTypes()
    {
        var link = new TestGenericLink();
        var inputState = State<TestData>.Create(new Dictionary<string, object>
        {
            ["input"] = "test"
        });

        var resultState = await link.CallAsync(inputState);

        Assert.IsType<State<ProcessingData>>(resultState);
        Assert.Equal("processed", resultState.GetAny("output"));
    }
}

/// <summary>
/// Tests for generic Chain<TInput, TOutput>.
/// </summary>
public class GenericChainTests
{
    [Fact]
    public async Task RunAsync_EmptyGenericChain_ShouldReturnOriginalState()
    {
        var chain = new Chain<TestData, TestData>();
        var state = State<TestData>.Create().Insert("test", "value");

        var result = await chain.RunAsync(state);

        Assert.Equal("value", result.GetAny("test"));
    }

    [Fact]
    public async Task RunAsync_WithGenericLinks_ShouldExecuteLinksWithTypeEvolution()
    {
        var chain = new Chain<TestData, ProcessingData>()
            .AddLink("process", new TestGenericLink());

        var inputState = State<TestData>.Create(new Dictionary<string, object>
        {
            ["input"] = "test"
        });

        var resultState = await chain.RunAsync(inputState);

        Assert.IsType<State<ProcessingData>>(resultState);
        Assert.Equal("processed", resultState.GetAny("output"));
    }

    [Fact]
    public async Task RunAsync_MultipleLinksWithTypeEvolution_ShouldWorkCorrectly()
    {
        var chain = new Chain<TestData, OutputData>()
            .AddLink("step1", new Step1Link())
            .AddLink("step2", new Step2Link());

        var inputState = State<TestData>.Create(new Dictionary<string, object>
        {
            ["value"] = 10
        });

        var resultState = await chain.RunAsync(inputState);

        Assert.IsType<State<OutputData>>(resultState);
        Assert.Equal(25, resultState.GetAny("final"));
    }
}

/// <summary>
/// Integration tests for typed features.
/// </summary>
public class TypedFeaturesIntegrationTests
{
    [Fact]
    public async Task TypedVsUntyped_SameRuntimeBehavior()
    {
        // Untyped version
        var untypedChain = new Chain()
            .AddLink("add", new UntypedMathLink());

        var untypedInput = State.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var untypedResult = await untypedChain.RunAsync(untypedInput);

        // Typed version
        var typedChain = new Chain<MathInput, MathOutput>()
            .AddLink("add", new TypedMathLink());

        var typedInput = State<MathInput>.Create(new Dictionary<string, object>
        {
            ["a"] = 3,
            ["b"] = 4
        });

        var typedResult = await typedChain.RunAsync(typedInput);

        // Both should produce the same result
        Assert.Equal(7, untypedResult.Get<int>("sum"));
        Assert.Equal(7, typedResult.GetAny("sum"));
    }

    [Fact]
    public async Task TypeEvolution_InsertAs_CleanTransformation()
    {
        var state = State<MathInput>.Create(new Dictionary<string, object>
        {
            ["numbers"] = new List<int> { 1, 2, 3 }
        });

        // Type evolution without casting
        var evolved = state.InsertAs<MathOutput>("sum", 6);

        Assert.IsType<State<MathOutput>>(evolved);
        Assert.Equal(new List<int> { 1, 2, 3 }, evolved.GetAny("numbers"));
        Assert.Equal(6, evolved.GetAny("sum"));
    }

    [Fact]
    public async Task MixedUsage_TypedAndUntypedTogether()
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
        var typedState = State<MathInput>.Create(new Dictionary<string, object>
        {
            ["numbers"] = untypedResult.Get("parsed")
        });

        // Continue with typed processing
        var typedChain = new Chain<MathInput, MathOutput>()
            .AddLink("sum", new TypedSumLink());

        var finalResult = await typedChain.RunAsync(typedState);

        Assert.Equal(6, finalResult.GetAny("sum"));
    }
}

/// <summary>
/// Test data classes.
/// </summary>
public class TestData { }
public class ProcessingData { }
public class OutputData { }
public class MathInput { }
public class MathOutput { }

/// <summary>
/// Test implementations.
/// </summary>
public class TestGenericLink : IStateLink<TestData, ProcessingData>
{
    public async Task<State<ProcessingData>> CallAsync(State<TestData> state)
    {
        var input = state.GetAny("input")?.ToString() ?? "";
        var output = input + "_processed";

        return State<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["output"] = output
        });
    }
}

public class Step1Link : IStateLink<TestData, ProcessingData>
{
    public async Task<State<ProcessingData>> CallAsync(State<TestData> state)
    {
        var value = state.GetAny("value") as int? ?? 0;
        return State<ProcessingData>.Create(new Dictionary<string, object>
        {
            ["step1"] = value * 2
        });
    }
}

public class Step2Link : IStateLink<ProcessingData, OutputData>
{
    public async Task<State<OutputData>> CallAsync(State<ProcessingData> state)
    {
        var step1 = state.GetAny("step1") as int? ?? 0;
        return State<OutputData>.Create(new Dictionary<string, object>
        {
            ["final"] = step1 + 5
        });
    }
}

public class UntypedMathLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var a = state.Get<int>("a");
        var b = state.Get<int>("b");
        return ValueTask.FromResult(state.Insert("sum", a + b));
    }
}

public class TypedMathLink : IStateLink<MathInput, MathOutput>
{
    public async Task<State<MathOutput>> CallAsync(State<MathInput> state)
    {
        var a = state.GetAny("a") as int? ?? 0;
        var b = state.GetAny("b") as int? ?? 0;
        return State<MathOutput>.Create(new Dictionary<string, object>
        {
            ["sum"] = a + b
        });
    }
}

public class UntypedParseLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var data = state.Get<string>("data");
        var parsed = data?.Split(',').Select(int.Parse).ToList();
        return ValueTask.FromResult(state.Insert("parsed", parsed));
    }
}

public class TypedSumLink : IStateLink<MathInput, MathOutput>
{
    public async Task<State<MathOutput>> CallAsync(State<MathInput> state)
    {
        var numbers = state.GetAny("numbers") as List<int> ?? new List<int>();
        var sum = numbers.Sum();
        return State<MathOutput>.Create(new Dictionary<string, object>
        {
            ["sum"] = sum
        });
    }
}