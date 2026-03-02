using System.Threading.Tasks;

/// <summary>
/// Legacy Processor: Implements old ILink interface
/// </summary>
public class LegacyProcessor : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var input = state.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(state.Insert("output", input.ToUpper()));
    }
}

/// <summary>
/// Modern Processor: Implements old ILink interface with chaining
/// </summary>
public class ModernProcessor : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var input = state.Get("input")?.ToString() ?? "";
        var output = state.Get("output")?.ToString() ?? "";
        return ValueTask.FromResult(state.Insert("output", input.ToUpper()).Insert("final", $"{output}-MODERN"));
    }
}