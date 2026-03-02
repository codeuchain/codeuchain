using System.Threading.Tasks;

/// <summary>
/// Error Link: Throws errors for testing
/// </summary>
public class ErrorLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        if (state.GetAny("trigger")?.ToString() == "error")
            throw new InvalidOperationException("Test error");
        return state;
    }
}

/// <summary>
/// Safe Link: Implements old ILink interface safely
/// </summary>
public class SafeLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        if (state.Get("trigger")?.ToString() == "error")
            throw new InvalidOperationException("Test error");
        return ValueTask.FromResult(state.Insert("safe", "processed"));
    }
}

/// <summary>
/// Error Handling Hook: Handles errors gracefully
/// </summary>
public class ErrorHandlingHook : IHook
{
    public ValueTask<State> BeforeAsync(ILink? link, State state) => ValueTask.FromResult(state);

    public ValueTask<State> AfterAsync(ILink? link, State state) => ValueTask.FromResult(state);

    public ValueTask<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        return ValueTask.FromResult(state.Insert("handled", true).Insert("error", exception.Message));
    }
}