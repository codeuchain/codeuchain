using System.Threading.Tasks;

/// <summary>
/// Simple Link: Basic processing link
/// </summary>
public class SimpleLink : ILink
{
    public ValueTask<State> ProcessAsync(State state)
    {
        var input = state.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(state.Insert("processed", input.ToUpper()));
    }
}

/// <summary>
/// Async Delay Link: Simulates async work
/// </summary>
public class AsyncDelayLink : ILink
{
    public async ValueTask<State> ProcessAsync(State state)
    {
        var delay = (int?)state.Get("delay") ?? 100;
        await Task.Delay(delay);
        return state.Insert("delayed", true).Insert("completed", true);
    }
}