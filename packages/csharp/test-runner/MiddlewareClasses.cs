using System.Threading.Tasks;

/// <summary>
/// Logging Hook: Logs chain execution
/// </summary>
public class LoggingHook : IHook
{
    public ValueTask<State> BeforeAsync(ILink? link, State state)
    {
        Console.WriteLine($"[LOG] Starting: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(state);
    }

    public ValueTask<State> AfterAsync(ILink? link, State state)
    {
        Console.WriteLine($"[LOG] Completed: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(state);
    }

    public ValueTask<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        Console.WriteLine($"[LOG] Error in {link?.GetType().Name ?? "Chain"}: {exception.Message}");
        return ValueTask.FromResult(state);
    }
}

/// <summary>
/// Timing Hook: Measures execution time
/// </summary>
public class TimingHook : IHook
{
    public ValueTask<State> BeforeAsync(ILink? link, State state)
    {
        return ValueTask.FromResult(state.Insert("start", DateTime.Now));
    }

    public ValueTask<State> AfterAsync(ILink? link, State state)
    {
        var start = (DateTime?)state.Get("start");
        if (start.HasValue)
        {
            var duration = DateTime.Now - start.Value;
            return ValueTask.FromResult(state.Insert("duration", duration.TotalMilliseconds));
        }
        return ValueTask.FromResult(state);
    }

    public ValueTask<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        return ValueTask.FromResult(state);
    }
}