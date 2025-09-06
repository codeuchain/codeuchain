using System.Threading.Tasks;

/// <summary>
/// Simple Link: Basic processing link
/// </summary>
public class SimpleLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var input = context.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("processed", input.ToUpper()));
    }
}

/// <summary>
/// Async Delay Link: Simulates async work
/// </summary>
public class AsyncDelayLink : ILink
{
    public async ValueTask<Context> ProcessAsync(Context context)
    {
        var delay = (int?)context.Get("delay") ?? 100;
        await Task.Delay(delay);
        return context.Insert("delayed", true).Insert("completed", true);
    }
}