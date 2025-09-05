using System.Threading.Tasks;

/// <summary>
/// Logging Middleware: Logs chain execution
/// </summary>
public class LoggingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        Console.WriteLine($"[LOG] Starting: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        Console.WriteLine($"[LOG] Completed: {link?.GetType().Name ?? "Chain"}");
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        Console.WriteLine($"[LOG] Error in {link?.GetType().Name ?? "Chain"}: {exception.Message}");
        return ValueTask.FromResult(context);
    }
}

/// <summary>
/// Timing Middleware: Measures execution time
/// </summary>
public class TimingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context)
    {
        return ValueTask.FromResult(context.Insert("start", DateTime.Now));
    }

    public ValueTask<Context> AfterAsync(ILink? link, Context context)
    {
        var start = (DateTime?)context.Get("start");
        if (start.HasValue)
        {
            var duration = DateTime.Now - start.Value;
            return ValueTask.FromResult(context.Insert("duration", duration.TotalMilliseconds));
        }
        return ValueTask.FromResult(context);
    }

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        return ValueTask.FromResult(context);
    }
}