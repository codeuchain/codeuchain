using System.Threading.Tasks;

/// <summary>
/// Error Link: Throws errors for testing
/// </summary>
public class ErrorLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        if (context.GetAny("trigger")?.ToString() == "error")
            throw new InvalidOperationException("Test error");
        return context;
    }
}

/// <summary>
/// Safe Link: Implements old ILink interface safely
/// </summary>
public class SafeLink : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        if (context.Get("trigger")?.ToString() == "error")
            throw new InvalidOperationException("Test error");
        return ValueTask.FromResult(context.Insert("safe", "processed"));
    }
}

/// <summary>
/// Error Handling Middleware: Handles errors gracefully
/// </summary>
public class ErrorHandlingMiddleware : IMiddleware
{
    public ValueTask<Context> BeforeAsync(ILink? link, Context context) => ValueTask.FromResult(context);

    public ValueTask<Context> AfterAsync(ILink? link, Context context) => ValueTask.FromResult(context);

    public ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        return ValueTask.FromResult(context.Insert("handled", true).Insert("error", exception.Message));
    }
}