/// <summary>
/// Synchronous version of the Link interface for performance comparison.
/// </summary>
public interface ISyncLink
{
    Context Call(Context context);
}

/// <summary>
/// Synchronous version of the Middleware interface.
/// </summary>
public interface ISyncMiddleware
{
    Context Before(ISyncLink? link, Context context);
    Context After(ISyncLink? link, Context context);
    Context OnError(ISyncLink? link, Exception exception, Context context);
}

/// <summary>
/// Synchronous version of the Chain for performance comparison.
/// </summary>
public class SyncChain
{
    private readonly List<KeyValuePair<string, ISyncLink>> _links;
    private readonly List<ISyncMiddleware> _middlewares;

    public SyncChain()
    {
        _links = new List<KeyValuePair<string, ISyncLink>>();
        _middlewares = new List<ISyncMiddleware>();
    }

    public SyncChain AddLink(string name, ISyncLink link)
    {
        _links.Add(new KeyValuePair<string, ISyncLink>(name, link));
        return this;
    }

    public SyncChain UseMiddleware(ISyncMiddleware middleware)
    {
        _middlewares.Add(middleware);
        return this;
    }

    public Context Run(Context initialContext)
    {
        var currentContext = initialContext;

        // Execute before hooks
        foreach (var middleware in _middlewares)
        {
            currentContext = middleware.Before(null, currentContext);
        }

        // Execute links
        foreach (var (name, link) in _links)
        {
            // Before each link
            foreach (var middleware in _middlewares)
            {
                currentContext = middleware.Before(link, currentContext);
            }

            // Execute link
            currentContext = link.Call(currentContext);

            // After each link
            foreach (var middleware in _middlewares)
            {
                currentContext = middleware.After(link, currentContext);
            }
        }

        // Final after hooks
        foreach (var middleware in _middlewares)
        {
            currentContext = middleware.After(null, currentContext);
        }

        return currentContext;
    }
}

/// <summary>
/// Synchronous versions of the example links.
/// </summary>
public class SyncAddLink : ISyncLink
{
    public Context Call(Context context)
    {
        var a = context.Get<int>("a");
        var b = context.Get<int>("b");
        return context.Insert("sum", a + b);
    }
}

public class SyncMultiplyLink : ISyncLink
{
    public Context Call(Context context)
    {
        var sum = context.Get<int>("sum");
        return context.Insert("result", sum * 2);
    }
}

public class SyncLoggingMiddleware : ISyncMiddleware
{
    public Context Before(ISyncLink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return context;
    }

    public Context After(ISyncLink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return context;
    }

    public Context OnError(ISyncLink? link, Exception exception, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return context;
    }
}