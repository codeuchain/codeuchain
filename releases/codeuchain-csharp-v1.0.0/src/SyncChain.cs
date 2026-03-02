/// <summary>
/// Synchronous version of the Link interface for performance comparison.
/// </summary>
public interface ISyncLink
{
    State Call(State state);
}

/// <summary>
/// Synchronous version of the Hook interface.
/// </summary>
public interface ISyncHook
{
    State Before(ISyncLink? link, State state);
    State After(ISyncLink? link, State state);
    State OnError(ISyncLink? link, Exception exception, State state);
}

/// <summary>
/// Synchronous version of the Chain for performance comparison.
/// </summary>
public class SyncChain
{
    private readonly List<KeyValuePair<string, ISyncLink>> _links;
    private readonly List<ISyncHook> _hooks;

    public SyncChain()
    {
        _links = new List<KeyValuePair<string, ISyncLink>>();
        _hooks = new List<ISyncHook>();
    }

    public SyncChain AddLink(string name, ISyncLink link)
    {
        _links.Add(new KeyValuePair<string, ISyncLink>(name, link));
        return this;
    }

    public SyncChain UseHook(ISyncHook hook)
    {
        _hooks.Add(hook);
        return this;
    }

    public State Run(State initialState)
    {
        var currentState = initialState;

        // Execute before hooks
        foreach (var hook in _hooks)
        {
            currentState = hook.Before(null, currentState);
        }

        // Execute links
        foreach (var (name, link) in _links)
        {
            // Before each link
            foreach (var hook in _hooks)
            {
                currentState = hook.Before(link, currentState);
            }

            // Execute link
            currentState = link.Call(currentState);

            // After each link
            foreach (var hook in _hooks)
            {
                currentState = hook.After(link, currentState);
            }
        }

        // Final after hooks
        foreach (var hook in _hooks)
        {
            currentState = hook.After(null, currentState);
        }

        return currentState;
    }
}

/// <summary>
/// Synchronous versions of the example links.
/// </summary>
public class SyncAddLink : ISyncLink
{
    public State Call(State state)
    {
        var a = state.Get<int>("a");
        var b = state.Get<int>("b");
        return state.Insert("sum", a + b);
    }
}

public class SyncMultiplyLink : ISyncLink
{
    public State Call(State state)
    {
        var sum = state.Get<int>("sum");
        return state.Insert("result", sum * 2);
    }
}

public class SyncLoggingHook : ISyncHook
{
    public State Before(ISyncLink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return state;
    }

    public State After(ISyncLink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return state;
    }

    public State OnError(ISyncLink? link, Exception exception, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return state;
    }
}