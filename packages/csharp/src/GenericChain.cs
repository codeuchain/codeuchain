using System.Collections.Immutable;

/// <summary>
/// Generic Context: Strongly-typed immutable data container.
/// </summary>
public class Context<T> where T : class
{
    private readonly ImmutableDictionary<string, T> _data;

    private Context(ImmutableDictionary<string, T> data)
    {
        _data = data;
    }

    /// <summary>
    /// Creates a new empty context.
    /// </summary>
    public static Context<T> Create()
    {
        return new Context<T>(ImmutableDictionary<string, T>.Empty);
    }

    /// <summary>
    /// Creates a new context with initial data.
    /// </summary>
    public static Context<T> Create(IDictionary<string, T> data)
    {
        return new Context<T>(data.ToImmutableDictionary());
    }

    /// <summary>
    /// Retrieves a value from the context.
    /// </summary>
    public T? Get(string key)
    {
        return _data.TryGetValue(key, out var value) ? value : default;
    }

    /// <summary>
    /// Checks if the context contains a key.
    /// </summary>
    public bool ContainsKey(string key)
    {
        return _data.ContainsKey(key);
    }

    /// <summary>
    /// Returns a new context with the specified key-value pair inserted.
    /// </summary>
    public Context<T> Insert(string key, T value)
    {
        return new Context<T>(_data.SetItem(key, value));
    }

    /// <summary>
    /// Returns a new context with the specified key removed.
    /// </summary>
    public Context<T> Remove(string key)
    {
        return new Context<T>(_data.Remove(key));
    }

    /// <summary>
    /// Returns all keys in the context.
    /// </summary>
    public IEnumerable<string> Keys => _data.Keys;

    /// <summary>
    /// Returns all values in the context.
    /// </summary>
    public IEnumerable<T> Values => _data.Values;

    /// <summary>
    /// Returns the number of items in the context.
    /// </summary>
    public int Count => _data.Count;

    /// <summary>
    /// Returns a string representation of the context.
    /// </summary>
    public override string ToString()
    {
        return $"Context<{typeof(T).Name}>({string.Join(", ", _data.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    }
}

/// <summary>
/// Generic Link interface for context-based processing.
/// </summary>
public interface IContextLink<T> where T : class
{
    Task<Context<T>> CallAsync(Context<T> context);
}

/// <summary>
/// Generic Middleware interface.
/// </summary>
public interface IMiddleware<T> where T : class
{
    Task<Context<T>> BeforeAsync(IContextLink<T>? link, Context<T> context);
    Task<Context<T>> AfterAsync(IContextLink<T>? link, Context<T> context);
    Task<Context<T>> OnErrorAsync(IContextLink<T>? link, Exception exception, Context<T> context);
}

/// <summary>
/// Generic Chain with type safety.
/// </summary>
public class Chain<T> where T : class
{
    private readonly ImmutableList<KeyValuePair<string, IContextLink<T>>> _links;
    private readonly ImmutableList<IMiddleware<T>> _middlewares;

    private Chain(ImmutableList<KeyValuePair<string, IContextLink<T>>> links, ImmutableList<IMiddleware<T>> middlewares)
    {
        _links = links;
        _middlewares = middlewares;
    }

    public Chain()
    {
        _links = ImmutableList<KeyValuePair<string, IContextLink<T>>>.Empty;
        _middlewares = ImmutableList<IMiddleware<T>>.Empty;
    }

    /// <summary>
    /// Adds a link to the chain.
    /// </summary>
    public Chain<T> AddLink(string name, IContextLink<T> link)
    {
        return new Chain<T>(_links.Add(new KeyValuePair<string, IContextLink<T>>(name, link)), _middlewares);
    }

    /// <summary>
    /// Adds middleware to the chain.
    /// </summary>
    public Chain<T> UseMiddleware(IMiddleware<T> middleware)
    {
        return new Chain<T>(_links, _middlewares.Add(middleware));
    }

    /// <summary>
    /// Executes the chain with the given context.
    /// </summary>
    public async Task<Context<T>> RunAsync(Context<T> initialContext)
    {
        var currentContext = initialContext;

        // Execute before hooks
        foreach (var middleware in _middlewares)
        {
            try
            {
                currentContext = await middleware.BeforeAsync(null, currentContext);
            }
            catch (Exception ex)
            {
                // Handle middleware errors
                foreach (var errorMiddleware in _middlewares)
                {
                    try
                    {
                        currentContext = await errorMiddleware.OnErrorAsync(null, ex, currentContext);
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }
                throw;
            }
        }

        // Execute links
        foreach (var (name, link) in _links)
        {
            // Before each link
            foreach (var middleware in _middlewares)
            {
                try
                {
                    currentContext = await middleware.BeforeAsync(null, currentContext);
                }
                catch (Exception ex)
                {
                    // Handle middleware errors
                    foreach (var errorMiddleware in _middlewares)
                    {
                        try
                        {
                            currentContext = await errorMiddleware.OnErrorAsync(null, ex, currentContext);
                        }
                        catch
                        {
                            // Continue with other error handlers
                        }
                    }
                    throw;
                }
            }

            // Execute link
            try
            {
                currentContext = await link.CallAsync(currentContext);
            }
            catch (Exception ex)
            {
                // Handle link errors
                foreach (var middleware in _middlewares)
                {
                    try
                    {
                        currentContext = await middleware.OnErrorAsync(null, ex, currentContext);
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }
                throw;
            }

            // After each link
            foreach (var middleware in _middlewares)
            {
                try
                {
                    currentContext = await middleware.AfterAsync(null, currentContext);
                }
                catch (Exception ex)
                {
                    // Handle middleware errors
                    foreach (var errorMiddleware in _middlewares)
                    {
                        try
                        {
                            currentContext = await errorMiddleware.OnErrorAsync(null, ex, currentContext);
                        }
                        catch
                        {
                            // Continue with other error handlers
                        }
                    }
                    throw;
                }
            }
        }

        // Final after hooks
        foreach (var middleware in _middlewares)
        {
            try
            {
                currentContext = await middleware.AfterAsync(null, currentContext);
            }
            catch (Exception ex)
            {
                // Handle middleware errors
                foreach (var errorMiddleware in _middlewares)
                {
                    try
                    {
                        currentContext = await errorMiddleware.OnErrorAsync(null, ex, currentContext);
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }
                throw;
            }
        }

        return currentContext;
    }
}