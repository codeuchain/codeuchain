using System.Collections.Immutable;

/// <summary>
/// Chain: The Harmonious Connector
/// Unified implementation that handles both sync and async operations seamlessly.
/// </summary>
public class Chain
{
    private readonly ImmutableList<KeyValuePair<string, ILink>> _links;
    private readonly ImmutableList<IMiddleware> _middlewares;

    private Chain(ImmutableList<KeyValuePair<string, ILink>> links, ImmutableList<IMiddleware> middlewares)
    {
        _links = links;
        _middlewares = middlewares;
    }

    public Chain()
    {
        _links = ImmutableList<KeyValuePair<string, ILink>>.Empty;
        _middlewares = ImmutableList<IMiddleware>.Empty;
    }

    /// <summary>
    /// Adds a link to the chain.
    /// </summary>
    public Chain AddLink(string name, ILink link)
    {
        return new Chain(_links.Add(new KeyValuePair<string, ILink>(name, link)), _middlewares);
    }

    /// <summary>
    /// Adds middleware to the chain.
    /// </summary>
    public Chain UseMiddleware(IMiddleware middleware)
    {
        return new Chain(_links, _middlewares.Add(middleware));
    }

    /// <summary>
    /// Executes the chain. Automatically handles sync/async based on the links.
    /// </summary>
    public async ValueTask<Context> RunAsync(Context initialContext)
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
                    currentContext = await middleware.BeforeAsync(link, currentContext);
                }
                catch (Exception ex)
                {
                    // Handle middleware errors
                    foreach (var errorMiddleware in _middlewares)
                    {
                        try
                        {
                            currentContext = await errorMiddleware.OnErrorAsync(link, ex, currentContext);
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
                currentContext = await link.ProcessAsync(currentContext);
            }
            catch (Exception ex)
            {
                // Handle link errors
                bool errorHandled = false;
                foreach (var middleware in _middlewares)
                {
                    try
                    {
                        currentContext = await middleware.OnErrorAsync(link, ex, currentContext);
                        errorHandled = true; // Assume middleware handled the error
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }

                // Only rethrow if no middleware handled the error
                if (!errorHandled)
                    throw;
            }

            // After each link
            foreach (var middleware in _middlewares)
            {
                try
                {
                    currentContext = await middleware.AfterAsync(link, currentContext);
                }
                catch (Exception ex)
                {
                    // Handle middleware errors
                    foreach (var errorMiddleware in _middlewares)
                    {
                        try
                        {
                            currentContext = await errorMiddleware.OnErrorAsync(link, ex, currentContext);
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

    /// <summary>
    /// Synchronous execution - blocks if any async operations are present.
    /// </summary>
    public Context RunSync(Context initialContext)
    {
        return RunAsync(initialContext).GetAwaiter().GetResult();
    }
}

/// <summary>
/// Generic Chain with type safety.
/// Supports the universal Link[Input, Output] pattern for clean type evolution.
/// Note: Middleware is simplified to work with single types for now.
/// </summary>
public class Chain<TInput, TOutput>
    where TInput : class
    where TOutput : class
{
    private readonly ImmutableList<KeyValuePair<string, IContextLink<TInput, TOutput>>> _links;

    private Chain(ImmutableList<KeyValuePair<string, IContextLink<TInput, TOutput>>> links)
    {
        _links = links;
    }

    public Chain()
    {
        _links = ImmutableList<KeyValuePair<string, IContextLink<TInput, TOutput>>>.Empty;
    }

    /// <summary>
    /// Adds a link to the chain.
    /// </summary>
    public Chain<TInput, TOutput> AddLink(string name, IContextLink<TInput, TOutput> link)
    {
        return new Chain<TInput, TOutput>(_links.Add(new KeyValuePair<string, IContextLink<TInput, TOutput>>(name, link)));
    }

    /// <summary>
    /// Executes the chain with the given context.
    /// </summary>
    public async Task<Context<TOutput>> RunAsync(Context<TInput> initialContext)
    {
        // For a chain with type evolution, we need to handle the type transformation properly
        // This is a simplified implementation - in practice, you'd want a more sophisticated approach

        Context<TInput> currentInputContext = initialContext;
        Context<TOutput> currentOutputContext = default!;

        // Execute links with type evolution
        foreach (var (name, link) in _links)
        {
            try
            {
                currentOutputContext = await link.CallAsync(currentInputContext);
                // For subsequent links, we need to adapt the context type
                // This is a limitation of the current simplified implementation
                currentInputContext = currentOutputContext.InsertAs<TInput>("__temp", new object()).Remove("__temp");
            }
            catch (Exception)
            {
                // For now, rethrow exceptions - middleware can be added later
                throw;
            }
        }

        return currentOutputContext;
    }
}