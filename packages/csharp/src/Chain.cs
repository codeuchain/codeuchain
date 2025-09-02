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
                foreach (var middleware in _middlewares)
                {
                    try
                    {
                        currentContext = await middleware.OnErrorAsync(link, ex, currentContext);
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