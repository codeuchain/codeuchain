using System.Collections.Immutable;

/// <summary>
/// Chain: The Harmonious Connector
/// Unified implementation that handles both sync and async operations seamlessly.
/// </summary>
public class Chain
{
    private readonly ImmutableList<KeyValuePair<string, ILink>> _links;
    private readonly ImmutableList<IHook> _hooks;

    private Chain(ImmutableList<KeyValuePair<string, ILink>> links, ImmutableList<IHook> hooks)
    {
        _links = links;
        _hooks = hooks;
    }

    public Chain()
    {
        _links = ImmutableList<KeyValuePair<string, ILink>>.Empty;
        _hooks = ImmutableList<IHook>.Empty;
    }

    /// <summary>
    /// Adds a link to the chain.
    /// </summary>
    public Chain AddLink(string name, ILink link)
    {
        return new Chain(_links.Add(new KeyValuePair<string, ILink>(name, link)), _hooks);
    }

    /// <summary>
    /// Adds hook to the chain.
    /// </summary>
    public Chain UseHook(IHook hook)
    {
        return new Chain(_links, _hooks.Add(hook));
    }

    /// <summary>
    /// Executes the chain. Automatically handles sync/async based on the links.
    /// </summary>
    public async ValueTask<State> RunAsync(State initialState)
    {
        var currentState = initialState;

        // Execute before hooks
        foreach (var hook in _hooks)
        {
            try
            {
                currentState = await hook.BeforeAsync(null, currentState);
            }
            catch (Exception ex)
            {
                // Handle hook errors
                foreach (var errorHook in _hooks)
                {
                    try
                    {
                        currentState = await errorHook.OnErrorAsync(null, ex, currentState);
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
            foreach (var hook in _hooks)
            {
                try
                {
                    currentState = await hook.BeforeAsync(link, currentState);
                }
                catch (Exception ex)
                {
                    // Handle hook errors
                    foreach (var errorHook in _hooks)
                    {
                        try
                        {
                            currentState = await errorHook.OnErrorAsync(link, ex, currentState);
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
                currentState = await link.ProcessAsync(currentState);
            }
            catch (Exception ex)
            {
                // Handle link errors
                bool errorHandled = false;
                foreach (var hook in _hooks)
                {
                    try
                    {
                        currentState = await hook.OnErrorAsync(link, ex, currentState);
                        errorHandled = true; // Assume hook handled the error
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }

                // Only rethrow if no hook handled the error
                if (!errorHandled)
                    throw;
            }

            // After each link
            foreach (var hook in _hooks)
            {
                try
                {
                    currentState = await hook.AfterAsync(link, currentState);
                }
                catch (Exception ex)
                {
                    // Handle hook errors
                    foreach (var errorHook in _hooks)
                    {
                        try
                        {
                            currentState = await errorHook.OnErrorAsync(link, ex, currentState);
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
        foreach (var hook in _hooks)
        {
            try
            {
                currentState = await hook.AfterAsync(null, currentState);
            }
            catch (Exception ex)
            {
                // Handle hook errors
                foreach (var errorHook in _hooks)
                {
                    try
                    {
                        currentState = await errorHook.OnErrorAsync(null, ex, currentState);
                    }
                    catch
                    {
                        // Continue with other error handlers
                    }
                }
                throw;
            }
        }

        return currentState;
    }

    /// <summary>
    /// Synchronous execution - blocks if any async operations are present.
    /// </summary>
    public State RunSync(State initialState)
    {
        return RunAsync(initialState).GetAwaiter().GetResult();
    }
}

/// <summary>
/// Generic Chain with type safety.
/// Supports the universal Link[Input, Output] pattern for clean type evolution.
/// Note: Hook is simplified to work with single types for now.
/// </summary>
public class Chain<TInput, TOutput>
    where TInput : class
    where TOutput : class
{
    private readonly ImmutableList<KeyValuePair<string, IStateLink<TInput, TOutput>>> _links;

    private Chain(ImmutableList<KeyValuePair<string, IStateLink<TInput, TOutput>>> links)
    {
        _links = links;
    }

    public Chain()
    {
        _links = ImmutableList<KeyValuePair<string, IStateLink<TInput, TOutput>>>.Empty;
    }

    /// <summary>
    /// Adds a link to the chain.
    /// </summary>
    public Chain<TInput, TOutput> AddLink(string name, IStateLink<TInput, TOutput> link)
    {
        return new Chain<TInput, TOutput>(_links.Add(new KeyValuePair<string, IStateLink<TInput, TOutput>>(name, link)));
    }

    /// <summary>
    /// Executes the chain with the given state.
    /// </summary>
    public async Task<State<TOutput>> RunAsync(State<TInput> initialState)
    {
        // For a chain with type evolution, we need to handle the type transformation properly
        // This is a simplified implementation - in practice, you'd want a more sophisticated approach

        State<TInput> currentInputState = initialState;
        State<TOutput> currentOutputState = default!;

        // Execute links with type evolution
        foreach (var (name, link) in _links)
        {
            try
            {
                currentOutputState = await link.CallAsync(currentInputState);
                // For subsequent links, we need to adapt the state type
                // This is a limitation of the current simplified implementation
                currentInputState = currentOutputState.InsertAs<TInput>("__temp", new object()).Remove("__temp");
            }
            catch (Exception)
            {
                // For now, rethrow exceptions - hook can be added later
                throw;
            }
        }

        return currentOutputState;
    }
}