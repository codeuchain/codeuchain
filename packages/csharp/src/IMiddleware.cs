/// <summary>
/// Hook: The Chain Enhancement Interface
/// Provides hooks for intercepting and modifying chain execution.
/// Unified hook that handles both sync and async operations.
/// </summary>
public interface IHook
{
    /// <summary>
    /// Called before a link is executed.
    /// </summary>
    ValueTask<State> BeforeAsync(ILink? link, State state);

    /// <summary>
    /// Called after a link is executed successfully.
    /// </summary>
    ValueTask<State> AfterAsync(ILink? link, State state);

    /// <summary>
    /// Called when a link throws an exception.
    /// </summary>
    ValueTask<State> OnErrorAsync(ILink? link, Exception exception, State state);
}

/// <summary>
/// Generic Hook interface.
/// Simplified for type-evolving chains - hook operates on the current state type.
/// </summary>
public interface IHook<T>
    where T : class
{
    Task<State<T>> BeforeAsync(IStateLink<T, T>? link, State<T> state);
    Task<State<T>> AfterAsync(IStateLink<T, T>? link, State<T> state);
    Task<State<T>> OnErrorAsync(IStateLink<T, T>? link, Exception exception, State<T> state);
}