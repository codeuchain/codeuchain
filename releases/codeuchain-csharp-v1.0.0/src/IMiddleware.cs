/// <summary>
/// Middleware: The Chain Enhancement Interface
/// Provides hooks for intercepting and modifying chain execution.
/// Unified middleware that handles both sync and async operations.
/// </summary>
public interface IMiddleware
{
    /// <summary>
    /// Called before a link is executed.
    /// </summary>
    ValueTask<Context> BeforeAsync(ILink? link, Context context);

    /// <summary>
    /// Called after a link is executed successfully.
    /// </summary>
    ValueTask<Context> AfterAsync(ILink? link, Context context);

    /// <summary>
    /// Called when a link throws an exception.
    /// </summary>
    ValueTask<Context> OnErrorAsync(ILink? link, Exception exception, Context context);
}

/// <summary>
/// Generic Middleware interface.
/// Simplified for type-evolving chains - middleware operates on the current context type.
/// </summary>
public interface IMiddleware<T>
    where T : class
{
    Task<Context<T>> BeforeAsync(IContextLink<T, T>? link, Context<T> context);
    Task<Context<T>> AfterAsync(IContextLink<T, T>? link, Context<T> context);
    Task<Context<T>> OnErrorAsync(IContextLink<T, T>? link, Exception exception, Context<T> context);
}