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