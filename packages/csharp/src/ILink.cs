/// <summary>
/// Link: The Processing Unit Interface
/// Unified interface that handles both sync and async operations automatically.
/// </summary>
public interface ILink
{
    /// <summary>
    /// Processes the context and returns a new context.
    /// Can be implemented as sync or async - the chain handles both automatically.
    /// </summary>
    /// <param name="context">The input context</param>
    /// <returns>The processed context</returns>
    ValueTask<Context> ProcessAsync(Context context);
}

/// <summary>
/// Extension methods to make implementing links easier.
/// </summary>
public static class LinkExtensions
{
    /// <summary>
    /// Synchronous link implementation helper.
    /// </summary>
    public static ValueTask<Context> ProcessAsync(this Func<Context, Context> processor, Context context)
    {
        return ValueTask.FromResult(processor(context));
    }

    /// <summary>
    /// Asynchronous link implementation helper.
    /// </summary>
    public static ValueTask<Context> ProcessAsync(this Func<Context, Task<Context>> processor, Context context)
    {
        return new ValueTask<Context>(processor(context));
    }
}