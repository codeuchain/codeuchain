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
/// Generic Link: Opt-in Type Safety
/// Strongly-typed version of ILink for static type checking while maintaining runtime flexibility.
/// Follows the universal Link[Input, Output] pattern across all CodeUChain languages.
/// </summary>
public interface ILink<TInput, TOutput>
{
    /// <summary>
    /// Processes the context with type safety.
    /// Provides clean type evolution without explicit casting.
    /// </summary>
    ValueTask<Context<TOutput>> ProcessAsync(Context<TInput> context);
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

/// <summary>
/// Generic Link interface for context-based processing.
/// Follows the universal Link[Input, Output] pattern across all CodeUChain languages.
/// </summary>
public interface IContextLink<TInput, TOutput>
{
    Task<Context<TOutput>> CallAsync(Context<TInput> context);
}