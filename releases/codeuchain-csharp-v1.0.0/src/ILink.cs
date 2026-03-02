/// <summary>
/// Link: The Processing Unit Interface
/// Unified interface that handles both sync and async operations automatically.
/// </summary>
public interface ILink
{
    /// <summary>
    /// Processes the state and returns a new state.
    /// Can be implemented as sync or async - the chain handles both automatically.
    /// </summary>
    /// <param name="state">The input state</param>
    /// <returns>The processed state</returns>
    ValueTask<State> ProcessAsync(State state);
}

/// <summary>
/// Generic Link: Opt-in Type Safety
/// Strongly-typed version of ILink for static type checking while maintaining runtime flexibility.
/// Follows the universal Link[Input, Output] pattern across all CodeUChain languages.
/// </summary>
public interface ILink<TInput, TOutput>
    where TInput : class
    where TOutput : class
{
    /// <summary>
    /// Processes the state with type safety.
    /// Provides clean type evolution without explicit casting.
    /// </summary>
    ValueTask<State<TOutput>> ProcessAsync(State<TInput> state);
}

/// <summary>
/// Extension methods to make implementing links easier.
/// </summary>
public static class LinkExtensions
{
    /// <summary>
    /// Synchronous link implementation helper.
    /// </summary>
    public static ValueTask<State> ProcessAsync(this Func<State, State> processor, State state)
    {
        return ValueTask.FromResult(processor(state));
    }

    /// <summary>
    /// Asynchronous link implementation helper.
    /// </summary>
    public static ValueTask<State> ProcessAsync(this Func<State, Task<State>> processor, State state)
    {
        return new ValueTask<State>(processor(state));
    }
}

/// <summary>
/// Generic Link interface for state-based processing.
/// Follows the universal Link[Input, Output] pattern across all CodeUChain languages.
/// </summary>
public interface IStateLink<TInput, TOutput>
    where TInput : class
    where TOutput : class
{
    Task<State<TOutput>> CallAsync(State<TInput> state);
}