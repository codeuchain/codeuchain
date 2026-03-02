using System.Collections.Immutable;

/// <summary>
/// State: The Immutable Data Carrier
/// Carries data through the processing chain in an immutable manner.
/// </summary>
public class State
{
    private readonly ImmutableDictionary<string, object> _data;

    private State(ImmutableDictionary<string, object> data)
    {
        _data = data;
    }

    /// <summary>
    /// Creates a new empty state.
    /// </summary>
    public static State Create()
    {
        return new State(ImmutableDictionary<string, object>.Empty);
    }

    /// <summary>
    /// Creates a new state with initial data.
    /// </summary>
    public static State Create(IDictionary<string, object> data)
    {
        return new State(data.ToImmutableDictionary());
    }

    /// <summary>
    /// Retrieves a value from the state.
    /// </summary>
    public object? Get(string key)
    {
        return _data.TryGetValue(key, out var value) ? value : null;
    }

    /// <summary>
    /// Retrieves a typed value from the state.
    /// </summary>
    public T? Get<T>(string key)
    {
        return _data.TryGetValue(key, out var value) && value is T typedValue ? typedValue : default;
    }

    /// <summary>
    /// Checks if the state contains a key.
    /// </summary>
    public bool ContainsKey(string key)
    {
        return _data.ContainsKey(key);
    }

    /// <summary>
    /// Returns a new state with the specified key-value pair inserted.
    /// </summary>
    public State Insert(string key, object value)
    {
        return new State(_data.SetItem(key, value));
    }

    /// <summary>
    /// Type Evolution: Insert with type transformation
    /// Returns a new state with the specified key-value pair inserted, enabling clean type evolution.
    /// This method allows transforming the state's type without explicit casting.
    /// </summary>
    public State InsertAs(string key, object value)
    {
        return new State(_data.SetItem(key, value));
    }

    /// <summary>
    /// Returns a new state with the specified key removed.
    /// </summary>
    public State Remove(string key)
    {
        return new State(_data.Remove(key));
    }

    /// <summary>
    /// Returns all keys in the state.
    /// </summary>
    public IEnumerable<string> Keys => _data.Keys;

    /// <summary>
    /// Returns all values in the state.
    /// </summary>
    public IEnumerable<object> Values => _data.Values;

    /// <summary>
    /// Returns the number of items in the state.
    /// </summary>
    public int Count => _data.Count;

    /// <summary>
    /// Returns a string representation of the state.
    /// </summary>
    public override string ToString()
    {
        return $"State({string.Join(", ", _data.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    }
}

/// <summary>
/// Generic State: Opt-in Type Safety
/// Strongly-typed version of State for static type checking while maintaining runtime flexibility.
/// Supports clean type evolution through InsertAs<U>() method.
/// Follows the universal pattern across all CodeUChain languages.
/// </summary>
public class State<T> where T : class
{
    private readonly ImmutableDictionary<string, object> _data;

    private State(ImmutableDictionary<string, object> data)
    {
        _data = data;
    }

    /// <summary>
    /// Creates a new empty generic state.
    /// </summary>
    public static State<T> Create()
    {
        return new State<T>(ImmutableDictionary<string, object>.Empty);
    }

    /// <summary>
    /// Creates a new generic state with initial data.
    /// </summary>
    public static State<T> Create(IDictionary<string, object> data)
    {
        return new State<T>(data.ToImmutableDictionary());
    }

    /// <summary>
    /// Retrieves a typed value from the state.
    /// </summary>
    public T? Get(string key)
    {
        return _data.TryGetValue(key, out var value) && value is T typedValue ? typedValue : default;
    }

    /// <summary>
    /// Retrieves a value of any type from the state.
    /// </summary>
    public object? GetAny(string key)
    {
        return _data.TryGetValue(key, out var value) ? value : null;
    }

    /// <summary>
    /// Checks if the state contains a key.
    /// </summary>
    public bool ContainsKey(string key)
    {
        return _data.ContainsKey(key);
    }

    /// <summary>
    /// Type Preservation: Insert that maintains current type T
    /// Returns a new state with the specified key-value pair inserted.
    /// </summary>
    public State<T> Insert(string key, object value)
    {
        return new State<T>(_data.SetItem(key, value));
    }

    /// <summary>
    /// Type Evolution: Insert with type transformation
    /// Returns a new state with the specified key-value pair inserted, enabling clean type evolution.
    /// This method allows transforming the state's type to U without explicit casting.
    /// </summary>
    public State<U> InsertAs<U>(string key, object value) where U : class
    {
        return new State<U>(_data.SetItem(key, value));
    }

    /// <summary>
    /// Returns a new state with the specified key removed.
    /// </summary>
    public State<T> Remove(string key)
    {
        return new State<T>(_data.Remove(key));
    }

    /// <summary>
    /// Returns all keys in the state.
    /// </summary>
    public IEnumerable<string> Keys => _data.Keys;

    /// <summary>
    /// Returns all values in the state.
    /// </summary>
    public IEnumerable<object> Values => _data.Values;

    /// <summary>
    /// Returns the number of items in the state.
    /// </summary>
    public int Count => _data.Count;

    /// <summary>
    /// Returns a string representation of the generic state.
    /// </summary>
    public override string ToString()
    {
        return $"State<{typeof(T).Name}>({string.Join(", ", _data.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    }
}