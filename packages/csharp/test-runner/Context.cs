using System.Collections.Immutable;

/// <summary>
/// Context: The Immutable Data Carrier
/// Carries data through the processing chain in an immutable manner.
/// </summary>
public class Context
{
    private readonly ImmutableDictionary<string, object> _data;

    private Context(ImmutableDictionary<string, object> data)
    {
        _data = data;
    }

    /// <summary>
    /// Creates a new empty context.
    /// </summary>
    public static Context Create()
    {
        return new Context(ImmutableDictionary<string, object>.Empty);
    }

    /// <summary>
    /// Creates a new context with initial data.
    /// </summary>
    public static Context Create(IDictionary<string, object> data)
    {
        return new Context(data.ToImmutableDictionary());
    }

    /// <summary>
    /// Retrieves a value from the context.
    /// </summary>
    public object? Get(string key)
    {
        return _data.TryGetValue(key, out var value) ? value : null;
    }

    /// <summary>
    /// Retrieves a typed value from the context.
    /// </summary>
    public T? Get<T>(string key)
    {
        return _data.TryGetValue(key, out var value) && value is T typedValue ? typedValue : default;
    }

    /// <summary>
    /// Checks if the context contains a key.
    /// </summary>
    public bool ContainsKey(string key)
    {
        return _data.ContainsKey(key);
    }

    /// <summary>
    /// Returns a new context with the specified key-value pair inserted.
    /// </summary>
    public Context Insert(string key, object value)
    {
        return new Context(_data.SetItem(key, value));
    }

    /// <summary>
    /// Type Evolution: Insert with type transformation
    /// Returns a new context with the specified key-value pair inserted, enabling clean type evolution.
    /// This method allows transforming the context's type without explicit casting.
    /// </summary>
    public Context InsertAs(string key, object value)
    {
        return new Context(_data.SetItem(key, value));
    }

    /// <summary>
    /// Returns a new context with the specified key removed.
    /// </summary>
    public Context Remove(string key)
    {
        return new Context(_data.Remove(key));
    }

    /// <summary>
    /// Returns all keys in the context.
    /// </summary>
    public IEnumerable<string> Keys => _data.Keys;

    /// <summary>
    /// Returns all values in the context.
    /// </summary>
    public IEnumerable<object> Values => _data.Values;

    /// <summary>
    /// Returns the number of items in the context.
    /// </summary>
    public int Count => _data.Count;

    /// <summary>
    /// Returns a string representation of the context.
    /// </summary>
    public override string ToString()
    {
        return $"Context({string.Join(", ", _data.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    }
}

/// <summary>
/// Generic Context: Opt-in Type Safety
/// Strongly-typed version of Context for static type checking while maintaining runtime flexibility.
/// Supports clean type evolution through InsertAs<U>() method.
/// Follows the universal pattern across all CodeUChain languages.
/// </summary>
public class Context<T>
{
    private readonly ImmutableDictionary<string, object> _data;

    private Context(ImmutableDictionary<string, object> data)
    {
        _data = data;
    }

    /// <summary>
    /// Creates a new empty generic context.
    /// </summary>
    public static Context<T> Create()
    {
        return new Context<T>(ImmutableDictionary<string, object>.Empty);
    }

    /// <summary>
    /// Creates a new generic context with initial data.
    /// </summary>
    public static Context<T> Create(IDictionary<string, object> data)
    {
        return new Context<T>(data.ToImmutableDictionary());
    }

    /// <summary>
    /// Retrieves a typed value from the context.
    /// </summary>
    public T? Get(string key)
    {
        return _data.TryGetValue(key, out var value) && value is T typedValue ? typedValue : default;
    }

    /// <summary>
    /// Retrieves a value of any type from the context.
    /// </summary>
    public object? GetAny(string key)
    {
        return _data.TryGetValue(key, out var value) ? value : null;
    }

    /// <summary>
    /// Checks if the context contains a key.
    /// </summary>
    public bool ContainsKey(string key)
    {
        return _data.ContainsKey(key);
    }

    /// <summary>
    /// Type Preservation: Insert that maintains current type T
    /// Returns a new context with the specified key-value pair inserted.
    /// </summary>
    public Context<T> Insert(string key, object value)
    {
        return new Context<T>(_data.SetItem(key, value));
    }

    /// <summary>
    /// Type Evolution: Insert with type transformation
    /// Returns a new context with the specified key-value pair inserted, enabling clean type evolution.
    /// This method allows transforming the context's type to U without explicit casting.
    /// </summary>
    public Context<U> InsertAs<U>(string key, object value)
    {
        return new Context<U>(_data.SetItem(key, value));
    }

    /// <summary>
    /// Returns a new context with the specified key removed.
    /// </summary>
    public Context<T> Remove(string key)
    {
        return new Context<T>(_data.Remove(key));
    }

    /// <summary>
    /// Returns all keys in the context.
    /// </summary>
    public IEnumerable<string> Keys => _data.Keys;

    /// <summary>
    /// Returns all values in the context.
    /// </summary>
    public IEnumerable<object> Values => _data.Values;

    /// <summary>
    /// Returns the number of items in the context.
    /// </summary>
    public int Count => _data.Count;

    /// <summary>
    /// Returns a string representation of the generic context.
    /// </summary>
    public override string ToString()
    {
        return $"Context<{typeof(T).Name}>({string.Join(", ", _data.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    }
}