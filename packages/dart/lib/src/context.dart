/// Context: The Data Container
///
/// The Context holds data carefully, immutable by default for safety, mutable for flexibility.
/// Optimized for Dart's type system—embracing Map with String keys and dynamic values 
/// interface with null safety. Enhanced with generic typing for type-safe workflows.
library;

/// Immutable context with generic typing support
/// 
/// Holds data without judgment, returns fresh copies for changes.
/// Enhanced with generic typing for type-safe workflows.
class Context<T> {
  final Map<String, dynamic> _data;

  /// Creates a new context with optional initial data
  /// 
  /// [data] - Initial data to store in the context
  Context([Map<String, dynamic>? data]) : _data = Map.from(data ?? {});

  /// Private constructor for creating new instances with specific data
  Context._internal(this._data);

  /// Returns the value for the given key, forgiving absence with null
  /// 
  /// [key] - The key to look up
  /// Returns the value or null if not found
  dynamic get(String key) => _data[key];

  /// Returns a fresh context with the addition, maintaining immutability
  /// 
  /// [key] - The key to insert
  /// [value] - The value to associate with the key
  /// Returns a new Context with the same type parameter
  Context<T> insert(String key, dynamic value) {
    final newData = Map<String, dynamic>.from(_data);
    newData[key] = value;
    return Context<T>._internal(newData);
  }

  /// Returns a fresh context with type evolution, allowing clean transformations
  /// 
  /// This method enables clean transformation between related types without explicit casting.
  /// Following the universal CodeUChain pattern for type evolution.
  /// 
  /// [key] - The key to insert
  /// [value] - The value to associate with the key
  /// Returns a new Context with evolved type parameter
  Context<U> insertAs<U>(String key, dynamic value) {
    final newData = Map<String, dynamic>.from(_data);
    newData[key] = value;
    return Context<U>._internal(newData);
  }

  /// Combines contexts, favoring the other with compassion
  /// 
  /// [other] - The context to merge with this one
  /// Returns a new Context containing data from both contexts
  Context<T> merge(Context<T> other) {
    final newData = Map<String, dynamic>.from(_data);
    newData.addAll(other._data);
    return Context<T>._internal(newData);
  }

  /// Returns all keys in the context
  Iterable<String> get keys => _data.keys;

  /// Returns all values in the context
  Iterable<dynamic> get values => _data.values;

  /// Returns the number of key-value pairs in the context
  int get length => _data.length;

  /// Checks if the context is empty
  bool get isEmpty => _data.isEmpty;

  /// Checks if the context is not empty
  bool get isNotEmpty => _data.isNotEmpty;

  /// Checks if the context contains the given key
  bool containsKey(String key) => _data.containsKey(key);

  /// Returns a copy of the internal data map
  /// 
  /// Useful for debugging or integration with other systems
  Map<String, dynamic> toMap() => Map<String, dynamic>.from(_data);

  /// Creates a mutable version of this context for performance-critical operations
  /// 
  /// Returns a [MutableContext] that allows in-place modifications
  MutableContext<T> toMutable() => MutableContext<T>._internal(Map.from(_data));

  @override
  String toString() => 'Context<$T>($_data)';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Context<T> && _mapEquals(_data, other._data);

  @override
  int get hashCode => _data.hashCode;

  /// Deep equality check for maps
  static bool _mapEquals(Map<String, dynamic> a, Map<String, dynamic> b) {
    if (a.length != b.length) return false;
    for (final key in a.keys) {
      if (!b.containsKey(key) || a[key] != b[key]) return false;
    }
    return true;
  }
}

/// Mutable context for performance-critical operations
/// 
/// Provides in-place modifications when immutability is not required.
/// Can be converted back to immutable Context when needed.
class MutableContext<T> {
  final Map<String, dynamic> _data;

  /// Creates a new mutable context with optional initial data
  MutableContext([Map<String, dynamic>? data]) : _data = data ?? <String, dynamic>{};

  /// Private constructor for internal use
  MutableContext._internal(this._data);

  /// Returns the value for the given key
  dynamic get(String key) => _data[key];

  /// Sets a value directly (mutable operation)
  void set(String key, dynamic value) => _data[key] = value;

  /// Removes a key-value pair
  dynamic remove(String key) => _data.remove(key);

  /// Clears all data
  void clear() => _data.clear();

  /// Returns all keys in the context
  Iterable<String> get keys => _data.keys;

  /// Returns all values in the context
  Iterable<dynamic> get values => _data.values;

  /// Returns the number of key-value pairs in the context
  int get length => _data.length;

  /// Checks if the context is empty
  bool get isEmpty => _data.isEmpty;

  /// Checks if the context is not empty
  bool get isNotEmpty => _data.isNotEmpty;

  /// Checks if the context contains the given key
  bool containsKey(String key) => _data.containsKey(key);

  /// Returns a copy of the internal data map
  Map<String, dynamic> toMap() => Map<String, dynamic>.from(_data);

  /// Creates an immutable version of this context
  Context<T> toImmutable() => Context<T>._internal(Map.from(_data));

  @override
  String toString() => 'MutableContext<$T>($_data)';
}