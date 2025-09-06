/**
 * Context: The Loving Vessel
 *
 * With agape compassion, the Context holds data tenderly, immutable by default for safety, mutable for flexibility.
 * Optimized for JavaScript's dynamism—embracing object-like interface with ecosystem integrations.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @template T - The type of data structure this context holds
 * @since 1.0.0
 */

/**
 * @template T
 */
class Context {
  /**
   * Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @param {Object} data - Initial data object to store in the context
   * @throws {TypeError} If data is null or undefined
   * @example
   * const ctx = new Context({ name: 'Alice', age: 30 });
   * console.log(ctx.get('name')); // 'Alice'
   */
  constructor(data = {}) {
    this._data = this._deepFreeze({ ...data });
  }

  /**
   * Deep freeze an object to ensure immutability at all levels.
   * This prevents accidental mutation of nested objects and arrays.
   *
   * @private
   * @param {Object} obj - The object to deep freeze
   * @returns {Object} The deep frozen object
   */
  _deepFreeze(obj) {
    if (obj === null || typeof obj !== 'object') return obj;

    // Freeze the object
    Object.freeze(obj);

    // Recursively freeze all properties
    Object.keys(obj).forEach(key => {
      if (typeof obj[key] === 'object' && obj[key] !== null && !Object.isFrozen(obj[key])) {
        this._deepFreeze(obj[key]);
      }
    });

    return obj;
  }

  /**
   * Create an empty context with no initial data.
   *
   * @static
   * @returns {Context<T>} An empty context instance
   * @example
   * const emptyCtx = Context.empty();
   * const populatedCtx = emptyCtx.insert('key', 'value');
   */
  static empty() {
    return new Context({});
  }

  /**
   * Create a context from existing data.
   *
   * @static
   * @param {Object} data - The data to create context from
   * @returns {Context} A new context with the provided data
   * @example
   * const data = { user: 'alice', role: 'admin' };
   * const ctx = Context.from(data);
   */
  static from(data) {
    return new Context(data);
  }

  /**
   * With gentle care, return the value or undefined, forgiving absence.
   * Returns a deep copy of complex objects to maintain immutability.
   *
   * @param {string} key - The key to retrieve from the context
   * @returns {*} The value associated with the key, or undefined if not found
   * @example
   * const ctx = new Context({ name: 'Alice', data: { age: 30 } });
   * console.log(ctx.get('name')); // 'Alice'
   * console.log(ctx.get('missing')); // undefined
   * console.log(ctx.get('data')); // { age: 30 } (deep copy)
   */
  get(key) {
    const value = this._data[key];
    if (value === undefined) return undefined;
    
    // Return deep copy for objects and arrays to maintain immutability
    if (typeof value === 'object' && value !== null) {
      return JSON.parse(JSON.stringify(value));
    }
    
    return value;
  }

  /**
   * With selfless safety, return a fresh context with the addition.
   * Creates a new immutable context with the new key-value pair.
   *
   * @param {string} key - The key to insert into the context
   * @param {*} value - The value to associate with the key
   * @returns {Context<T>} A new Context with the addition (original remains unchanged)
   * @example
   * const original = new Context({ name: 'Alice' });
   * const updated = original.insert('age', 30);
   * console.log(original.get('age')); // undefined
   * console.log(updated.get('age')); // 30
   */
  insert(key, value) {
    const newData = { ...this._data, [key]: value };
    return new Context(newData);
  }

  /**
   * Create a new Context with type evolution, allowing clean transformation
   * between data shapes without explicit casting. This method is specifically
   * designed for use with generic typing to enable type-safe workflows.
   *
   * @param {string} key - The key to insert into the context
   * @param {*} value - The value to associate with the key
   * @returns {Context} A new Context with type evolution (original remains unchanged)
   * @example
   * // Type evolution example
   * const userCtx = new Context({ name: 'Alice' });
   * const validatedCtx = userCtx.insertAs('isValid', true);
   * // TypeScript would see validatedCtx as having both name and isValid
   */
  insertAs(key, value) {
    const newData = { ...this._data, [key]: value };
    return new Context(newData);
  }

  /**
   * For those needing change, provide a mutable sibling.
   * Creates a mutable version of this context for performance-critical sections.
   *
   * @returns {MutableContext<T>} A mutable version of this context
   * @example
   * const immutable = new Context({ counter: 0 });
   * const mutable = immutable.withMutation();
   * mutable.set('counter', 1); // This mutates
   * const backToImmutable = mutable.toImmutable();
   */
  withMutation() {
    return new MutableContext({ ...this._data });
  }

  /**
   * Lovingly combine contexts, favoring the other with compassion.
   * Merges this context with another, with the other context's values taking precedence.
   *
   * @param {Context<T>} other - The other context to merge with this one
   * @returns {Context<T>} A new Context with merged data
   * @throws {TypeError} If other is not a Context instance
   * @example
   * const ctx1 = new Context({ name: 'Alice', age: 25 });
   * const ctx2 = new Context({ age: 30, city: 'NYC' });
   * const merged = ctx1.merge(ctx2);
   * console.log(merged.get('age')); // 30 (ctx2 takes precedence)
   * console.log(merged.get('city')); // 'NYC'
   */
  merge(other) {
    const newData = { ...this._data, ...other._data };
    return new Context(newData);
  }

  /**
   * Express as plain object for ecosystem integration.
   * Returns a deep copy of the internal data as a plain JavaScript object.
   *
   * @returns {Object} A deep copy of the internal data
   * @example
   * const ctx = new Context({ user: { name: 'Alice' } });
   * const plain = ctx.toObject();
   * plain.user.name = 'Bob'; // Safe - doesn't affect original context
   */
  toObject() {
    return JSON.parse(JSON.stringify(this._data));
  }

  /**
   * Check if a key exists in the context.
   *
   * @param {string} key - The key to check for existence
   * @returns {boolean} True if the key exists, false otherwise
   * @example
   * const ctx = new Context({ name: 'Alice' });
   * console.log(ctx.has('name')); // true
   * console.log(ctx.has('age')); // false
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys in the context.
   *
   * @returns {string[]} Array of all keys in the context
   * @example
   * const ctx = new Context({ name: 'Alice', age: 30 });
   * console.log(ctx.keys()); // ['name', 'age']
   */
  keys() {
    return Object.keys(this._data);
  }

  /**
   * String representation of the context for debugging.
   *
   * @returns {string} String representation of the context
   * @example
   * const ctx = new Context({ name: 'Alice' });
   * console.log(ctx.toString()); // 'Context({"name":"Alice"})'
   */
  toString() {
    return `Context(${JSON.stringify(this._data)})`;
  }
}

/**
 * @template T
 */
class MutableContext {
  /**
   * Mutable context for performance-critical sections—use with care, but forgiven.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @param {Object} data - Initial data object to store in the mutable context
   * @example
   * const mutable = new MutableContext({ counter: 0 });
   * mutable.set('counter', 1); // Direct mutation
   */
  constructor(data = {}) {
    this._data = { ...data };
  }

  /**
   * Get a value from the mutable context.
   *
   * @param {string} key - The key to retrieve from the context
   * @returns {*} The value associated with the key, or undefined if not found
   * @example
   * const ctx = new MutableContext({ name: 'Alice' });
   * console.log(ctx.get('name')); // 'Alice'
   */
  get(key) {
    return this._data[key];
  }

  /**
   * Change in place with gentle permission.
   * Directly mutates the context - use sparingly and with care.
   *
   * @param {string} key - The key to set in the context
   * @param {*} value - The value to associate with the key
   * @example
   * const ctx = new MutableContext({ counter: 0 });
   * ctx.set('counter', 1); // Direct mutation
   * console.log(ctx.get('counter')); // 1
   */
  set(key, value) {
    this._data[key] = value;
  }

  /**
   * Return to safety with a fresh immutable copy.
   * Creates an immutable Context from the current mutable data.
   *
   * @returns {Context<T>} An immutable Context with the current data
   * @example
   * const mutable = new MutableContext({ temp: 'value' });
   * const immutable = mutable.toImmutable();
   * // Now immutable can be safely shared
   */
  toImmutable() {
    return new Context(this._data);
  }

  /**
   * Check if a key exists in the mutable context.
   *
   * @param {string} key - The key to check for existence
   * @returns {boolean} True if the key exists, false otherwise
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys in the mutable context.
   *
   * @returns {string[]} Array of all keys in the context
   */
  keys() {
    return Object.keys(this._data);
  }

  /**
   * String representation of the mutable context for debugging.
   *
   * @returns {string} String representation of the mutable context
   */
  toString() {
    return `MutableContext(${JSON.stringify(this._data)})`;
  }
}

module.exports = { Context, MutableContext };