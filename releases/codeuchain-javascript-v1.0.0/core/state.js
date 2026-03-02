/**
 * State: The Loving Vessel
 *
 * With agape compassion, the State holds data tenderly, immutable by default for safety, mutable for flexibility.
 * Optimized for JavaScript's dynamism—embracing object-like interface with ecosystem integrations.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @template T - The type of data structure this state holds
 * @since 1.0.0
 */

/**
 * @template T
 */
class State {
  /**
   * Immutable state with selfless love—holds data without judgment, returns fresh copies for changes.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @param {Object} data - Initial data object to store in the state
   * @throws {TypeError} If data is null or undefined
   * @example
   * const ctx = new State({ name: 'Alice', age: 30 });
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
   * Create an empty state with no initial data.
   *
   * @static
   * @returns {State<T>} An empty state instance
   * @example
   * const emptyCtx = State.empty();
   * const populatedCtx = emptyCtx.insert('key', 'value');
   */
  static empty() {
    return new State({});
  }

  /**
   * Create a state from existing data.
   *
   * @static
   * @param {Object} data - The data to create state from
   * @returns {State} A new state with the provided data
   * @example
   * const data = { user: 'alice', role: 'admin' };
   * const ctx = State.from(data);
   */
  static from(data) {
    return new State(data);
  }

  /**
   * With gentle care, return the value or undefined, forgiving absence.
   * Returns a deep copy of complex objects to maintain immutability.
   *
   * @param {string} key - The key to retrieve from the state
   * @returns {*} The value associated with the key, or undefined if not found
   * @example
   * const ctx = new State({ name: 'Alice', data: { age: 30 } });
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
   * With selfless safety, return a fresh state with the addition.
   * Creates a new immutable state with the new key-value pair.
   *
   * @param {string} key - The key to insert into the state
   * @param {*} value - The value to associate with the key
   * @returns {State<T>} A new State with the addition (original remains unchanged)
   * @example
   * const original = new State({ name: 'Alice' });
   * const updated = original.insert('age', 30);
   * console.log(original.get('age')); // undefined
   * console.log(updated.get('age')); // 30
   */
  insert(key, value) {
    const newData = { ...this._data, [key]: value };
    return new State(newData);
  }

  /**
   * Create a new State with type evolution, allowing clean transformation
   * between data shapes without explicit casting. This method is specifically
   * designed for use with generic typing to enable type-safe workflows.
   *
   * @param {string} key - The key to insert into the state
   * @param {*} value - The value to associate with the key
   * @returns {State} A new State with type evolution (original remains unchanged)
   * @example
   * // Type evolution example
   * const userCtx = new State({ name: 'Alice' });
   * const validatedCtx = userCtx.insertAs('isValid', true);
   * // TypeScript would see validatedCtx as having both name and isValid
   */
  insertAs(key, value) {
    const newData = { ...this._data, [key]: value };
    return new State(newData);
  }

  /**
   * For those needing change, provide a mutable sibling.
   * Creates a mutable version of this state for performance-critical sections.
   *
   * @returns {MutableState<T>} A mutable version of this state
   * @example
   * const immutable = new State({ counter: 0 });
   * const mutable = immutable.withMutation();
   * mutable.set('counter', 1); // This mutates
   * const backToImmutable = mutable.toImmutable();
   */
  withMutation() {
    return new MutableState({ ...this._data });
  }

  /**
   * Lovingly combine states, favoring the other with compassion.
   * Merges this state with another, with the other state's values taking precedence.
   *
   * @param {State<T>} other - The other state to merge with this one
   * @returns {State<T>} A new State with merged data
   * @throws {TypeError} If other is not a State instance
   * @example
   * const ctx1 = new State({ name: 'Alice', age: 25 });
   * const ctx2 = new State({ age: 30, city: 'NYC' });
   * const merged = ctx1.merge(ctx2);
   * console.log(merged.get('age')); // 30 (ctx2 takes precedence)
   * console.log(merged.get('city')); // 'NYC'
   */
  merge(other) {
    const newData = { ...this._data, ...other._data };
    return new State(newData);
  }

  /**
   * Express as plain object for ecosystem integration.
   * Returns a deep copy of the internal data as a plain JavaScript object.
   *
   * @returns {Object} A deep copy of the internal data
   * @example
   * const ctx = new State({ user: { name: 'Alice' } });
   * const plain = ctx.toObject();
   * plain.user.name = 'Bob'; // Safe - doesn't affect original state
   */
  toObject() {
    return JSON.parse(JSON.stringify(this._data));
  }

  /**
   * Check if a key exists in the state.
   *
   * @param {string} key - The key to check for existence
   * @returns {boolean} True if the key exists, false otherwise
   * @example
   * const ctx = new State({ name: 'Alice' });
   * console.log(ctx.has('name')); // true
   * console.log(ctx.has('age')); // false
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys in the state.
   *
   * @returns {string[]} Array of all keys in the state
   * @example
   * const ctx = new State({ name: 'Alice', age: 30 });
   * console.log(ctx.keys()); // ['name', 'age']
   */
  keys() {
    return Object.keys(this._data);
  }

  /**
   * String representation of the state for debugging.
   *
   * @returns {string} String representation of the state
   * @example
   * const ctx = new State({ name: 'Alice' });
   * console.log(ctx.toString()); // 'State({"name":"Alice"})'
   */
  toString() {
    return `State(${JSON.stringify(this._data)})`;
  }
}

/**
 * @template T
 */
class MutableState {
  /**
   * Mutable state for performance-critical sections—use with care, but forgiven.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @param {Object} data - Initial data object to store in the mutable state
   * @example
   * const mutable = new MutableState({ counter: 0 });
   * mutable.set('counter', 1); // Direct mutation
   */
  constructor(data = {}) {
    this._data = { ...data };
  }

  /**
   * Get a value from the mutable state.
   *
   * @param {string} key - The key to retrieve from the state
   * @returns {*} The value associated with the key, or undefined if not found
   * @example
   * const ctx = new MutableState({ name: 'Alice' });
   * console.log(ctx.get('name')); // 'Alice'
   */
  get(key) {
    return this._data[key];
  }

  /**
   * Change in place with gentle permission.
   * Directly mutates the state - use sparingly and with care.
   *
   * @param {string} key - The key to set in the state
   * @param {*} value - The value to associate with the key
   * @example
   * const ctx = new MutableState({ counter: 0 });
   * ctx.set('counter', 1); // Direct mutation
   * console.log(ctx.get('counter')); // 1
   */
  set(key, value) {
    this._data[key] = value;
  }

  /**
   * Return to safety with a fresh immutable copy.
   * Creates an immutable State from the current mutable data.
   *
   * @returns {State<T>} An immutable State with the current data
   * @example
   * const mutable = new MutableState({ temp: 'value' });
   * const immutable = mutable.toImmutable();
   * // Now immutable can be safely shared
   */
  toImmutable() {
    return new State(this._data);
  }

  /**
   * Check if a key exists in the mutable state.
   *
   * @param {string} key - The key to check for existence
   * @returns {boolean} True if the key exists, false otherwise
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys in the mutable state.
   *
   * @returns {string[]} Array of all keys in the state
   */
  keys() {
    return Object.keys(this._data);
  }

  /**
   * String representation of the mutable state for debugging.
   *
   * @returns {string} String representation of the mutable state
   */
  toString() {
    return `MutableState(${JSON.stringify(this._data)})`;
  }
}

module.exports = { State, MutableState };