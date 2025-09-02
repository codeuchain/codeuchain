/**
 * Context: The Loving Vessel
 *
 * With agape compassion, the Context holds data tenderly, immutable by default for safety, mutable for flexibility.
 * Optimized for JavaScript's dynamism—embracing object-like interface with ecosystem integrations.
 */

class Context {
  /**
   * Immutable context with selfless love—holds data without judgment, returns fresh copies for changes.
   * @param {Object} data - Initial data object
   */
  constructor(data = {}) {
    this._data = this._deepFreeze({ ...data });
  }

  /**
   * Deep freeze an object to ensure immutability at all levels
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
   * Create an empty context
   * @returns {Context} An empty context
   */
  static empty() {
    return new Context({});
  }

  /**
   * Create a context from data
   * @param {Object} data - The data to create context from
   * @returns {Context} A new context with the data
   */
  static from(data) {
    return new Context(data);
  }

  /**
   * With gentle care, return the value or undefined, forgiving absence.
   * Returns a deep copy of complex objects to maintain immutability.
   * @param {string} key - The key to retrieve
   * @returns {*} The value or undefined
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
   * @param {string} key - The key to insert
   * @param {*} value - The value to insert
   * @returns {Context} A new Context with the addition
   */
  insert(key, value) {
    const newData = { ...this._data, [key]: value };
    return new Context(newData);
  }

  /**
   * For those needing change, provide a mutable sibling.
   * @returns {MutableContext} A mutable version of this context
   */
  withMutation() {
    return new MutableContext({ ...this._data });
  }

  /**
   * Lovingly combine contexts, favoring the other with compassion.
   * @param {Context} other - The other context to merge
   * @returns {Context} A new Context with merged data
   */
  merge(other) {
    const newData = { ...this._data, ...other._data };
    return new Context(newData);
  }

  /**
   * Express as plain object for ecosystem integration.
   * @returns {Object} A copy of the internal data
   */
  toObject() {
    return JSON.parse(JSON.stringify(this._data));
  }

  /**
   * Check if a key exists in the context.
   * @param {string} key - The key to check
   * @returns {boolean} True if the key exists
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys in the context.
   * @returns {string[]} Array of keys
   */
  keys() {
    return Object.keys(this._data);
  }

  toString() {
    return `Context(${JSON.stringify(this._data)})`;
  }
}

class MutableContext {
  /**
   * Mutable context for performance-critical sections—use with care, but forgiven.
   * @param {Object} data - Initial data object
   */
  constructor(data = {}) {
    this._data = { ...data };
  }

  /**
   * Get a value from the context.
   * @param {string} key - The key to retrieve
   * @returns {*} The value or undefined
   */
  get(key) {
    return this._data[key];
  }

  /**
   * Change in place with gentle permission.
   * @param {string} key - The key to set
   * @param {*} value - The value to set
   */
  set(key, value) {
    this._data[key] = value;
  }

  /**
   * Return to safety with a fresh immutable copy.
   * @returns {Context} An immutable Context
   */
  toImmutable() {
    return new Context(this._data);
  }

  /**
   * Check if a key exists.
   * @param {string} key - The key to check
   * @returns {boolean} True if the key exists
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Get all keys.
   * @returns {string[]} Array of keys
   */
  keys() {
    return Object.keys(this._data);
  }

  toString() {
    return `MutableContext(${JSON.stringify(this._data)})`;
  }
}

module.exports = { Context, MutableContext };