/**
 * Hook: The Enhancement Layer
 *
 * The Hook provides optional enhancement hooks.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @since 1.0.0
 */

const { State } = require('./state');
const { Link } = require('./link');

/**
 * @template T - The state type that this hook operates on
 */
class Hook {
  /**
   * Gentle enhancer—optional hooks with forgiving defaults.
   * Base class that hook implementations can inherit from.
   * Subclasses can override any combination of before(), after(), and onError().
   * Enhanced with generic typing for type-safe workflows.
   *
   * @example
   * class LoggingHook extends Hook {
   *   async before(link, ctx, linkName) {
   *     console.log(`Starting ${linkName}`);
   *     return ctx.insert('startTime', Date.now());
   *   }
   *
   *   async after(link, ctx, linkName) {
   *     console.log(`Completed ${linkName}`);
   *   }
   * }
   */

  /**
   * With selfless optionality, do nothing by default.
   * Called before each link execution. Can return a modified state.
   *
   * @param {Link} link - The link about to be executed
   * @param {State<T>} ctx - The current state before link execution
   * @param {string} linkName - The name of the link being executed
   * @returns {Promise<State<T>|undefined>} Optionally return modified state
   * @example
   * async before(link, ctx, linkName) {
   *   console.log(`About to execute ${linkName}`);
   *   return ctx.insert('startTime', Date.now());
   * }
   */
  async before(link, ctx, linkName) {
    // Default: do nothing
  }

  /**
   * Forgiving default called after successful link execution.
   * Called after each successful link execution. Can return a modified state.
   *
   * @param {Link} link - The link that was executed
   * @param {State<T>} ctx - The state after link execution
   * @param {string} linkName - The name of the link that was executed
   * @returns {Promise<State<T>|undefined>} Optionally return modified state
   * @example
   * async after(link, ctx, linkName) {
   *   const duration = Date.now() - ctx.get('startTime');
   *   console.log(`${linkName} took ${duration}ms`);
   *   return ctx.insert('duration', duration);
   * }
   */
  async after(link, ctx, linkName) {
    // Default: do nothing
  }

  /**
   * Compassionate error handling called when links fail.
   * Called when any link throws an error during execution.
   *
   * @param {Link} link - The link that threw the error
   * @param {Error} error - The error that occurred
   * @param {State<T>} ctx - The state at the time of error
   * @param {string} linkName - The name of the link that failed
   * @returns {Promise<void>}
   * @example
   * async onError(link, error, ctx, linkName) {
   *   console.error(`Error in ${linkName}:`, error.message);
   *   // Send to error reporting service
   *   await errorReporting.report(error, { linkName, state: ctx.toObject() });
   * }
   */
  async onError(link, error, ctx, linkName) {
    // Default: log the error
    console.error(`Hook caught error in ${linkName}:`, error.message);
  }
}

// Common hook implementations

class LoggingHook extends Hook {
  /**
   * Logs link execution with timestamps.
   */
  async before(link, ctx, linkName) {
    console.log(`[${new Date().toISOString()}] Starting ${linkName}`);
  }

  async after(link, ctx, linkName) {
    console.log(`[${new Date().toISOString()}] Completed ${linkName}`);
  }

  async onError(link, error, ctx, linkName) {
    console.error(`[${new Date().toISOString()}] Error in ${linkName}: ${error.message}`);
  }
}

class TimingHook extends Hook {
  /**
   * Measures and logs execution time for each link.
   */
  constructor() {
    super();
    this._timings = new Map();
  }

  async before(link, ctx, linkName) {
    this._timings.set(linkName, Date.now());
  }

  async after(link, ctx, linkName) {
    const startTime = this._timings.get(linkName);
    if (startTime) {
      const duration = Date.now() - startTime;
      console.log(`${linkName} executed in ${duration}ms`);
      this._timings.delete(linkName);
    }
  }
}

class ValidationHook extends Hook {
  /**
   * Validates state before and after link execution.
   * @param {Object} options - Validation options
   * @param {Function} options.beforeValidator - Function to validate before execution
   * @param {Function} options.afterValidator - Function to validate after execution
   */
  constructor(options = {}) {
    super();
    this.beforeValidator = options.beforeValidator;
    this.afterValidator = options.afterValidator;
  }

  async before(link, ctx, linkName) {
    if (this.beforeValidator) {
      try {
        await this.beforeValidator(ctx, linkName);
      } catch (error) {
        throw new Error(`Pre-validation failed for ${linkName}: ${error.message}`);
      }
    }
  }

  async after(link, ctx, linkName) {
    if (this.afterValidator) {
      try {
        await this.afterValidator(ctx, linkName);
      } catch (error) {
        throw new Error(`Post-validation failed for ${linkName}: ${error.message}`);
      }
    }
  }
}

module.exports = {
  Hook,
  LoggingHook,
  TimingHook,
  ValidationHook
};