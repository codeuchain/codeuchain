/**
 * Middleware: The Gentle Enhancer
 *
 * With agape gentleness, the Middleware provides optional enhancement hooks.
 * Base class that implementations can extend.
 */

const { Context } = require('./context');
const { Link } = require('./link');

class Middleware {
  /**
   * Gentle enhancer—optional hooks with forgiving defaults.
   * Base class that middleware implementations can inherit from.
   * Subclasses can override any combination of before(), after(), and onError().
   */

  /**
   * With selfless optionality, do nothing by default.
   * @param {Link} link - The link about to be executed
   * @param {Context} ctx - The current context
   * @param {string} linkName - The name of the link
   */
  async before(link, ctx, linkName) {
    // Default: do nothing
  }

  /**
   * Forgiving default.
   * @param {Link} link - The link that was executed
   * @param {Context} ctx - The context after execution
   * @param {string} linkName - The name of the link
   */
  async after(link, ctx, linkName) {
    // Default: do nothing
  }

  /**
   * Compassionate error handling.
   * @param {Link} link - The link that threw the error
   * @param {Error} error - The error that occurred
   * @param {Context} ctx - The context at the time of error
   * @param {string} linkName - The name of the link
   */
  async onError(link, error, ctx, linkName) {
    // Default: log the error
    console.error(`Middleware caught error in ${linkName}:`, error.message);
  }
}

// Common middleware implementations

class LoggingMiddleware extends Middleware {
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

class TimingMiddleware extends Middleware {
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

class ValidationMiddleware extends Middleware {
  /**
   * Validates context before and after link execution.
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
  Middleware,
  LoggingMiddleware,
  TimingMiddleware,
  ValidationMiddleware
};