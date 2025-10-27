/**
 * Middleware: The Enhancement Layer
 *
 * The Middleware provides optional enhancement hooks.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @since 1.0.0
 */

const { Context } = require('./context');
const { Link } = require('./link');

/**
 * @template T - The context type that this middleware operates on
 */
function Middleware() {
  // Support both ES5 .call() and ES6 new instantiation
  if (!(this instanceof Middleware)) {
    return new Middleware();
  }
  
  /**
   * Gentle enhancer—optional hooks with forgiving defaults.
   * Base class that middleware implementations can inherit from.
   * Subclasses can override any combination of before(), after(), and onError().
   * Enhanced with generic typing for type-safe workflows.
   *
   * @example
   * class LoggingMiddleware extends Middleware {
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
}

/**
 * With selfless optionality, do nothing by default.
 * Called before each link execution. Can return a modified context.
 *
 * @param {Link} link - The link about to be executed
 * @param {Context<T>} ctx - The current context before link execution
 * @param {string} linkName - The name of the link being executed
 * @returns {Promise<Context<T>|undefined>} Optionally return modified context
 * @example
 * async before(link, ctx, linkName) {
 *   console.log(`About to execute ${linkName}`);
 *   return ctx.insert('startTime', Date.now());
 * }
 */
Middleware.prototype.before = async function(link, ctx, linkName) {
  // Default: do nothing
};

/**
 * Forgiving default called after successful link execution.
 * Called after each successful link execution. Can return a modified context.
 *
 * @param {Link} link - The link that was executed
 * @param {Context<T>} ctx - The context after link execution
 * @param {string} linkName - The name of the link that was executed
 * @returns {Promise<Context<T>|undefined>} Optionally return modified context
 * @example
 * async after(link, ctx, linkName) {
 *   const duration = Date.now() - ctx.get('startTime');
 *   console.log(`${linkName} took ${duration}ms`);
 *   return ctx.insert('duration', duration);
 * }
 */
Middleware.prototype.after = async function(link, ctx, linkName) {
  // Default: do nothing
};

/**
 * Compassionate error handling called when links fail.
 * Called when any link throws an error during execution.
 *
 * @param {Link} link - The link that threw the error
 * @param {Error} error - The error that occurred
 * @param {Context<T>} ctx - The context at the time of error
 * @param {string} linkName - The name of the link that failed
 * @returns {Promise<void>}
 * @example
 * async onError(link, error, ctx, linkName) {
 *   console.error(`Error in ${linkName}:`, error.message);
 *   // Send to error reporting service
 *   await errorReporting.report(error, { linkName, context: ctx.toObject() });
 * }
 */
Middleware.prototype.onError = async function(link, error, ctx, linkName) {
  // Default: log the error
  console.error(`Middleware caught error in ${linkName}:`, error.message);
};

// Common middleware implementations

function LoggingMiddleware() {
  // Support ES5 .call() inheritance and ES6 new
  if (!(this instanceof LoggingMiddleware)) {
    return new LoggingMiddleware();
  }
  Middleware.call(this);
}

/**
 * Logs link execution with timestamps.
 */
LoggingMiddleware.prototype = Object.create(Middleware.prototype);
LoggingMiddleware.prototype.constructor = LoggingMiddleware;

LoggingMiddleware.prototype.before = async function(link, ctx, linkName) {
  console.log(`[${new Date().toISOString()}] Starting ${linkName}`);
};

LoggingMiddleware.prototype.after = async function(link, ctx, linkName) {
  console.log(`[${new Date().toISOString()}] Completed ${linkName}`);
};

LoggingMiddleware.prototype.onError = async function(link, error, ctx, linkName) {
  console.error(`[${new Date().toISOString()}] Error in ${linkName}: ${error.message}`);
};

function TimingMiddleware() {
  // Support ES5 .call() inheritance and ES6 new
  if (!(this instanceof TimingMiddleware)) {
    return new TimingMiddleware();
  }
  Middleware.call(this);
  
  /**
   * Measures and logs execution time for each link.
   */
  this._timings = new Map();
}

TimingMiddleware.prototype = Object.create(Middleware.prototype);
TimingMiddleware.prototype.constructor = TimingMiddleware;

TimingMiddleware.prototype.before = async function(link, ctx, linkName) {
  this._timings.set(linkName, Date.now());
};

TimingMiddleware.prototype.after = async function(link, ctx, linkName) {
  const startTime = this._timings.get(linkName);
  if (startTime) {
    const duration = Date.now() - startTime;
    console.log(`${linkName} executed in ${duration}ms`);
    this._timings.delete(linkName);
  }
};

function ValidationMiddleware(options) {
  // Support ES5 .call() inheritance and ES6 new
  if (!(this instanceof ValidationMiddleware)) {
    return new ValidationMiddleware(options);
  }
  Middleware.call(this);
  
  /**
   * Validates context before and after link execution.
   * @param {Object} options - Validation options
   * @param {Function} options.beforeValidator - Function to validate before execution
   * @param {Function} options.afterValidator - Function to validate after execution
   */
  options = options || {};
  this.beforeValidator = options.beforeValidator;
  this.afterValidator = options.afterValidator;
}

ValidationMiddleware.prototype = Object.create(Middleware.prototype);
ValidationMiddleware.prototype.constructor = ValidationMiddleware;

ValidationMiddleware.prototype.before = async function(link, ctx, linkName) {
  if (this.beforeValidator) {
    try {
      await this.beforeValidator(ctx, linkName);
    } catch (error) {
      throw new Error(`Pre-validation failed for ${linkName}: ${error.message}`);
    }
  }
};

ValidationMiddleware.prototype.after = async function(link, ctx, linkName) {
  if (this.afterValidator) {
    try {
      await this.afterValidator(ctx, linkName);
    } catch (error) {
      throw new Error(`Post-validation failed for ${linkName}: ${error.message}`);
    }
  }
};

module.exports = {
  Middleware,
  LoggingMiddleware,
  TimingMiddleware,
  ValidationMiddleware
};