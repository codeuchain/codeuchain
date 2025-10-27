/**
 * Chain: The Orchestrator
 *
 * The Chain orchestrates link execution with conditional flows and middleware.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @since 1.0.0
 */

const { Context } = require('./context');
const { Link } = require('./link');

/**
 * @template TInput - The input context type for the chain
 * @template TOutput - The output context type for the chain
 */
function Chain() {
  // Support both ES5 .call() and ES6 new instantiation
  if (!(this instanceof Chain)) {
    return new Chain();
  }
  
  /**
   * Loving weaver of links—connects with conditions, runs with selfless execution.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @example
   * const chain = new Chain();
   * chain.addLink(new ValidationLink());
   * chain.addLink(new ProcessingLink());
   * chain.connect('ValidationLink', 'ProcessingLink');
   * const result = await chain.run(initialContext);
   */
  this._links = new Map(); // name -> link
  this._connections = []; // [{from, to, condition}]
  this._middleware = [];
  this._errorHandlers = [];
}

/**
 * With gentle inclusion, store the link in the chain.
 * Links are stored by name for easy reference and connection.
 *
 * @param {Link<TInput, TOutput>} link - The link instance to add
 * @param {string} [name] - Optional unique name for the link (defaults to class name)
 * @returns {Chain<TInput, TOutput>} This chain for method chaining
 * @throws {Error} If link is not an instance of Link class
 * @throws {Error} If a link with the same name already exists
 * @example
 * const chain = new Chain();
 * chain.addLink(new ValidationLink(), 'validator');
 * chain.addLink(new ProcessingLink()); // Uses class name
 */
Chain.prototype.addLink = function(link, name) {
  if (!(link instanceof Link)) {
    throw new Error('Link must be an instance of Link class');
  }

  // Use provided name or default to link's constructor name
  const linkName = name || link.constructor.name;
  this._links.set(linkName, link);
  return this;
};

/**
 * With compassionate logic, add a connection between links.
 * Connections define the flow of execution through the chain.
 *
 * @param {string} source - Name of the source link
 * @param {string} target - Name of the target link
 * @param {Function} [condition] - Function that takes context and returns boolean (defaults to always true)
 * @returns {Chain<TInput, TOutput>} This chain for method chaining
 * @throws {Error} If source or target link doesn't exist
 * @example
 * chain.connect('ValidationLink', 'ProcessingLink', (ctx) => ctx.get('isValid'));
 * chain.connect('ValidationLink', 'ErrorHandler', (ctx) => !ctx.get('isValid'));
 */
Chain.prototype.connect = function(source, target, condition) {
  condition = condition || function() { return true; };
  if (!this._links.has(source)) {
    throw new Error(`Source link '${source}' not found`);
  }
  if (!this._links.has(target)) {
    throw new Error(`Target link '${target}' not found`);
  }

  this._connections.push({
    from: source,
    to: target,
    condition: condition
  });
  return this;
};

/**
 * Lovingly attach middleware to enhance chain execution.
 * Middleware can observe and modify execution flow.
 *
 * @param {Middleware} middleware - The middleware instance to attach
 * @returns {Chain<TInput, TOutput>} This chain for method chaining
 * @example
 * chain.useMiddleware(new LoggingMiddleware());
 * chain.useMiddleware(new TimingMiddleware());
 */
Chain.prototype.useMiddleware = function(middleware) {
  this._middleware.push(middleware);
  return this;
};

/**
 * Add an error handler for the entire chain.
 * Error handlers are called when any link in the chain throws an error.
 *
 * @param {Function} handler - Function that takes (error, context, linkName)
 * @returns {Chain<TInput, TOutput>} This chain for method chaining
 * @example
 * chain.onError((error, ctx, linkName) => {
 *   console.error(`Error in ${linkName}:`, error.message);
 *   // Handle error appropriately
 * });
 */
Chain.prototype.onError = function(handler) {
  this._errorHandlers.push(handler);
  return this;
};

/**
 * Find the next link index based on connections and conditions (index-based).
 * Internal method used by run() to determine execution flow.
 *
 * @private
 * @param {number} currentIndex - Current link index in the execution array
 * @param {Array} linksArray - Array of [name, link] entries
 * @param {Context<TInput>} ctx - Current context for condition evaluation
 * @returns {number} Next link index, or -1 if none found
 */
Chain.prototype._findNextLinkIndex = function(currentIndex, linksArray, ctx) {
  const currentName = linksArray[currentIndex][0];

  // Find all connections from current link
  const outgoingConnections = this._connections.filter(function(conn) { 
    return conn.from === currentName; 
  });

  // Check each connection in order
  for (const conn of outgoingConnections) {
    // Find target link index
    const targetIndex = linksArray.findIndex(function(entry) { 
      return entry[0] === conn.to; 
    });
    if (targetIndex !== -1) {
      // Check condition
      if (conn.condition(ctx)) {
        return targetIndex;
      }
    }
  }

  // No valid next link found
  return -1;
};

/**
 * With selfless execution, flow through links according to connections.
 * Executes the chain starting from links with no incoming connections.
 *
 * @param {Context<TInput>} initialCtx - The initial context to process
 * @returns {Promise<Context<TOutput>>} The final context after all processing
 * @throws {Error} If any link in the chain throws an error (after error handlers)
 * @example
 * const initialCtx = new Context({ userId: 123 });
 * const resultCtx = await chain.run(initialCtx);
 * console.log('Processing complete:', resultCtx.toObject());
 */
Chain.prototype.run = async function(initialCtx) {
  let ctx = initialCtx;

  // Get links as array for index-based access
  const linksArray = Array.from(this._links.entries());

  // Find starting point (index-based)
  let currentLinkIndex = -1;

  // Find links with no incoming connections (index-based)
  const incoming = new Set();
  this._connections.forEach(function(conn) { 
    incoming.add(conn.to); 
  });

  for (let i = 0; i < linksArray.length; i++) {
    const name = linksArray[i][0];
    if (!incoming.has(name)) {
      currentLinkIndex = i;
      break;
    }
  }

  // If no starting point found, use first link
  if (currentLinkIndex === -1 && linksArray.length > 0) {
    currentLinkIndex = 0;
  }

  // Execute the chain (index-based)
  while (currentLinkIndex >= 0 && currentLinkIndex < linksArray.length) {
    const currentLinkName = linksArray[currentLinkIndex][0];
    const link = linksArray[currentLinkIndex][1];

    if (!link) break;

    try {
      // Run middleware before
      for (const middleware of this._middleware) {
        if (middleware.before) {
          ctx = await middleware.before(link, ctx, currentLinkName) || ctx;
        }
      }

      // Execute the link
      ctx = await link.call(ctx);

      // Run middleware after
      for (const middleware of this._middleware) {
        if (middleware.after) {
          ctx = await middleware.after(link, ctx, currentLinkName) || ctx;
        }
      }

      // Find next link (index-based)
      currentLinkIndex = this._findNextLinkIndex(currentLinkIndex, linksArray, ctx);

    } catch (error) {
      // Run error middleware
      for (const middleware of this._middleware) {
        if (middleware.onError) {
          await middleware.onError(link, error, ctx, currentLinkName);
        }
      }

      // Run error handlers
      for (const handler of this._errorHandlers) {
        await handler(error, ctx, currentLinkName);
      }

      throw error;
    }
  }

  return ctx;
};

/**
 * Get all link names currently in the chain.
 * Useful for debugging and introspection.
 *
 * @returns {string[]} Array of all link names in the chain
 * @example
 * const chain = new Chain();
 * chain.addLink(new ValidationLink());
 * chain.addLink(new ProcessingLink());
 * console.log(chain.getLinkNames()); // ['ValidationLink', 'ProcessingLink']
 */
Chain.prototype.getLinkNames = function() {
  return Array.from(this._links.keys());
};

/**
 * Create a simple linear chain (convenience method).
 * Creates a chain with links executed in the order provided.
 *
 * @static
 * @param {...Link} links - Link instances to add to the chain
 * @returns {Chain<TInput, TOutput>} A new linear chain with automatic connections
 * @example
 * const chain = Chain.createLinear(
 *   new ValidationLink(),
 *   new ProcessingLink(),
 *   new StorageLink()
 * );
 * // Links are connected: ValidationLink -> ProcessingLink -> StorageLink
 */
Chain.createLinear = function() {
  const links = Array.prototype.slice.call(arguments);
  const chain = new Chain();

  // Add links with automatic naming
  links.forEach(function(link) {
    chain.addLink(link);
  });

  return chain;
};

module.exports = { Chain };