/**
 * Chain: The Harmonious Connector
 *
 * With agape harmony, the Chain orchestrates link execution with conditional flows and middleware.
 */

const { Context } = require('./context');
const { Link } = require('./link');

class Chain {
  /**
   * Loving weaver of links—connects with conditions, runs with selfless execution.
   */
  constructor() {
    this._links = new Map(); // name -> link
    this._connections = []; // [{from, to, condition}]
    this._middleware = [];
    this._errorHandlers = [];
  }

  /**
   * With gentle inclusion, store the link.
   * @param {Link} link - The link instance
   * @param {string} [name] - Optional unique name for the link (defaults to class name)
   * @returns {Chain} This chain for chaining
   */
  addLink(link, name = null) {
    if (!(link instanceof Link)) {
      throw new Error('Link must be an instance of Link class');
    }

    // Use provided name or default to link's constructor name
    const linkName = name || link.constructor.name;
    this._links.set(linkName, link);
    return this;
  }

  /**
   * With compassionate logic, add a connection between links.
   * @param {string} source - Source link name
   * @param {string} target - Target link name
   * @param {Function} condition - Function that takes context and returns boolean
   * @returns {Chain} This chain for chaining
   */
  connect(source, target, condition = () => true) {
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
  }

  /**
   * Lovingly attach middleware.
   * @param {Middleware} middleware - The middleware instance
   * @returns {Chain} This chain for chaining
   */
  useMiddleware(middleware) {
    this._middleware.push(middleware);
    return this;
  }

  /**
   * Add an error handler for the entire chain.
   * @param {Function} handler - Function that takes (error, context, linkName)
   * @returns {Chain} This chain for chaining
   */
  onError(handler) {
    this._errorHandlers.push(handler);
    return this;
  }

  /**
   * Find the next link index based on connections and conditions (index-based).
   * @param {number} currentIndex - Current link index
   * @param {Array} linksArray - Array of [name, link] entries
   * @param {Context} ctx - Current context
   * @returns {number} Next link index, or -1 if none found
   * @private
   */
  _findNextLinkIndex(currentIndex, linksArray, ctx) {
    const [currentName] = linksArray[currentIndex];

    // Find all connections from current link
    const outgoingConnections = this._connections.filter(conn => conn.from === currentName);

    // Check each connection in order
    for (const conn of outgoingConnections) {
      // Find target link index
      const targetIndex = linksArray.findIndex(([name]) => name === conn.to);
      if (targetIndex !== -1) {
        // Check condition
        if (conn.condition(ctx)) {
          return targetIndex;
        }
      }
    }

    // No valid next link found
    return -1;
  }

  /**
   * With selfless execution, flow through links.
   * @param {Context} initialCtx - The initial context
   * @returns {Promise<Context>} The final context after processing
   */
  async run(initialCtx) {
    let ctx = initialCtx;

    // Get links as array for index-based access
    const linksArray = Array.from(this._links.entries());

    // Find starting point (index-based)
    let currentLinkIndex = -1;

    // Find links with no incoming connections (index-based)
    const incoming = new Set();
    this._connections.forEach(conn => incoming.add(conn.to));

    for (let i = 0; i < linksArray.length; i++) {
      const [name] = linksArray[i];
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
      const [currentLinkName, link] = linksArray[currentLinkIndex];

      if (!link) break;

      try {
        // Run middleware before
        for (const middleware of this._middleware) {
          if (middleware.before) {
            await middleware.before(link, ctx, currentLinkName);
          }
        }

        // Execute the link
        ctx = await link.call(ctx);

        // Run middleware after
        for (const middleware of this._middleware) {
          if (middleware.after) {
            await middleware.after(link, ctx, currentLinkName);
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
  }  /**
   * Create a simple linear chain (convenience method).
   * @param {...Link} links - Link instances (names will be auto-generated)
   * @returns {Chain} A new linear chain
   */
  static createLinear(...links) {
    const chain = new Chain();

    // Add links with automatic naming
    links.forEach(link => {
      chain.addLink(link);
    });

    return chain;
  }
}

module.exports = { Chain };