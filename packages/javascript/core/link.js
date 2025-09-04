/**
 * Link: The Selfless Processor
 *
 * With agape selflessness, the Link defines the interface for context processors.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 */

const { Context } = require('./context');

/**
 * @template TInput
 * @template TOutput
 */
class Link {
  /**
   * Selfless processor—input context, output context, no judgment.
   * Base class that all link implementations should extend.
   * Enhanced with generic typing for type-safe workflows.
   */

  /**
   * With unconditional love, process and return a transformed context.
   * Implementations should be pure functions with no side effects.
   * @param {Context<TInput>} ctx - The input context
   * @returns {Promise<Context<TOutput>>} A promise that resolves to the transformed context
   */
  async call(ctx) {
    // Base implementation - should be overridden
    throw new Error('Link.call() must be implemented by subclass');
  }

  /**
   * Get the name of this link for debugging/logging.
   * @returns {string} The name of the link
   */
  getName() {
    return this.constructor.name;
  }

  /**
   * Validate that the input context has required fields.
   * @param {Context<TInput>} ctx - The context to validate
   * @param {string[]} requiredFields - Array of required field names
   * @throws {Error} If required fields are missing
   */
  validateContext(ctx, requiredFields = []) {
    for (const field of requiredFields) {
      if (!ctx.has(field)) {
        throw new Error(`Required field '${field}' is missing from context`);
      }
    }
  }
}

module.exports = { Link };