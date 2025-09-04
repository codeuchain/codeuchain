/**
 * Link: The Selfless Processor
 *
 * With agape selflessness, the Link defines the interface for context processors.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @since 1.0.0
 */

const { Context } = require('./context');

/**
 * @template TInput - The input context type for this link
 * @template TOutput - The output context type for this link
 */
class Link {
  /**
   * Selfless processor—input context, output context, no judgment.
   * Base class that all link implementations should extend.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @example
   * class MyLink extends Link {
   *   async call(ctx) {
   *     // Process the context
   *     return ctx.insert('processed', true);
   *   }
   * }
   */

  /**
   * With unconditional love, process and return a transformed context.
   * Implementations should be pure functions with no side effects.
   *
   * @param {Context<TInput>} ctx - The input context to process
   * @returns {Promise<Context<TOutput>>} A promise that resolves to the transformed context
   * @throws {Error} If processing fails - implementations should throw descriptive errors
   * @example
   * async call(ctx) {
   *   const data = ctx.get('input');
   *   const result = await processData(data);
   *   return ctx.insert('output', result);
   * }
   */
  async call(ctx) {
    // Base implementation - should be overridden
    throw new Error('Link.call() must be implemented by subclass');
  }

  /**
   * Get the name of this link for debugging/logging purposes.
   * Defaults to the class constructor name.
   *
   * @returns {string} The name of the link
   * @example
   * class MyProcessor extends Link {}
   * const link = new MyProcessor();
   * console.log(link.getName()); // 'MyProcessor'
   */
  getName() {
    return this.constructor.name;
  }

  /**
   * Validate that the input context has all required fields.
   * Helper method for implementations to validate their inputs.
   *
   * @param {Context<TInput>} ctx - The context to validate
   * @param {string[]} requiredFields - Array of required field names
   * @throws {Error} If any required fields are missing from the context
   * @example
   * async call(ctx) {
   *   this.validateContext(ctx, ['userId', 'email']);
   *   // Continue processing...
   * }
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