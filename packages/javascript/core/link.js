/**
 * Link: The Processing Unit
 *
 * The Link defines the interface for state processors.
 * Base class that implementations can extend.
 * Enhanced with generic typing for type-safe workflows.
 *
 * @since 1.0.0
 */

const { State } = require('./state');

/**
 * @template TInput - The input state type for this link
 * @template TOutput - The output state type for this link
 */
class Link {
  /**
   * Processing unit—input state, output state, focused transformation.
   * Base class that all link implementations should extend.
   * Enhanced with generic typing for type-safe workflows.
   *
   * @example
   * class MyLink extends Link {
   *   async call(ctx) {
   *     // Process the state
   *     return ctx.insert('processed', true);
   *   }
   * }
   */

  /**
   * With unconditional love, process and return a transformed state.
   * Implementations should be pure functions with no side effects.
   *
   * @param {State<TInput>} ctx - The input state to process
   * @returns {Promise<State<TOutput>>} A promise that resolves to the transformed state
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
   * Validate that the input state has all required fields.
   * Helper method for implementations to validate their inputs.
   *
   * @param {State<TInput>} ctx - The state to validate
   * @param {string[]} requiredFields - Array of required field names
   * @throws {Error} If any required fields are missing from the state
   * @example
   * async call(ctx) {
   *   this.validateState(ctx, ['userId', 'email']);
   *   // Continue processing...
   * }
   */
  validateState(ctx, requiredFields = []) {
    for (const field of requiredFields) {
      if (!ctx.has(field)) {
        throw new Error(`Required field '${field}' is missing from state`);
      }
    }
  }
}

module.exports = { Link };