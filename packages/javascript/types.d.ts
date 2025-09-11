/**
 * CodeUChain TypeScript Definitions
 * 
 * Comprehensive type definitions for CodeUChain's opt-in generic typing features.
 * Provides type-safe workflows while maintaining runtime flexibility and backward compatibility.
 * 
 * @fileoverview Main TypeScript definitions for CodeUChain
 * @version 1.0.1
 * @since 1.0.0
 */

/**
 * Generic input type parameter for Links and Chains.
 * Use specific types for type safety, or leave as `any` for maximum flexibility.
 * 
 * @example
 * ```typescript
 * // Type-safe usage
 * interface UserInput { name: string; email: string; }
 * class ValidateUser extends Link<UserInput, UserValidated> { ... }
 * 
 * // Flexible usage
 * class FlexibleLink extends Link<any, any> { ... }
 * ```
 */
export type TInput = any;

/**
 * Generic output type parameter for Links and Chains.
 * Use specific types for type safety, or leave as `any` for maximum flexibility.
 * 
 * @example
 * ```typescript
 * // Type-safe usage
 * interface UserValidated { name: string; email: string; isValid: boolean; }
 * class ValidateUser extends Link<UserInput, UserValidated> { ... }
 * 
 * // Flexible usage  
 * class FlexibleLink extends Link<any, any> { ... }
 * ```
 */
export type TOutput = any;

/**
 * Context: The Loving Vessel
 * 
 * Immutable context container that holds data with agape compassion.
 * Enhanced with opt-in generic typing for type-safe workflows while maintaining
 * runtime flexibility. Provides clean type evolution without explicit casting.
 * 
 * **Performance Characteristics:**
 * - Immutable by default (uses Object.freeze() and deep copying)
 * - Zero runtime overhead for typing (same storage as Record<string, any>)
 * - Memory-efficient through structural sharing when possible
 * 
 * **Type Evolution:**
 * Use `insertAs<TNew>()` to transform between related types cleanly.
 * The context maintains the same runtime behavior regardless of TypeScript annotations.
 * 
 * @template T The type of data structure this context represents
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Basic usage
 * const ctx = new Context({ name: 'Alice', age: 30 });
 * console.log(ctx.get('name')); // 'Alice'
 * 
 * // Type-safe usage with interfaces
 * interface User { name: string; age: number; }
 * const userCtx = new Context<User>({ name: 'Alice', age: 30 });
 * 
 * // Type evolution
 * interface UserWithEmail extends User { email: string; }
 * const enhancedCtx = userCtx.insertAs<UserWithEmail>('email', 'alice@example.com');
 * 
 * // Mixed typed/untyped usage (fully compatible)
 * const untypedCtx = new Context({ dynamic: 'value' });
 * const mixedChain = untypedCtx.insert('typed', userCtx.get('name'));
 * ```
 */
export declare class Context<T = Record<string, any>> {
  /**
   * Creates a new immutable Context with the provided data.
   * Data is deep frozen to ensure immutability at all levels.
   * 
   * **Error Handling:**
   * Throws TypeError if data contains circular references when deep freezing.
   * 
   * @param data Initial data object to store in the context (default: {})
   * @throws {TypeError} If data contains circular references
   * 
   * @example
   * ```typescript
   * // Basic construction
   * const ctx = new Context({ name: 'Alice', age: 30 });
   * 
   * // With type annotation
   * interface User { name: string; age: number; }
   * const typedCtx = new Context<User>({ name: 'Alice', age: 30 });
   * 
   * // Empty context
   * const emptyCtx = new Context();
   * ```
   */
  constructor(data?: Record<string, any>);

  /**
   * Creates an empty context with no initial data.
   * Useful as a starting point for building contexts through chaining.
   * 
   * **Performance:** More efficient than `new Context({})` as it avoids object creation.
   * 
   * @returns An empty Context instance
   * 
   * @example
   * ```typescript
   * const emptyCtx = Context.empty<User>();
   * const populatedCtx = emptyCtx
   *   .insert('name', 'Alice')
   *   .insert('age', 30);
   * ```
   */
  static empty<T = any>(): Context<T>;

  /**
   * Creates a context from existing data with type inference.
   * Provides better type inference than the constructor in many cases.
   * 
   * @param data The data to create context from
   * @returns A new Context with the provided data and inferred type
   * 
   * @example
   * ```typescript
   * const userData = { name: 'Alice', age: 30 };
   * const ctx = Context.from(userData); // Type inferred as Context<{name: string, age: number}>
   * 
   * // Compare with constructor (requires explicit typing)
   * const ctx2 = new Context<typeof userData>(userData);
   * ```
   */
  static from<TData = any>(data: TData): Context<TData>;

  /**
   * Retrieves a value by key with gentle care, returning undefined if not found.
   * Returns deep copies of objects/arrays to maintain immutability.
   * 
   * **Performance:** O(1) lookup, O(n) for deep copying complex objects.
   * **Type Safety:** Returns `any` for maximum flexibility across typed/untyped usage.
   * 
   * @param key The key to retrieve from the context
   * @returns The value associated with the key, or undefined if not found
   * 
   * @example
   * ```typescript
   * const ctx = new Context({ 
   *   name: 'Alice', 
   *   data: { nested: 'value' },
   *   missing: undefined 
   * });
   * 
   * console.log(ctx.get('name'));      // 'Alice'
   * console.log(ctx.get('missing'));   // undefined  
   * console.log(ctx.get('notFound'));  // undefined
   * 
   * // Deep copies prevent mutation
   * const nested = ctx.get('data');
   * nested.nested = 'changed'; // Safe - doesn't affect original
   * ```
   */
  get(key: string): any;

  /**
   * Creates a new Context with an additional key-value pair, preserving the current type.
   * The original context remains unchanged (immutable operation).
   * 
   * **Type Preservation:** Maintains the same generic type `T` as the original context.
   * **Performance:** O(n) where n is the number of keys (creates new object).
   * 
   * @param key The key to insert into the context
   * @param value The value to associate with the key
   * @returns A new Context with the inserted key-value pair (same type T)
   * 
   * @example
   * ```typescript
   * interface User { name: string; age: number; }
   * const userCtx = new Context<User>({ name: 'Alice', age: 30 });
   * 
   * // Type is preserved as Context<User>
   * const updatedCtx = userCtx.insert('age', 31);
   * 
   * // Chain multiple insertions
   * const chainedCtx = userCtx
   *   .insert('name', 'Bob')
   *   .insert('age', 25);
   * 
   * // Original context unchanged
   * console.log(userCtx.get('age'));     // 30
   * console.log(updatedCtx.get('age'));  // 31
   * ```
   */
  insert(key: string, value: any): Context<T>;

  /**
   * Creates a new Context with type evolution, enabling clean transformation between related types.
   * This is the key method for type-safe workflows with opt-in generics.
   * 
   * **Type Evolution:** Allows transitioning from one type to another without explicit casting.
   * **Runtime Behavior:** Identical to `insert()` - no performance difference.
   * **Design Philosophy:** Enables clean typed workflows while maintaining runtime flexibility.
   * 
   * @template TNew The new type this context should represent after insertion
   * @param key The key to insert into the context
   * @param value The value to associate with the key
   * @returns A new Context with the evolved type TNew
   * 
   * @example
   * ```typescript
   * // Type evolution example
   * interface UserInput { name: string; email: string; }
   * interface UserValidated extends UserInput { isValid: boolean; }
   * interface UserWithProfile extends UserValidated { age: number; profileComplete: boolean; }
   * 
   * const inputCtx = new Context<UserInput>({ name: 'Alice', email: 'alice@example.com' });
   * 
   * // Clean type evolution without casting
   * const validatedCtx = inputCtx.insertAs<UserValidated>('isValid', true);
   * const profileCtx = validatedCtx.insertAs<UserWithProfile>('age', 30);
   * const completeCtx = profileCtx.insertAs<UserWithProfile>('profileComplete', true);
   * 
   * // Each step maintains type safety
   * const isValid: boolean = validatedCtx.get('isValid');
   * const age: number = profileCtx.get('age');
   * 
   * // Mixed with untyped usage (fully compatible)
   * const flexibleCtx = completeCtx.insertAs('dynamicField', 'dynamicValue');
   * ```
   */
  insertAs<TNew = any>(key: string, value: any): Context<TNew>;

  /**
   * Creates a mutable version of this context for performance-critical sections.
   * Useful when many sequential modifications are needed.
   * 
   * **Performance:** Mutable operations are faster for bulk updates.
   * **Safety:** Use sparingly and convert back to immutable when done.
   * **Pattern:** Mutable contexts should have limited scope and be converted back quickly.
   * 
   * @returns A mutable version of this context with the same type
   * 
   * @example
   * ```typescript
   * const immutableCtx = new Context({ counter: 0 });
   * 
   * // Performance-critical section
   * const mutableCtx = immutableCtx.withMutation();
   * for (let i = 0; i < 1000; i++) {
   *   mutableCtx.set(`item_${i}`, i);
   * }
   * 
   * // Back to immutable for safety
   * const finalCtx = mutableCtx.toImmutable();
   * ```
   */
  withMutation(): MutableContext<T>;

  /**
   * Combines this context with another, with the other context's values taking precedence.
   * Creates a new context without modifying either original context.
   * 
   * **Merge Strategy:** Right-hand side wins for conflicting keys.
   * **Type Safety:** Both contexts must have the same generic type T.
   * **Performance:** O(n + m) where n and m are the number of keys in each context.
   * 
   * @param other The other context to merge with this one
   * @returns A new Context with merged data
   * @throws {TypeError} If other is not a Context instance
   * 
   * @example
   * ```typescript
   * interface User { name: string; age: number; city?: string; }
   * 
   * const ctx1 = new Context<User>({ name: 'Alice', age: 25 });
   * const ctx2 = new Context<User>({ age: 30, city: 'NYC' });
   * 
   * const merged = ctx1.merge(ctx2);
   * console.log(merged.get('name')); // 'Alice' (from ctx1)
   * console.log(merged.get('age'));  // 30 (ctx2 wins)
   * console.log(merged.get('city')); // 'NYC' (from ctx2)
   * 
   * // Error handling
   * try {
   *   ctx1.merge(null); // TypeError: Invalid context
   * } catch (error) {
   *   console.error('Merge failed:', error.message);
   * }
   * ```
   */
  merge(other: Context<T>): Context<T>;

  /**
   * Converts the context to a plain JavaScript object for ecosystem integration.
   * Returns a deep copy to maintain immutability of the original context.
   * 
   * **Use Cases:** Serialization, logging, integration with non-CodeUChain libraries.
   * **Performance:** O(n) deep copy operation.
   * **Safety:** Returned object is completely detached from the original context.
   * 
   * @returns A deep copy of the internal data as a plain JavaScript object
   * 
   * @example
   * ```typescript
   * const ctx = new Context({ 
   *   user: { name: 'Alice', data: { score: 100 } },
   *   timestamp: Date.now()
   * });
   * 
   * // Safe conversion for external use
   * const plainObj = ctx.toObject();
   * plainObj.user.data.score = 0; // Safe - doesn't affect original
   * 
   * // Integration examples
   * const jsonString = JSON.stringify(ctx.toObject());
   * const logData = { ...ctx.toObject(), logLevel: 'info' };
   * await externalAPI.send(ctx.toObject());
   * ```
   */
  toObject(): Record<string, any>;

  /**
   * Checks if a key exists in the context, regardless of its value.
   * Returns true even if the value is undefined, null, or falsy.
   * 
   * **Performance:** O(1) operation.
   * **Behavior:** Checks for key existence, not value truthiness.
   * 
   * @param key The key to check for existence
   * @returns True if the key exists in the context, false otherwise
   * 
   * @example
   * ```typescript
   * const ctx = new Context({ 
   *   name: 'Alice',
   *   age: 0,           // falsy but exists
   *   active: false,    // falsy but exists
   *   data: null,       // null but exists
   *   undefined: undefined // undefined but exists
   * });
   * 
   * console.log(ctx.has('name'));      // true
   * console.log(ctx.has('age'));       // true (even though 0)
   * console.log(ctx.has('active'));    // true (even though false)
   * console.log(ctx.has('data'));      // true (even though null)
   * console.log(ctx.has('undefined')); // true (key exists)
   * console.log(ctx.has('missing'));   // false (key doesn't exist)
   * ```
   */
  has(key: string): boolean;

  /**
   * Returns an array of all keys in the context.
   * Order is not guaranteed and may vary between JavaScript engines.
   * 
   * **Performance:** O(n) where n is the number of keys.
   * **Use Cases:** Iteration, debugging, serialization control.
   * 
   * @returns Array of all keys in the context
   * 
   * @example
   * ```typescript
   * const ctx = new Context({ name: 'Alice', age: 30, city: 'NYC' });
   * const allKeys = ctx.keys(); // ['name', 'age', 'city'] (order may vary)
   * 
   * // Iteration example
   * allKeys.forEach(key => {
   *   console.log(`${key}: ${ctx.get(key)}`);
   * });
   * 
   * // Filtering example
   * const userKeys = allKeys.filter(key => key.startsWith('user'));
   * ```
   */
  keys(): string[];
}

/**
 * MutableContext: The Performance-Focused Vessel
 * 
 * Mutable version of Context for performance-critical sections where many sequential
 * modifications are needed. Should be used sparingly and converted back to immutable
 * Context when modifications are complete.
 * 
 * **Performance Characteristics:**
 * - Mutations are O(1) operations (no object copying)
 * - Significantly faster for bulk operations (10-100x for large datasets)
 * - Higher memory efficiency during bulk updates
 * - Less garbage collection pressure
 * 
 * **Safety Considerations:**
 * - Use with limited scope - convert back to immutable quickly
 * - Not thread-safe (but JavaScript is single-threaded)
 * - Avoid sharing mutable contexts between functions
 * - Always convert to immutable before returning from functions
 * 
 * @template T The type of data structure this mutable context represents
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Performance-critical bulk updates
 * const immutableCtx = new Context<UserData>({ users: [] });
 * const mutableCtx = immutableCtx.withMutation();
 * 
 * // Fast bulk operations
 * for (let i = 0; i < 10000; i++) {
 *   mutableCtx.set(`user_${i}`, { id: i, name: `User ${i}` });
 * }
 * 
 * // Back to safe immutable
 * const result = mutableCtx.toImmutable();
 * 
 * // Pattern: Limited scope usage
 * function bulkProcess(data: any[]): Context<ProcessedData> {
 *   const mutable = Context.empty<ProcessedData>().withMutation();
 *   
 *   data.forEach((item, index) => {
 *     mutable.set(`item_${index}`, processItem(item));
 *   });
 *   
 *   return mutable.toImmutable(); // Always return immutable
 * }
 * ```
 */
export declare class MutableContext<T = Record<string, any>> {
  /**
   * Creates a new mutable context with the provided data.
   * Unlike immutable Context, data is not frozen and can be modified directly.
   * 
   * **Recommendation:** Prefer `Context.withMutation()` over direct construction.
   * 
   * @param data Initial data object to store (default: {})
   * 
   * @example
   * ```typescript
   * // Direct construction (not recommended)
   * const mutableCtx = new MutableContext({ count: 0 });
   * 
   * // Preferred approach
   * const immutableCtx = new Context({ count: 0 });
   * const mutableCtx = immutableCtx.withMutation();
   * ```
   */
  constructor(data?: Record<string, any>);

  /**
   * Retrieves a value by key, identical to immutable Context.get().
   * No deep copying is performed since mutations are expected.
   * 
   * **Performance:** O(1) operation, faster than immutable Context.get() for objects.
   * **Warning:** Returned objects are mutable and changes will affect the context.
   * 
   * @param key The key to retrieve from the context
   * @returns The value associated with the key, or undefined if not found
   * 
   * @example
   * ```typescript
   * const mutableCtx = new MutableContext({ data: { count: 5 } });
   * 
   * const data = mutableCtx.get('data'); 
   * data.count = 10; // Warning: This mutates the context!
   * 
   * console.log(mutableCtx.get('data')); // { count: 10 } - modified
   * ```
   */
  get(key: string): any;

  /**
   * Sets a key-value pair directly in this context (mutation operation).
   * Modifies the existing context rather than creating a new one.
   * 
   * **Performance:** O(1) operation - very fast for bulk updates.
   * **Mutation:** This method modifies the existing context.
   * **Return:** Void - operation modifies this context directly.
   * 
   * @param key The key to set in the context
   * @param value The value to associate with the key
   * 
   * @example
   * ```typescript
   * const mutableCtx = new MutableContext({ count: 0 });
   * 
   * // Direct mutation
   * mutableCtx.set('count', 1);
   * mutableCtx.set('name', 'Alice');
   * 
   * console.log(mutableCtx.get('count')); // 1
   * console.log(mutableCtx.get('name'));  // 'Alice'
   * 
   * // Bulk updates are very efficient
   * const startTime = performance.now();
   * for (let i = 0; i < 10000; i++) {
   *   mutableCtx.set(`item_${i}`, i);
   * }
   * const endTime = performance.now();
   * console.log(`Bulk update took ${endTime - startTime}ms`);
   * ```
   */
  set(key: string, value: any): void;

  /**
   * Converts this mutable context back to an immutable Context.
   * Creates a deep-frozen copy, leaving the original mutable context unchanged.
   * 
   * **Best Practice:** Always call this when done with mutations.
   * **Performance:** O(n) operation to create immutable copy.
   * **Safety:** Returned context is completely immutable and safe to share.
   * 
   * @returns A new immutable Context with the same data and type
   * 
   * @example
   * ```typescript
   * function processLargeDataset(items: any[]): Context<ProcessedData> {
   *   const mutableCtx = Context.empty<ProcessedData>().withMutation();
   *   
   *   // Fast bulk processing
   *   items.forEach((item, index) => {
   *     mutableCtx.set(`processed_${index}`, processItem(item));
   *     mutableCtx.set(`metadata_${index}`, getMetadata(item));
   *   });
   *   
   *   // Convert back to immutable before returning
   *   return mutableCtx.toImmutable();
   * }
   * 
   * // Usage
   * const result = processLargeDataset(largeArray);
   * // result is now immutable and safe to use
   * ```
   */
  toImmutable(): Context<T>;

  /**
   * Checks if a key exists in the context, identical to immutable Context.has().
   * 
   * @param key The key to check for existence
   * @returns True if the key exists, false otherwise
   * 
   * @example
   * ```typescript
   * const mutableCtx = new MutableContext({ name: 'Alice' });
   * 
   * console.log(mutableCtx.has('name'));     // true
   * console.log(mutableCtx.has('missing'));  // false
   * 
   * mutableCtx.set('age', 30);
   * console.log(mutableCtx.has('age'));      // true
   * ```
   */
  has(key: string): boolean;

  /**
   * Returns an array of all keys in the context, identical to immutable Context.keys().
   * 
   * @returns Array of all keys in the context
   * 
   * @example
   * ```typescript
   * const mutableCtx = new MutableContext({ name: 'Alice', age: 30 });
   * 
   * console.log(mutableCtx.keys()); // ['name', 'age'] (order may vary)
   * 
   * mutableCtx.set('city', 'NYC');
   * console.log(mutableCtx.keys()); // ['name', 'age', 'city']
   * ```
   */
  keys(): string[];
}

/**
 * Link: The Selfless Processor
 * 
 * Base class for all context processors in CodeUChain. Implements the core pattern
 * of transforming input contexts to output contexts with agape selflessness.
 * Enhanced with opt-in generic typing for type-safe workflows.
 * 
 * **Design Philosophy:**
 * - Selfless processing: Focus on transformation, not state
 * - Pure functions: No side effects, predictable behavior
 * - Type evolution: Clean transitions between related types
 * - Error transparency: Clear error handling and reporting
 * 
 * **Generic Type Parameters:**
 * - `TInput`: The expected input context data shape
 * - `TOutput`: The resulting output context data shape
 * - Use `any` for maximum flexibility or specific interfaces for type safety
 * 
 * **Performance Characteristics:**
 * - Async by design for I/O operations and external services
 * - Zero runtime overhead for typing (same as untyped Links)
 * - Memory efficient through immutable context patterns
 * 
 * @template TInput The input context type for this link
 * @template TOutput The output context type for this link
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Type-safe Link implementation
 * interface UserInput { name: string; email: string; }
 * interface UserValidated extends UserInput { isValid: boolean; emailConfirmed: boolean; }
 * 
 * class ValidateUserLink extends Link<UserInput, UserValidated> {
 *   async call(ctx: Context<UserInput>): Promise<Context<UserValidated>> {
 *     const name = ctx.get('name');
 *     const email = ctx.get('email');
 *     
 *     // Validation logic
 *     const isValid = name.length > 0 && email.includes('@');
 *     const emailConfirmed = await this.checkEmailExists(email);
 *     
 *     // Type evolution with insertAs
 *     return ctx
 *       .insertAs<UserValidated>('isValid', isValid)
 *       .insert('emailConfirmed', emailConfirmed);
 *   }
 *   
 *   private async checkEmailExists(email: string): Promise<boolean> {
 *     // External validation logic
 *     return true;
 *   }
 * }
 * 
 * // Flexible Link (works with any data)
 * class LoggingLink extends Link<any, any> {
 *   async call(ctx: Context<any>): Promise<Context<any>> {
 *     console.log('Processing context:', ctx.toObject());
 *     return ctx.insert('logged', true);
 *   }
 * }
 * 
 * // Mixed typed/untyped usage
 * const userCtx = new Context<UserInput>({ name: 'Alice', email: 'alice@example.com' });
 * const validatedCtx = await new ValidateUserLink().call(userCtx);
 * const loggedCtx = await new LoggingLink().call(validatedCtx); // Works seamlessly
 * ```
 */
export declare class Link<TInput = any, TOutput = any> {
  /**
   * Core processing method that transforms an input context to an output context.
   * This method should be implemented by all concrete Link classes.
   * 
   * **Implementation Guidelines:**
   * - Should be a pure function with no side effects
   * - Should not modify the input context (it's immutable anyway)
   * - Should handle errors gracefully and throw descriptive errors
   * - Should use context.insertAs() for type evolution when using generics
   * - Can perform async operations (I/O, external services, etc.)
   * 
   * **Error Handling:**
   * - Throw descriptive errors that will be caught by Chain error handlers
   * - Include context about what went wrong and potential solutions
   * - Use specific Error types when appropriate (ValidationError, NetworkError, etc.)
   * 
   * **Type Safety:**
   * - Input context is typed as Context<TInput>
   * - Return type must be Context<TOutput> wrapped in Promise
   * - Use insertAs<TOutput>() for clean type evolution
   * 
   * @param ctx The input context to process
   * @returns A promise that resolves to the transformed context
   * @throws {Error} When processing fails - should include descriptive error messages
   * 
   * @example
   * ```typescript
   * // Basic implementation
   * class UppercaseLink extends Link<{text: string}, {text: string, uppercased: string}> {
   *   async call(ctx: Context<{text: string}>): Promise<Context<{text: string, uppercased: string}>> {
   *     const text = ctx.get('text');
   *     
   *     if (typeof text !== 'string') {
   *       throw new Error('UppercaseLink requires text field to be a string');
   *     }
   *     
   *     return ctx.insertAs('uppercased', text.toUpperCase());
   *   }
   * }
   * 
   * // Async operations
   * class FetchUserLink extends Link<{userId: string}, {userId: string, user: User}> {
   *   async call(ctx: Context<{userId: string}>): Promise<Context<{userId: string, user: User}>> {
   *     const userId = ctx.get('userId');
   *     
   *     try {
   *       const user = await this.fetchUser(userId);
   *       return ctx.insertAs('user', user);
   *     } catch (error) {
   *       throw new Error(`Failed to fetch user ${userId}: ${error.message}`);
   *     }
   *   }
   *   
   *   private async fetchUser(userId: string): Promise<User> {
   *     // External API call
   *   }
   * }
   * 
   * // Error handling
   * class ValidatedProcessingLink extends Link<InputData, ValidatedData> {
   *   async call(ctx: Context<InputData>): Promise<Context<ValidatedData>> {
   *     this.validateContext(ctx, ['requiredField', 'anotherField']);
   *     
   *     // Processing logic here
   *     return ctx.insertAs('validated', true);
   *   }
   * }
   * ```
   */
  call(ctx: Context<TInput>): Promise<Context<TOutput>>;

  /**
   * Returns a human-readable name for this link, useful for debugging and logging.
   * Default implementation returns the class name, but can be overridden.
   * 
   * **Use Cases:**
   * - Error messages and stack traces
   * - Logging and monitoring
   * - Chain visualization and debugging
   * - Performance profiling
   * 
   * @returns A descriptive name for this link
   * 
   * @example
   * ```typescript
   * class ValidateUserEmailLink extends Link<UserInput, UserValidated> {
   *   getName(): string {
   *     return 'User Email Validation';
   *   }
   *   
   *   async call(ctx: Context<UserInput>): Promise<Context<UserValidated>> {
   *     // Implementation
   *   }
   * }
   * 
   * // Usage in logging
   * const link = new ValidateUserEmailLink();
   * console.log(`Executing: ${link.getName()}`); // "Executing: User Email Validation"
   * 
   * // Chain will use this for error reporting
   * try {
   *   await chain.run(inputCtx);
   * } catch (error) {
   *   console.error(`Error in ${link.getName()}: ${error.message}`);
   * }
   * ```
   */
  getName(): string;

  /**
   * Validates that the input context contains all required fields.
   * Throws descriptive errors if validation fails.
   * 
   * **Validation Behavior:**
   * - Checks that all required fields exist (using context.has())
   * - Does not validate field types or values (only existence)
   * - Throws Error with details about missing fields
   * 
   * **Best Practices:**
   * - Call this at the beginning of your call() method
   * - Include all fields your link actually uses
   * - Consider creating custom validation for type/value checking
   * 
   * @param ctx The context to validate
   * @param requiredFields Array of field names that must exist in the context
   * @throws {Error} If any required fields are missing
   * 
   * @example
   * ```typescript
   * class ProcessUserDataLink extends Link<UserInput, UserProcessed> {
   *   async call(ctx: Context<UserInput>): Promise<Context<UserProcessed>> {
   *     // Validate required fields exist
   *     this.validateContext(ctx, ['name', 'email', 'age']);
   *     
   *     // Now safe to access these fields
   *     const name = ctx.get('name');
   *     const email = ctx.get('email');
   *     const age = ctx.get('age');
   *     
   *     // Additional type validation if needed
   *     if (typeof age !== 'number') {
   *       throw new Error('Age must be a number');
   *     }
   *     
   *     // Processing logic
   *     return ctx.insertAs('processed', true);
   *   }
   * }
   * 
   * // Error handling example
   * try {
   *   const incompleteCtx = new Context({ name: 'Alice' }); // missing email and age
   *   await new ProcessUserDataLink().call(incompleteCtx);
   * } catch (error) {
   *   console.error(error.message); // "Missing required fields: email, age"
   * }
   * ```
   */
  validateContext(ctx: Context<TInput>, requiredFields?: string[]): void;
}

/**
 * Chain: The Orchestrating Conductor
 * 
 * Manages the execution flow of multiple Links in sequence or conditionally.
 * Provides error handling, middleware support, and conditional branching.
 * Enhanced with opt-in generic typing for end-to-end type safety.
 * 
 * **Execution Models:**
 * - Linear: Links execute in sequence (default)
 * - Conditional: Links execute based on runtime conditions
 * - Parallel: Links can be composed for parallel execution patterns
 * 
 * **Generic Type Parameters:**
 * - `TInput`: The initial input context type for the chain
 * - `TOutput`: The final output context type after all processing
 * - Intermediate types are handled automatically through Link type evolution
 * 
 * **Error Handling:**
 * - Global error handlers can be registered
 * - Errors include context about which Link failed
 * - Middleware can intercept and handle errors
 * - Chain execution stops on first unhandled error
 * 
 * **Performance Characteristics:**
 * - Async execution with proper error propagation
 * - Middleware overhead is minimal (function call + await)
 * - Context passing is efficient through immutable references
 * - Memory usage scales linearly with chain length
 * 
 * @template TInput The initial input context type for the chain
 * @template TOutput The final output context type after all processing
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Type-safe chain composition
 * interface UserInput { name: string; email: string; }
 * interface UserValidated extends UserInput { isValid: boolean; }
 * interface UserProcessed extends UserValidated { id: string; createdAt: Date; }
 * 
 * const userProcessingChain = new Chain<UserInput, UserProcessed>()
 *   .addLink(new ValidateUserLink(), 'validate')
 *   .addLink(new CreateUserLink(), 'create')
 *   .addLink(new SendWelcomeEmailLink(), 'welcome')
 *   .onError((error, ctx, linkName) => {
 *     console.error(`Failed at ${linkName}:`, error.message);
 *     // Could return recovery context or re-throw
 *   });
 * 
 * // Usage
 * const inputCtx = new Context<UserInput>({ name: 'Alice', email: 'alice@example.com' });
 * const resultCtx = await userProcessingChain.run(inputCtx);
 * 
 * // Mixed typed/untyped usage
 * const flexibleChain = new Chain<any, any>()
 *   .addLink(new FlexibleProcessingLink())
 *   .addLink(new TypedValidationLink()) // Can mix typed and untyped links
 *   .addLink(new AnotherFlexibleLink());
 * 
 * // Conditional execution
 * const conditionalChain = new Chain<UserInput, UserProcessed>()
 *   .addLink(new ValidateUserLink(), 'validate')
 *   .addLink(new CheckPremiumStatusLink(), 'premium-check')
 *   .addLink(new PremiumProcessingLink(), 'premium-processing')
 *   .connect('premium-check', 'premium-processing', (ctx) => ctx.get('isPremium'))
 *   .addLink(new StandardProcessingLink(), 'standard-processing')
 *   .connect('premium-check', 'standard-processing', (ctx) => !ctx.get('isPremium'));
 * ```
 */
export declare class Chain<TInput = any, TOutput = any> {
  /**
   * Creates a new empty Chain ready for Link composition.
   * 
   * @example
   * ```typescript
   * const chain = new Chain<UserInput, UserOutput>();
   * 
   * // Type inference example
   * const inferredChain = new Chain(); // Chain<any, any>
   * ```
   */
  constructor();

  /**
   * Adds a Link to the chain with an optional name for identification.
   * Links are executed in the order they are added (unless conditional connections are used).
   * 
   * **Type Safety:**
   * - The chain maintains type continuity through Link type parameters
   * - Intermediate type transformations are handled automatically
   * - Compile-time checking ensures compatible Link compositions
   * 
   * **Naming:**
   * - Names are used for error reporting and conditional connections
   * - If no name provided, uses Link.getName() or a generated name
   * - Names should be unique within a chain for clarity
   * 
   * @param link The Link instance to add to the chain
   * @param name Optional name for the link (for error reporting and connections)
   * @returns This chain instance for method chaining
   * 
   * @example
   * ```typescript
   * // Basic link addition
   * const chain = new Chain<UserInput, UserOutput>()
   *   .addLink(new ValidateUserLink())
   *   .addLink(new ProcessUserLink())
   *   .addLink(new SaveUserLink());
   * 
   * // Named links for better error reporting
   * const namedChain = new Chain<UserInput, UserOutput>()
   *   .addLink(new ValidateUserLink(), 'validation')
   *   .addLink(new ProcessUserLink(), 'processing')
   *   .addLink(new SaveUserLink(), 'persistence');
   * 
   * // Type evolution through chain
   * interface Step1 { raw: string; }
   * interface Step2 extends Step1 { parsed: object; }
   * interface Step3 extends Step2 { validated: boolean; }
   * 
   * const typedChain = new Chain<Step1, Step3>()
   *   .addLink(new ParseLink()) // Step1 -> Step2
   *   .addLink(new ValidateLink()); // Step2 -> Step3
   * ```
   */
  addLink(link: Link<TInput, TOutput>, name?: string): Chain<TInput, TOutput>;

  /**
   * Creates a conditional connection between two named links in the chain.
   * Allows for branching execution based on runtime context values.
   * 
   * **Execution Flow:**
   * - After source link executes, condition function is evaluated
   * - If condition returns true, target link executes
   * - If condition returns false, target link is skipped
   * - Multiple conditions can be connected from the same source
   * 
   * **Condition Function:**
   * - Receives the current context after source link execution
   * - Should return boolean to determine if target should execute
   * - Should be pure function with no side effects
   * - Can access any data in the context for decision making
   * 
   * @param source Name of the source link (must be already added)
   * @param target Name of the target link (must be already added)
   * @param condition Function that determines if target should execute
   * @returns This chain instance for method chaining
   * 
   * @example
   * ```typescript
   * // Conditional processing based on user type
   * const userChain = new Chain<UserInput, UserOutput>()
   *   .addLink(new ValidateUserLink(), 'validate')
   *   .addLink(new CheckUserTypeLink(), 'check-type')
   *   .addLink(new AdminProcessingLink(), 'admin-process')
   *   .addLink(new StandardProcessingLink(), 'standard-process')
   *   .addLink(new FinalizeLink(), 'finalize')
   * 
   *   // Conditional connections
   *   .connect('check-type', 'admin-process', (ctx) => ctx.get('userType') === 'admin')
   *   .connect('check-type', 'standard-process', (ctx) => ctx.get('userType') === 'standard')
   *   .connect('admin-process', 'finalize', () => true)
   *   .connect('standard-process', 'finalize', () => true);
   * 
   * // Complex conditions
   * const complexChain = new Chain()
   *   .addLink(new DataAnalysisLink(), 'analyze')
   *   .addLink(new HighVolumeProcessingLink(), 'high-volume')
   *   .addLink(new StandardProcessingLink(), 'standard')
   *   .connect('analyze', 'high-volume', (ctx) => {
   *     const volume = ctx.get('dataVolume');
   *     const complexity = ctx.get('complexity');
   *     return volume > 1000 && complexity > 0.8;
   *   })
   *   .connect('analyze', 'standard', (ctx) => {
   *     const volume = ctx.get('dataVolume');
   *     return volume <= 1000;
   *   });
   * ```
   */
  connect(source: string, target: string, condition?: (ctx: Context<TInput>) => boolean): Chain<TInput, TOutput>;

  /**
   * Adds middleware to the chain that will be applied to all link executions.
   * Middleware can intercept before/after link execution and handle errors.
   * 
   * **Middleware Execution Order:**
   * - Multiple middleware execute in the order they are added
   * - before() methods execute before each link
   * - after() methods execute after successful link execution
   * - onError() methods execute if a link throws an error
   * 
   * **Use Cases:**
   * - Logging and monitoring
   * - Performance timing
   * - Input/output validation
   * - Caching and memoization
   * - Error transformation and recovery
   * 
   * @param middleware The middleware instance to add
   * @returns This chain instance for method chaining
   * 
   * @example
   * ```typescript
   * // Adding built-in middleware
   * const chain = new Chain<UserInput, UserOutput>()
   *   .useMiddleware(new LoggingMiddleware())
   *   .useMiddleware(new TimingMiddleware())
   *   .useMiddleware(new ValidationMiddleware())
   *   .addLink(new ProcessUserLink());
   * 
   * // Custom middleware
   * class CachingMiddleware extends Middleware {
   *   private cache = new Map();
   * 
   *   async before(link: Link, ctx: Context, linkName: string): Promise<void> {
   *     const cacheKey = this.generateCacheKey(ctx, linkName);
   *     const cached = this.cache.get(cacheKey);
   *     if (cached) {
   *       // Skip link execution if cached result exists
   *       throw new CacheHitException(cached);
   *     }
   *   }
   * 
   *   async after(link: Link, ctx: Context, linkName: string): Promise<void> {
   *     const cacheKey = this.generateCacheKey(ctx, linkName);
   *     this.cache.set(cacheKey, ctx.toObject());
   *   }
   * }
   * 
   * const cachedChain = chain.useMiddleware(new CachingMiddleware());
   * ```
   */
  useMiddleware(middleware: Middleware): Chain<TInput, TOutput>;

  /**
   * Registers a global error handler for the chain.
   * Called when any link in the chain throws an unhandled error.
   * 
   * **Error Handler Capabilities:**
   * - Receive the error, context, and link name that failed
   * - Can log errors, send notifications, or perform cleanup
   * - Can return a recovery context to continue execution
   * - Can re-throw the error to stop chain execution
   * - Can transform errors for better error reporting
   * 
   * **Error Handler Behavior:**
   * - If handler returns a Context, chain continues with that context
   * - If handler throws or returns nothing, chain execution stops
   * - Handler receives context state at the time of the error
   * - Multiple error handlers can be registered (execute in order)
   * 
   * @param handler Function to handle errors during chain execution
   * @returns This chain instance for method chaining
   * 
   * @example
   * ```typescript
   * // Basic error logging
   * const chain = new Chain<UserInput, UserOutput>()
   *   .addLink(new RiskyProcessingLink())
   *   .onError((error, ctx, linkName) => {
   *     console.error(`Error in ${linkName}:`, error.message);
   *     console.error('Context at error:', ctx.toObject());
   *     // Re-throw to stop execution
   *     throw error;
   *   });
   * 
   * // Error recovery
   * const resilientChain = new Chain<UserInput, UserOutput>()
   *   .addLink(new NetworkDependentLink())
   *   .onError((error, ctx, linkName) => {
   *     if (error.name === 'NetworkError' && linkName === 'network-call') {
   *       // Provide fallback data
   *       return ctx.insert('networkData', 'fallback-value')
   *                 .insert('usingFallback', true);
   *     }
   *     throw error; // Re-throw other errors
   *   });
   * 
   * // Error transformation and monitoring
   * const monitoredChain = new Chain<UserInput, UserOutput>()
   *   .addLink(new CriticalProcessingLink())
   *   .onError((error, ctx, linkName) => {
   *     // Send to monitoring service
   *     errorMonitoringService.recordError({
   *       error: error.message,
   *       linkName,
   *       context: ctx.toObject(),
   *       timestamp: new Date()
   *     });
   * 
   *     // Transform error for user-friendly messages
   *     if (error.name === 'ValidationError') {
   *       throw new Error('Invalid input data provided');
   *     }
   *     
   *     throw error;
   *   });
   * ```
   */
  onError(handler: (err: Error, ctx: Context<TInput>, linkName: string) => any): Chain<TInput, TOutput>;

  /**
   * Executes the chain with the provided initial context.
   * Links execute in sequence (or according to conditional connections).
   * 
   * **Execution Flow:**
   * 1. Middleware before() methods execute
   * 2. Link.call() executes
   * 3. Middleware after() methods execute
   * 4. Process moves to next link or conditional target
   * 5. On error: middleware onError() and chain error handlers execute
   * 
   * **Type Safety:**
   * - Input context must match TInput type
   * - Returns Promise<Context<TOutput>> matching chain's output type
   * - Type checking ensures input/output compatibility
   * 
   * **Error Handling:**
   * - First unhandled error stops chain execution
   * - Error handlers can provide recovery contexts
   * - All errors include context about failed link
   * - Original stack traces are preserved
   * 
   * @param initialCtx The initial context to process through the chain
   * @returns Promise resolving to the final processed context
   * @throws {Error} If any link fails and no error handler provides recovery
   * 
   * @example
   * ```typescript
   * // Basic usage
   * const chain = new Chain<UserInput, UserOutput>()
   *   .addLink(new ValidateUserLink())
   *   .addLink(new ProcessUserLink());
   * 
   * const inputCtx = new Context<UserInput>({ name: 'Alice', email: 'alice@example.com' });
   * 
   * try {
   *   const resultCtx = await chain.run(inputCtx);
   *   console.log('Processing complete:', resultCtx.toObject());
   * } catch (error) {
   *   console.error('Chain execution failed:', error.message);
   * }
   * 
   * // Conditional execution
   * const conditionalChain = new Chain<DataInput, ProcessedData>()
   *   .addLink(new AnalyzeDataLink(), 'analyze')
   *   .addLink(new FastProcessLink(), 'fast')
   *   .addLink(new SlowProcessLink(), 'slow')
   *   .connect('analyze', 'fast', (ctx) => ctx.get('size') < 1000)
   *   .connect('analyze', 'slow', (ctx) => ctx.get('size') >= 1000);
   * 
   * const dataCtx = new Context<DataInput>({ data: largeDataset });
   * const processedCtx = await conditionalChain.run(dataCtx);
   * 
   * // Performance monitoring
   * const timedChain = chain.useMiddleware(new TimingMiddleware());
   * const start = performance.now();
   * const result = await timedChain.run(inputCtx);
   * const duration = performance.now() - start;
   * console.log(`Chain executed in ${duration}ms`);
   * ```
   */
  run(initialCtx: Context<TInput>): Promise<Context<TOutput>>;

  /**
   * Creates a linear chain from a sequence of Links.
   * Convenience method for simple sequential processing without conditional branching.
   * 
   * **Usage Patterns:**
   * - Quick chain creation for simple linear workflows
   * - Functional composition style programming
   * - Prototyping and testing chain concepts
   * - When you don't need conditional branching or complex error handling
   * 
   * **Limitations:**
   * - No conditional connections
   * - No custom error handling (uses default behavior)
   * - No middleware (must be added separately)
   * - All links execute in strict sequence
   * 
   * @param links Array of Link instances to execute in sequence
   * @returns A new Chain configured for linear execution
   * 
   * @example
   * ```typescript
   * // Quick linear chain creation
   * const quickChain = Chain.createLinear<UserInput, UserOutput>(
   *   new ValidateUserLink(),
   *   new ProcessUserLink(),
   *   new SaveUserLink()
   * );
   * 
   * // Equivalent to:
   * const manualChain = new Chain<UserInput, UserOutput>()
   *   .addLink(new ValidateUserLink())
   *   .addLink(new ProcessUserLink())
   *   .addLink(new SaveUserLink());
   * 
   * // Functional style composition
   * const pipeline = Chain.createLinear(
   *   new ParseDataLink(),
   *   new ValidateDataLink(),
   *   new TransformDataLink(),
   *   new SaveDataLink()
   * );
   * 
   * const result = await pipeline.run(inputContext);
   * 
   * // Adding middleware to static chain
   * const enhancedPipeline = pipeline
   *   .useMiddleware(new LoggingMiddleware())
   *   .onError((error, ctx, linkName) => {
   *     console.error(`Pipeline failed at ${linkName}:`, error.message);
   *     throw error;
   *   });
   * ```
   */
  static createLinear<TInput = any, TOutput = any>(...links: Link<any, any>[]): Chain<TInput, TOutput>;
}

/**
 * Middleware: The Compassionate Interceptor
 * 
 * Base class for implementing middleware that can intercept and enhance
 * Link execution within Chains. Provides hooks for before/after processing
 * and error handling with agape compassion.
 * 
 * **Middleware Lifecycle:**
 * 1. before() - Called before each Link execution
 * 2. Link.call() - The actual link processing
 * 3. after() - Called after successful Link execution
 * 4. onError() - Called if Link throws an error
 * 
 * **Use Cases:**
 * - Logging and monitoring
 * - Performance timing and profiling
 * - Input/output validation
 * - Caching and memoization
 * - Error handling and recovery
 * - Request tracing and debugging
 * - Rate limiting and throttling
 * 
 * **Implementation Guidelines:**
 * - Keep middleware lightweight and focused
 * - Avoid side effects that could break chain execution
 * - Handle errors gracefully in middleware methods
 * - Document any performance impact
 * - Consider async operations carefully
 * 
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Custom monitoring middleware
 * class MonitoringMiddleware extends Middleware {
 *   private metrics = new Map<string, number>();
 * 
 *   async before(link: Link, ctx: Context, linkName: string): Promise<void> {
 *     console.log(`Starting ${linkName} with context:`, ctx.keys());
 *     this.metrics.set(`${linkName}_start`, Date.now());
 *   }
 * 
 *   async after(link: Link, ctx: Context, linkName: string): Promise<void> {
 *     const startTime = this.metrics.get(`${linkName}_start`);
 *     const duration = Date.now() - startTime;
 *     console.log(`Completed ${linkName} in ${duration}ms`);
 *     
 *     // Send metrics to monitoring service
 *     await this.sendMetrics(linkName, duration, ctx.keys().length);
 *   }
 * 
 *   async onError(link: Link, error: Error, ctx: Context, linkName: string): Promise<void> {
 *     console.error(`Error in ${linkName}:`, error.message);
 *     await this.sendErrorMetrics(linkName, error.name, ctx.keys().length);
 *   }
 * 
 *   private async sendMetrics(linkName: string, duration: number, contextSize: number) {
 *     // Send to external monitoring service
 *   }
 * 
 *   private async sendErrorMetrics(linkName: string, errorType: string, contextSize: number) {
 *     // Send error metrics to monitoring service
 *   }
 * }
 * 
 * // Usage in chain
 * const monitoredChain = new Chain<UserInput, UserOutput>()
 *   .useMiddleware(new MonitoringMiddleware())
 *   .useMiddleware(new LoggingMiddleware())
 *   .addLink(new ProcessUserLink());
 * ```
 */
export declare class Middleware {
  /**
   * Called before each Link execution in the chain.
   * Can be used for setup, validation, logging, or preprocessing.
   * 
   * **Execution Context:**
   * - Called with the context that will be passed to the Link
   * - Cannot modify the context (it's immutable)
   * - Can perform side effects like logging or metrics collection
   * - Should not throw errors unless you want to stop chain execution
   * 
   * **Performance Considerations:**
   * - Keep this method fast as it's called for every Link
   * - Avoid heavy I/O operations unless necessary
   * - Consider using async sparingly to avoid blocking
   * 
   * @param link The Link instance that is about to execute
   * @param ctx The context that will be passed to the Link
   * @param linkName The name of the Link (for identification)
   * @returns Promise<void> or void
   * 
   * @example
   * ```typescript
   * class PreprocessingMiddleware extends Middleware {
   *   async before(link: Link, ctx: Context, linkName: string): Promise<void> {
   *     // Log the incoming request
   *     console.log(`Processing ${linkName}:`, {
   *       contextKeys: ctx.keys(),
   *       timestamp: new Date().toISOString()
   *     });
   * 
   *     // Validate context before processing
   *     if (linkName === 'critical-process' && !ctx.has('requiredField')) {
   *       throw new Error('Critical process requires requiredField');
   *     }
   * 
   *     // Setup for Link execution
   *     await this.setupResources(linkName);
   *   }
   * 
   *   private async setupResources(linkName: string): Promise<void> {
   *     // Prepare any resources the Link might need
   *   }
   * }
   * ```
   */
  before?(link: Link, ctx: Context, linkName: string): Promise<void> | void;

  /**
   * Called after successful Link execution.
   * Can be used for cleanup, logging, postprocessing, or metrics collection.
   * 
   * **Execution Context:**
   * - Called with the context returned by the Link
   * - Link has successfully completed without throwing errors
   * - Cannot modify the context (it's immutable)
   * - Can perform side effects like logging or cleanup
   * 
   * **Use Cases:**
   * - Performance timing and metrics
   * - Success logging and monitoring
   * - Cleanup of resources allocated in before()
   * - Caching successful results
   * - Triggering downstream notifications
   * 
   * @param link The Link instance that just executed successfully
   * @param ctx The context returned by the Link
   * @param linkName The name of the Link (for identification)
   * @returns Promise<void> or void
   * 
   * @example
   * ```typescript
   * class CachingMiddleware extends Middleware {
   *   private cache = new Map<string, any>();
   * 
   *   async after(link: Link, ctx: Context, linkName: string): Promise<void> {
   *     // Cache successful results
   *     const cacheKey = this.generateCacheKey(linkName, ctx);
   *     this.cache.set(cacheKey, ctx.toObject());
   * 
   *     // Log successful execution
   *     console.log(`Successfully cached result for ${linkName}`);
   * 
   *     // Cleanup old cache entries
   *     if (this.cache.size > 1000) {
   *       await this.cleanupOldEntries();
   *     }
   *   }
   * 
   *   private generateCacheKey(linkName: string, ctx: Context): string {
   *     return `${linkName}_${JSON.stringify(ctx.toObject())}`;
   *   }
   * 
   *   private async cleanupOldEntries(): Promise<void> {
   *     // Remove old cache entries
   *   }
   * }
   * ```
   */
  after?(link: Link, ctx: Context, linkName: string): Promise<void> | void;

  /**
   * Called when a Link throws an error during execution.
   * Can be used for error logging, recovery, cleanup, or error transformation.
   * 
   * **Error Handling:**
   * - Receives the original error thrown by the Link
   * - Gets the context that was passed to the Link (before error)
   * - Cannot modify the context or error (for transparency)
   * - Should not throw unless you want to replace the original error
   * 
   * **Recovery Options:**
   * - Log and re-throw the error (most common)
   * - Perform cleanup and re-throw
   * - Transform the error for better messaging
   * - Generally should not swallow errors silently
   * 
   * @param link The Link instance that threw the error
   * @param error The error that was thrown
   * @param ctx The context that was passed to the Link
   * @param linkName The name of the Link (for identification)
   * @returns Promise<void> or void
   * 
   * @example
   * ```typescript
   * class ErrorHandlingMiddleware extends Middleware {
   *   async onError(link: Link, error: Error, ctx: Context, linkName: string): Promise<void> {
   *     // Log detailed error information
   *     console.error(`Error in ${linkName}:`, {
   *       error: error.message,
   *       stack: error.stack,
   *       context: ctx.toObject(),
   *       timestamp: new Date().toISOString()
   *     });
   * 
   *     // Send to error tracking service
   *     await this.sendErrorToTracking({
   *       linkName,
   *       error: error.message,
   *       contextKeys: ctx.keys(),
   *       userAgent: ctx.get('userAgent'),
   *       userId: ctx.get('userId')
   *     });
   * 
   *     // Cleanup any resources that were allocated in before()
   *     await this.cleanupResources(linkName);
   * 
   *     // Transform error for better user experience
   *     if (error.name === 'ValidationError') {
   *       throw new Error('Invalid input data provided. Please check your input and try again.');
   *     }
   * 
   *     // Re-throw original error to maintain transparency
   *     throw error;
   *   }
   * 
   *   private async sendErrorToTracking(errorData: any): Promise<void> {
   *     // Send to external error tracking service
   *   }
   * 
   *   private async cleanupResources(linkName: string): Promise<void> {
   *     // Cleanup any resources allocated for this link
   *   }
   * }
   * ```
   */
  onError?(link: Link, error: Error, ctx: Context, linkName: string): Promise<void> | void;
}

/**
 * LoggingMiddleware: The Compassionate Observer
 * 
 * Built-in middleware that provides comprehensive logging for Chain execution.
 * Logs Link start/completion, execution times, context changes, and errors
 * with configurable detail levels.
 * 
 * **Logging Features:**
 * - Link execution start/completion
 * - Execution timing information
 * - Context key changes between Links
 * - Error details and stack traces
 * - Configurable log levels
 * 
 * **Performance Impact:**
 * - Minimal overhead for basic logging
 * - Context diffing adds small overhead
 * - Can be configured for production use
 * - JSON serialization only when needed
 * 
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Basic usage
 * const chain = new Chain<UserInput, UserOutput>()
 *   .useMiddleware(new LoggingMiddleware())
 *   .addLink(new ValidateUserLink())
 *   .addLink(new ProcessUserLink());
 * 
 * // Will output:
 * // [INFO] Starting ValidateUserLink with 2 context keys
 * // [INFO] Completed ValidateUserLink in 15ms (3 context keys)
 * // [INFO] Starting ProcessUserLink with 3 context keys  
 * // [INFO] Completed ProcessUserLink in 42ms (5 context keys)
 * 
 * // Error logging
 * try {
 *   await chain.run(inputContext);
 * } catch (error) {
 *   // [ERROR] Error in ProcessUserLink: Validation failed
 *   // [ERROR] Context at error: {...}
 * }
 * ```
 */
export declare class LoggingMiddleware extends Middleware {}

/**
 * TimingMiddleware: The Performance Guardian
 * 
 * Built-in middleware that measures and logs execution times for each Link.
 * Provides detailed performance metrics and can identify bottlenecks in chains.
 * 
 * **Timing Features:**
 * - Individual Link execution times
 * - Total chain execution time
 * - Performance warnings for slow Links
 * - Configurable timing thresholds
 * - Memory usage tracking (if available)
 * 
 * **Metrics Collected:**
 * - Start/end timestamps
 * - Execution duration in milliseconds
 * - Context size changes
 * - Performance warnings
 * 
 * **Performance Impact:**
 * - Negligible overhead (< 1ms per Link)
 * - Uses high-resolution timing when available
 * - Safe for production use
 * 
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Basic performance monitoring
 * const chain = new Chain<DataInput, ProcessedData>()
 *   .useMiddleware(new TimingMiddleware())
 *   .addLink(new LoadDataLink())      // 125ms
 *   .addLink(new ProcessDataLink())   // 2.3s ⚠️ slow
 *   .addLink(new SaveDataLink());     // 89ms
 * 
 * // Will output:
 * // [TIMING] LoadDataLink: 125ms
 * // [TIMING] ProcessDataLink: 2345ms ⚠️ 
 * // [TIMING] SaveDataLink: 89ms
 * // [TIMING] Total chain execution: 2559ms
 * 
 * // Combined with other middleware
 * const monitoredChain = new Chain<UserInput, UserOutput>()
 *   .useMiddleware(new LoggingMiddleware())
 *   .useMiddleware(new TimingMiddleware())
 *   .addLink(new ExpensiveProcessingLink());
 * 
 * // Performance analysis
 * const start = performance.now();
 * await monitoredChain.run(inputContext);
 * const total = performance.now() - start;
 * ```
 */
export declare class TimingMiddleware extends Middleware {}

/**
 * ValidationMiddleware: The Protective Guardian
 * 
 * Built-in middleware that validates contexts before and after Link execution.
 * Ensures data integrity and catches common issues early in the chain.
 * 
 * **Validation Features:**
 * - Pre-execution context validation
 * - Post-execution result validation
 * - Required field checking
 * - Type validation (basic)
 * - Custom validation rules
 * 
 * **Validation Rules:**
 * - Context must not be null/undefined
 * - Required fields must exist
 * - Data types match expectations
 * - Custom business rules
 * 
 * **Error Handling:**
 * - Throws descriptive validation errors
 * - Includes details about what failed
 * - Preserves original error stack traces
 * - Provides suggestions for fixing issues
 * 
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Basic validation
 * const chain = new Chain<UserInput, UserOutput>()
 *   .useMiddleware(new ValidationMiddleware())
 *   .addLink(new ProcessUserLink());
 * 
 * // Will validate:
 * // - Context is not null/undefined
 * // - Context has required methods
 * // - Link returns valid Context
 * 
 * // Custom validation with required fields
 * class CustomValidationLink extends Link<UserInput, UserValidated> {
 *   async call(ctx: Context<UserInput>): Promise<Context<UserValidated>> {
 *     this.validateContext(ctx, ['name', 'email']); // Built-in validation
 *     // Additional custom validation here
 *     return ctx.insertAs('validated', true);
 *   }
 * }
 * 
 * // Validation errors provide clear messages:
 * // ValidationError: Missing required fields: email
 * // ValidationError: Context must be a valid Context instance
 * // ValidationError: Link must return a Context instance
 * ```
 */
export declare class ValidationMiddleware extends Middleware {}

/**
 * Package version string.
 * Follows semantic versioning (major.minor.patch).
 * 
 * @example
 * ```typescript
 * import { version } from 'codeuchain';
 * console.log(`Using CodeUChain v${version}`);
 * ```
 */
export declare const version: string;

/**
 * Default export type definition for CommonJS and ES module compatibility.
 * Provides access to all main classes and the version string.
 * 
 * **Usage Patterns:**
 * - CommonJS: `const CodeUChain = require('codeuchain');`
 * - ES Modules: `import CodeUChain from 'codeuchain';`
 * - Named imports: `import { Context, Chain, Link } from 'codeuchain';`
 * - Mixed: `import CodeUChain, { Context } from 'codeuchain';`
 * 
 * @example
 * ```typescript
 * // CommonJS usage
 * const CodeUChain = require('codeuchain');
 * const ctx = new CodeUChain.Context({ data: 'value' });
 * const chain = new CodeUChain.Chain();
 * 
 * // ES Module default import
 * import CodeUChain from 'codeuchain';
 * const ctx = new CodeUChain.Context({ data: 'value' });
 * 
 * // ES Module named imports (preferred)
 * import { Context, Chain, Link, LoggingMiddleware } from 'codeuchain';
 * const ctx = new Context({ data: 'value' });
 * const chain = new Chain();
 * 
 * // Mixed usage
 * import CodeUChain, { Context } from 'codeuchain';
 * console.log(`CodeUChain v${CodeUChain.version}`);
 * const ctx = new Context({ data: 'value' });
 * ```
 */
export type DefaultExport = {
  Context: typeof Context;
  MutableContext: typeof MutableContext;
  Link: typeof Link;
  Chain: typeof Chain;
  Middleware: typeof Middleware;
  LoggingMiddleware: typeof LoggingMiddleware;
  TimingMiddleware: typeof TimingMiddleware;
  ValidationMiddleware: typeof ValidationMiddleware;
  version: string;
};

/**
 * Default export providing all CodeUChain classes and utilities.
 * Supports both CommonJS require() and ES module import patterns.
 * 
 * @example
 * ```typescript
 * // TypeScript with default import
 * import CodeUChain from 'codeuchain';
 * const ctx = new CodeUChain.Context<MyDataType>({ id: 1, name: 'Alice' });
 * 
 * // JavaScript with require
 * const CodeUChain = require('codeuchain');
 * const ctx = new CodeUChain.Context({ id: 1, name: 'Alice' });
 * ```
 */
declare const _default: DefaultExport;
export default _default;
