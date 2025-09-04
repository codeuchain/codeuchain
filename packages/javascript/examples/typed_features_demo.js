/**
 * CodeUChain JavaScript: Typed Features Demonstration
 *
 * This example demonstrates the opt-in typed features in JavaScript CodeUChain.
 * While JavaScript doesn't have built-in generics like TypeScript, we provide
 * JSDoc annotations and TypeScript definitions for enhanced developer experience.
 *
 * Key Features Demonstrated:
 * 1. Generic Context<T> with type evolution
 * 2. Generic Link<TInput, TOutput> interfaces
 * 3. Generic Chain<TInput, TOutput> processing
 * 4. Type-safe insertAs() method for clean transformations
 * 5. Backward compatibility with existing untyped code
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

// =============================================================================
// TYPE DEFINITIONS (Using JSDoc for TypeScript-like experience)
// =============================================================================

/**
 * @typedef {Object} UserInput
 * @property {string} name - User's full name
 * @property {string} email - User's email address
 */

/**
 * @typedef {UserInput & Object} UserValidated
 * @property {string} name - User's full name
 * @property {string} email - User's email address
 * @property {boolean} isValid - Whether the user data is valid
 */

/**
 * @typedef {UserValidated & Object} UserWithProfile
 * @property {string} name - User's full name
 * @property {string} email - User's email address
 * @property {boolean} isValid - Whether the user data is valid
 * @property {boolean} profileComplete - Whether profile is complete
 * @property {number} age - User's age
 */

/**
 * @typedef {UserWithProfile & Object} UserProcessed
 * @property {string} name - User's full name
 * @property {string} email - User's email address
 * @property {boolean} isValid - Whether the user data is valid
 * @property {boolean} profileComplete - Whether profile is complete
 * @property {number} age - User's age
 * @property {string} userId - Generated user ID
 * @property {string} status - Processing status
 */

// =============================================================================
// TYPED LINK IMPLEMENTATIONS
// =============================================================================

/**
 * Link for validating user input data
 * @extends {Link<UserInput, UserValidated>}
 */
class ValidateUserLink extends Link {
  /**
   * @param {Context<UserInput>} ctx
   * @returns {Promise<Context<UserValidated>>}
   */
  async call(ctx) {
    const name = ctx.get('name');
    const email = ctx.get('email');

    // Validation logic
    const isValid = name && email && email.includes('@') && email.includes('.');

    if (!isValid) {
      throw new Error('Invalid user data: name and valid email required');
    }

    console.log(`✅ User ${name} validated successfully`);
    // Use insertAs for type evolution
    return ctx.insertAs('isValid', true);
  }
}

/**
 * Link for processing user profile information
 * @extends {Link<UserValidated, UserWithProfile>}
 */
class ProcessProfileLink extends Link {
  /**
   * @param {Context<UserValidated>} ctx
   * @returns {Promise<Context<UserWithProfile>>}
   */
  async call(ctx) {
    const name = ctx.get('name');
    const isValid = ctx.get('isValid');

    if (!isValid) {
      throw new Error('Cannot process invalid user profile');
    }

    // Simulate profile processing
    const age = this._calculateAgeFromName(name);
    const profileComplete = age >= 18;

    console.log(`👤 Processed profile for ${name} (age: ${age})`);

    // Type evolution: UserValidated -> UserWithProfile
    return ctx
      .insertAs('age', age)
      .insertAs('profileComplete', profileComplete);
  }

  /**
   * Mock age calculation based on name length
   * @param {string} name
   * @returns {number}
   * @private
   */
  _calculateAgeFromName(name) {
    // Simple mock: age based on name length
    return 18 + (name.length % 50);
  }
}

/**
 * Link for creating user account
 * @extends {Link<UserWithProfile, UserProcessed>}
 */
class CreateUserAccountLink extends Link {
  /**
   * @param {Context<UserWithProfile>} ctx
   * @returns {Promise<Context<UserProcessed>>}
   */
  async call(ctx) {
    const name = ctx.get('name');
    const profileComplete = ctx.get('profileComplete');

    if (!profileComplete) {
      throw new Error('Cannot create account for incomplete profile');
    }

    // Simulate account creation
    const userId = `user_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    const status = 'active';

    console.log(`🎉 Created account for ${name} with ID: ${userId}`);

    // Final type evolution: UserWithProfile -> UserProcessed
    return ctx
      .insertAs('userId', userId)
      .insertAs('status', status);
  }
}

// =============================================================================
// TYPED CHAIN IMPLEMENTATIONS
// =============================================================================

/**
 * Typed user registration chain
 * @extends {Chain<UserInput, UserProcessed>}
 */
class UserRegistrationChain extends Chain {
  constructor() {
    super();

    // Add typed links with automatic naming
    this.addLink(new ValidateUserLink());
    this.addLink(new ProcessProfileLink());
    this.addLink(new CreateUserAccountLink());

    // Connect links in sequence
    this.connect('ValidateUserLink', 'ProcessProfileLink');
    this.connect('ProcessProfileLink', 'CreateUserAccountLink');

    // Add middleware
    this.useMiddleware(new LoggingMiddleware());
  }

  /**
   * Register a new user with full type safety
   * @param {Context<UserInput>} initialCtx
   * @returns {Promise<Context<UserProcessed>>}
   */
  async registerUser(initialCtx) {
    return await this.run(initialCtx);
  }
}

// =============================================================================
// DEMONSTRATION FUNCTIONS
// =============================================================================

/**
 * Demonstrate basic typed context operations
 */
function demonstrateTypedContext() {
  console.log('=== TYPED CONTEXT OPERATIONS ===\n');

  // Create typed context
  /** @type {UserInput} */
  const userData = {
    name: 'Alice Johnson',
    email: 'alice@example.com'
  };

  const ctx = new Context(userData);

  console.log('1. Initial context:');
  console.log('   Type: UserInput');
  console.log('   Data:', ctx.toObject());
  console.log();

  // Type evolution with insertAs
  console.log('2. After validation (type evolution):');
  const validatedCtx = ctx.insertAs('isValid', true);
  console.log('   Type: UserValidated');
  console.log('   Data:', validatedCtx.toObject());
  console.log();

  // Further evolution
  console.log('3. After profile processing (further evolution):');
  const profileCtx = validatedCtx
    .insertAs('age', 28)
    .insertAs('profileComplete', true);
  console.log('   Type: UserWithProfile');
  console.log('   Data:', profileCtx.toObject());
  console.log();
}

/**
 * Demonstrate typed chain processing
 */
async function demonstrateTypedChain() {
  console.log('=== TYPED CHAIN PROCESSING ===\n');

  const chain = new UserRegistrationChain();

  // Test data
  /** @type {UserInput} */
  const testUsers = [
    { name: 'Alice Johnson', email: 'alice@example.com' },
    { name: 'Bob Smith', email: 'bob@example.com' },
    { name: 'Charlie Brown', email: 'invalid-email' }, // This will fail
  ];

  for (const user of testUsers) {
    console.log(`\n📝 Processing user: ${user.name}`);

    try {
      const initialCtx = new Context(user);
      const resultCtx = await chain.registerUser(initialCtx);

      console.log('✅ Registration completed successfully!');
      console.log('📊 Final result:', resultCtx.toObject());

    } catch (error) {
      console.log('❌ Registration failed:', error.message);
    }

    console.log('─'.repeat(60));
  }
}

/**
 * Demonstrate backward compatibility
 */
async function demonstrateBackwardCompatibility() {
  console.log('=== BACKWARD COMPATIBILITY ===\n');

  // Untyped usage still works
  const untypedCtx = new Context({ name: 'Dave Wilson', email: 'dave@example.com' });
  const evolvedCtx = untypedCtx.insert('customField', 'customValue');

  console.log('1. Untyped context operations:');
  console.log('   Original:', untypedCtx.toObject());
  console.log('   Evolved:', evolvedCtx.toObject());
  console.log();

  // Mixed typed/untyped chains
  console.log('2. Mixed typed and untyped links:');

  class SimpleLoggerLink extends Link {
    async call(ctx) {
      const name = ctx.get('name');
      console.log(`📝 Processing ${name} in untyped link`);
      return ctx.insert('logged', true);
    }
  }

  const mixedChain = new Chain();
  mixedChain.addLink(new ValidateUserLink()); // Typed link
  mixedChain.addLink(new SimpleLoggerLink()); // Untyped link

  mixedChain.connect('ValidateUserLink', 'SimpleLoggerLink');

  try {
    const result = await mixedChain.run(new Context({ name: 'Eve Davis', email: 'eve@example.com' }));
    console.log('   Mixed chain result:', result.toObject());
  } catch (error) {
    console.log('   Mixed chain error:', error.message);
  }

  console.log();
}

/**
 * Demonstrate error handling with types
 */
async function demonstrateErrorHandling() {
  console.log('=== ERROR HANDLING WITH TYPES ===\n');

  const chain = new UserRegistrationChain();

  // Add error handler
  chain.onError((error, ctx, linkName) => {
    console.error(`🚨 Error in ${linkName}: ${error.message}`);
    console.error('   Context at error:', ctx.toObject());
  });

  // Test with invalid data
  /** @type {UserInput} */
  const invalidUser = {
    name: '', // Invalid: empty name
    email: 'invalid-email' // Invalid: bad email
  };

  console.log('Testing with invalid user data:');
  console.log('Input:', invalidUser);

  try {
    const result = await chain.run(new Context(invalidUser));
    console.log('Unexpected success:', result.toObject());
  } catch (error) {
    console.log('Expected error caught:', error.message);
  }

  console.log();
}

// =============================================================================
// MAIN DEMONSTRATION
// =============================================================================

async function main() {
  console.log('🎯 CodeUChain JavaScript: Typed Features Demonstration');
  console.log('=' * 60);
  console.log();

  console.log('This example demonstrates opt-in typed features in JavaScript:');
  console.log('• Generic Context<T> with type evolution');
  console.log('• Generic Link<TInput, TOutput> interfaces');
  console.log('• Generic Chain<TInput, TOutput> processing');
  console.log('• Type-safe insertAs() method');
  console.log('• Full backward compatibility');
  console.log();

  try {
    demonstrateTypedContext();
    await demonstrateTypedChain();
    await demonstrateBackwardCompatibility();
    await demonstrateErrorHandling();

    console.log('=== SUMMARY ===');
    console.log();
    console.log('✅ JavaScript typed features successfully demonstrated!');
    console.log();
    console.log('Key Benefits:');
    console.log('• Enhanced IDE support with JSDoc annotations');
    console.log('• TypeScript definitions for full type checking');
    console.log('• Clean type evolution with insertAs()');
    console.log('• Zero runtime performance impact');
    console.log('• 100% backward compatibility');
    console.log('• Mixed typed/untyped usage supported');
    console.log();
    console.log('The typed features are completely opt-in and enhance');
    console.log('the development experience without changing runtime behavior.');

  } catch (error) {
    console.error('❌ Demonstration failed:', error);
    process.exit(1);
  }
}

// Run the demonstration
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };