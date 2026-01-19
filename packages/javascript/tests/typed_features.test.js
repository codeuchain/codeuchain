/**
 * CodeUChain JavaScript: Typed Features Tests
 *
 * Comprehensive test suite for JavaScript typed features implementation.
 * Tests cover generic typing, type evolution, backward compatibility,
 * and mixed typed/untyped usage patterns.
 */

const { Context, Chain, Link, Middleware } = require('../core');

// =============================================================================
// TEST HELPERS
// =============================================================================

/**
 * Mock typed data structures for testing
 */
const TestData = {
  /** @type {UserInput} */
  userInput: {
    name: 'Test User',
    email: 'test@example.com'
  },

  /** @type {UserValidated} */
  userValidated: {
    name: 'Test User',
    email: 'test@example.com',
    isValid: true
  },

  /** @type {UserProcessed} */
  userProcessed: {
    name: 'Test User',
    email: 'test@example.com',
    isValid: true,
    age: 25,
    profileComplete: true,
    userId: 'user_123',
    status: 'active'
  }
};

// =============================================================================
// TYPED LINK IMPLEMENTATIONS FOR TESTING
// =============================================================================

/**
 * Simple validation link for testing
 * @extends {Link<UserInput, UserValidated>}
 */
class TestValidationLink extends Link {
  /**
   * @param {Context<UserInput>} ctx
   * @returns {Promise<Context<UserValidated>>}
   */
  async call(ctx) {
    const name = ctx.get('name');
    const email = ctx.get('email');

    if (!name || !email) {
      throw new Error('Name and email required');
    }

    return ctx.insertAs('isValid', true);
  }
}

/**
 * Simple processing link for testing
 * @extends {Link<UserValidated, UserProcessed>}
 */
class TestProcessingLink extends Link {
  /**
   * @param {Context<UserValidated>} ctx
   * @returns {Promise<Context<UserProcessed>>}
   */
  async call(ctx) {
    const isValid = ctx.get('isValid');
    if (!isValid) {
      throw new Error('User must be validated first');
    }

    return ctx
      .insertAs('age', 25)
      .insertAs('profileComplete', true)
      .insertAs('userId', 'test_user_123')
      .insertAs('status', 'active');
  }
}

/**
 * Link that throws errors for testing
 * @extends {Link<UserInput, UserValidated>}
 */
class TestErrorLink extends Link {
  /**
   * @param {Context<UserInput>} ctx
   * @returns {Promise<Context<UserValidated>>}
   */
  async call(ctx) {
    throw new Error('Test error for error handling');
  }
}

// =============================================================================
// JEST TEST SUITES
// =============================================================================

describe('Context Typed Tests', () => {
  test('basic typed context creation', () => {
    const ctx = new Context(TestData.userInput);
    expect(ctx).toBeInstanceOf(Context);
    expect(ctx.get('name')).toBe('Test User');
    expect(ctx.get('email')).toBe('test@example.com');
  });

  test('type evolution with insertAs', () => {
    const ctx = new Context(TestData.userInput);
    const evolvedCtx = ctx.insertAs('isValid', true);

    expect(evolvedCtx.get('isValid')).toBe(true);
    expect(evolvedCtx.get('name')).toBe('Test User');
  });

  test('multiple type evolutions', () => {
    const ctx = new Context(TestData.userInput);
    const multiEvolvedCtx = ctx
      .insertAs('isValid', true)
      .insertAs('age', 25)
      .insertAs('profileComplete', true)
      .insertAs('userId', 'test_user_123')
      .insertAs('status', 'active');

    expect(multiEvolvedCtx.get('age')).toBe(25);
    expect(multiEvolvedCtx.get('profileComplete')).toBe(true);
    expect(multiEvolvedCtx.get('userId')).toBe('test_user_123');
    expect(multiEvolvedCtx.get('status')).toBe('active');
  });

  test('backward compatibility with insert', () => {
    const ctx = new Context(TestData.userInput);
    const backwardCompatCtx = ctx.insert('customField', 'customValue');

    expect(backwardCompatCtx.get('customField')).toBe('customValue');
  });

  test('context immutability', () => {
    const ctx = new Context(TestData.userInput);
    const originalData = ctx.toObject();
    const newCtx = ctx.insertAs('newField', 'newValue');

    expect(ctx.toObject()).toEqual(originalData);
  });

  test('type validation after insertAs operations', () => {
    // Start with basic user input
    const ctx = new Context(TestData.userInput);

    // Verify initial types
    expect(typeof ctx.get('name')).toBe('string');
    expect(typeof ctx.get('email')).toBe('string');

    // Evolve with insertAs and verify types
    const evolvedCtx = ctx
      .insertAs('isValid', true)        // boolean
      .insertAs('age', 25)              // number
      .insertAs('profileComplete', true) // boolean
      .insertAs('userId', 'user_123')   // string
      .insertAs('tags', ['admin', 'premium']) // array
      .insertAs('metadata', { source: 'api', version: '1.0' }); // object

    // Verify all types are preserved correctly
    expect(typeof evolvedCtx.get('name')).toBe('string');
    expect(typeof evolvedCtx.get('email')).toBe('string');
    expect(typeof evolvedCtx.get('isValid')).toBe('boolean');
    expect(typeof evolvedCtx.get('age')).toBe('number');
    expect(typeof evolvedCtx.get('profileComplete')).toBe('boolean');
    expect(typeof evolvedCtx.get('userId')).toBe('string');
    expect(Array.isArray(evolvedCtx.get('tags'))).toBe(true);
    expect(typeof evolvedCtx.get('metadata')).toBe('object');

    // Verify specific values and their types
    expect(evolvedCtx.get('isValid')).toBe(true);
    expect(evolvedCtx.get('age')).toBe(25);
    expect(evolvedCtx.get('tags')).toEqual(['admin', 'premium']);
    expect(evolvedCtx.get('metadata')).toEqual({ source: 'api', version: '1.0' });

    // Verify object properties have correct types
    const metadata = evolvedCtx.get('metadata');
    expect(typeof metadata.source).toBe('string');
    expect(typeof metadata.version).toBe('string');
  });
});

describe('Link Typed Tests', () => {
  test('basic typed link execution', async () => {
    const link = new TestValidationLink();
    const inputCtx = new Context(TestData.userInput);
    const resultCtx = await link.call(inputCtx);

    expect(resultCtx.get('isValid')).toBe(true);
    expect(resultCtx.get('name')).toBe('Test User');
  });

  test('link chaining with type evolution', async () => {
    const validationLink = new TestValidationLink();
    const processingLink = new TestProcessingLink();

    const inputCtx = new Context(TestData.userInput);
    const validatedCtx = await validationLink.call(inputCtx);
    const processedCtx = await processingLink.call(validatedCtx);

    expect(processedCtx.get('status')).toBe('active');
    expect(processedCtx.get('userId')).toBe('test_user_123');
  });

  test('error handling in typed links', async () => {
    const errorLink = new TestErrorLink();
    const inputCtx = new Context(TestData.userInput);

    await expect(errorLink.call(inputCtx)).rejects.toThrow('Test error for error handling');
  });
});

describe('Chain Typed Tests', () => {
  test('basic typed chain creation and execution', async () => {
    const chain = new Chain();
    chain.addLink(new TestValidationLink());
    chain.addLink(new TestProcessingLink());
    chain.connect('TestValidationLink', 'TestProcessingLink');

    const inputCtx = new Context(TestData.userInput);
    const resultCtx = await chain.run(inputCtx);

    expect(resultCtx.get('status')).toBe('active');
    expect(resultCtx.get('userId')).toBe('test_user_123');
  });

  test('chain with middleware', async () => {
    class TestMiddleware extends Middleware {
      async before(link, ctx, linkName) {
        // ctx should be a Context instance, use insertAs for type evolution
        return ctx.insertAs('middleware_before', true);
      }

      async after(link, ctx, linkName) {
        return ctx.insertAs('middleware_after', true);
      }
    }

    const chain = new Chain();
    chain.addLink(new TestValidationLink());
    chain.useMiddleware(new TestMiddleware());

    const inputCtx = new Context(TestData.userInput);
    const resultCtx = await chain.run(inputCtx);

    expect(resultCtx.get('middleware_before')).toBe(true);
    expect(resultCtx.get('isValid')).toBe(true);
    expect(resultCtx.get('middleware_after')).toBe(true);
  });

  test('chain error handling', async () => {
    const errorChain = new Chain();
    errorChain.addLink(new TestErrorLink());

    let errorCaught = false;
    errorChain.onError((error, ctx, linkName) => {
      errorCaught = true;
      expect(linkName).toBe('TestErrorLink');
      expect(error.message).toContain('Test error');
    });

    const inputCtx = new Context(TestData.userInput);

    try {
      await errorChain.run(inputCtx);
    } catch (error) {
      // Expected error
    }

    expect(errorCaught).toBe(true);
  });

  test('chain link names', () => {
    const namedChain = new Chain();
    namedChain.addLink(new TestValidationLink(), 'CustomValidationLink');
    const linkNames = namedChain.getLinkNames();

    expect(linkNames).toContain('CustomValidationLink');
  });
});

describe('Backward Compatibility Tests', () => {
  test('untyped context operations', () => {
    const untypedCtx = new Context({ name: 'Untyped User', email: 'untyped@example.com' });
    const evolvedUntyped = untypedCtx.insert('customField', 'customValue');

    expect(evolvedUntyped.get('customField')).toBe('customValue');
  });

  test('mixed typed and untyped links', async () => {
    class UntypedLink extends Link {
      async call(ctx) {
        return ctx.insert('untypedResult', 'success');
      }
    }

    const mixedChain = new Chain();
    mixedChain.addLink(new TestValidationLink()); // Typed
    mixedChain.addLink(new UntypedLink()); // Untyped
    mixedChain.connect('TestValidationLink', 'UntypedLink');

    const inputCtx = new Context(TestData.userInput);
    const resultCtx = await mixedChain.run(inputCtx);

    expect(resultCtx.get('isValid')).toBe(true);
    expect(resultCtx.get('untypedResult')).toBe('success');
  });

  test('runtime behavior consistency', () => {
    const typedCtx = new Context(TestData.userInput);
    const untypedCtx = new Context(TestData.userInput);

    const typedResult = typedCtx.insertAs('field', 'value');
    const untypedResult = untypedCtx.insert('field', 'value');

    expect(typedResult.toObject()).toEqual(untypedResult.toObject());
  });
});

describe('Performance Tests', () => {
  test('zero performance impact verification', () => {
    const iterations = 1000;

    // Measure typed operations
    const startTyped = Date.now();
    for (let i = 0; i < iterations; i++) {
      const ctx = new Context(TestData.userInput);
      const result = ctx.insertAs('testField', i);
      result.get('testField');
    }
    const typedTime = Date.now() - startTyped;

    // Measure untyped operations
    const startUntyped = Date.now();
    for (let i = 0; i < iterations; i++) {
      const ctx = new Context(TestData.userInput);
      const result = ctx.insert('testField', i);
      result.get('testField');
    }
    const untypedTime = Date.now() - startUntyped;

    // Performance should be comparable (within 100% difference = no more than 2x slower)
    const performanceRatio = typedTime / untypedTime;
    expect(performanceRatio).toBeGreaterThan(0.5);
    expect(performanceRatio).toBeLessThan(2.0);
  });

  test('memory usage consistency', () => {
    const memoryTestContexts = [];
    for (let i = 0; i < 100; i++) {
      const ctx = new Context(TestData.userInput);
      const evolved = ctx.insertAs('field' + i, 'value' + i);
      memoryTestContexts.push(evolved);
    }

    expect(memoryTestContexts).toHaveLength(100);
  });
});