/**
 * Tests for Chain Converter
 *
 * Validates the bidirectional conversion between CodeUChain and traditional code.
 */

const { Context, Chain, Link, ChainConverter } = require('../core');

// Test Links
class AddNumbersLink extends Link {
  async call(ctx) {
    const a = ctx.get('a') || 0;
    const b = ctx.get('b') || 0;
    const result = a + b;
    return ctx.insert('result', result);
  }
}

class MultiplyLink extends Link {
  async call(ctx) {
    const result = ctx.get('result') || 0;
    const factor = ctx.get('factor') || 1;
    const multiplied = result * factor;
    return ctx.insert('result', multiplied);
  }
}

class FormatResultLink extends Link {
  async call(ctx) {
    const result = ctx.get('result');
    const formatted = `Result: ${result}`;
    return ctx.insert('formatted', formatted);
  }
}

describe('ChainConverter', () => {
  let converter;

  beforeEach(() => {
    converter = new ChainConverter();
  });

  describe('chainToCode', () => {
    test('should convert a simple chain to code', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');

      const code = converter.chainToCode(chain);

      expect(code).toContain('async function executePipeline');
      expect(code).toContain('initialData');
      expect(code).toContain('ctxData = { ...initialData }');
      expect(code).toContain('return ctxData');
      expect(code).toContain('addLink');
      expect(code).toContain('multiplyLink');
    });

    test('should use custom function name', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');

      const code = converter.chainToCode(chain, 'processData');

      expect(code).toContain('async function processData');
      expect(code).not.toContain('executePipeline');
    });

    test('should generate valid JavaScript code', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');

      const code = converter.chainToCode(chain);

      // Should not throw when parsed
      expect(() => {
        new Function(code);
      }).not.toThrow();
    });

    test('should handle multiple links in sequence', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');
      chain.addLink(new FormatResultLink(), 'format');

      const code = converter.chainToCode(chain);

      expect(code).toContain('addLink');
      expect(code).toContain('multiplyLink');
      expect(code).toContain('formatLink');

      // Verify sequencing
      const addIndex = code.indexOf('addLink');
      const multiplyIndex = code.indexOf('multiplyLink');
      const formatIndex = code.indexOf('formatLink');

      expect(addIndex).toBeLessThan(multiplyIndex);
      expect(multiplyIndex).toBeLessThan(formatIndex);
    });
  });

  describe('codeToChain', () => {
    test('should convert basic code to a chain', () => {
      const code = `
class AddNumbersLink {
  async call(ctx) {
    const a = ctx.get('a') || 0;
    const b = ctx.get('b') || 0;
    return ctx.insert('result', a + b);
  }
}

async function executePipeline(initialData) {
  let ctxData = { ...initialData };
  
  const addLink = new AddNumbersLink();
  const addCtx = new Context(ctxData);
  const addResult = await addLink.call(addCtx);
  ctxData = addResult.toObject();
  
  return ctxData;
}
`;

      const chain = converter.codeToChain(code);

      expect(chain._links.size).toBeGreaterThan(0);
      expect(chain._links.has('add')).toBe(true);
    });

    test('should handle multiple links', () => {
      const code = `
class AddNumbersLink {
  async call(ctx) {
    return ctx.insert('result', 42);
  }
}

class MultiplyLink {
  async call(ctx) {
    return ctx.insert('result', 84);
  }
}

async function executePipeline(initialData) {
  let ctxData = { ...initialData };
  
  const addLink = new AddNumbersLink();
  const addCtx = new Context(ctxData);
  const addResult = await addLink.call(addCtx);
  ctxData = addResult.toObject();
  
  const multiplyLink = new MultiplyLink();
  const multiplyCtx = new Context(ctxData);
  const multiplyResult = await multiplyLink.call(multiplyCtx);
  ctxData = multiplyResult.toObject();
  
  return ctxData;
}
`;

      const chain = converter.codeToChain(code);

      expect(chain._links.size).toBe(2);
      expect(chain._links.has('add')).toBe(true);
      expect(chain._links.has('multiply')).toBe(true);
    });
  });

  describe('validate', () => {
    test('should validate matching results', async () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');
      chain.connect('add', 'multiply');

      // Generate code
      const code = converter.chainToCode(chain);

      // Execute code to get function
      const executePipeline = new Function(
        'Context',
        'AddNumbersLink',
        'MultiplyLink',
        code + '\nreturn executePipeline;'
      )(Context, AddNumbersLink, MultiplyLink);

      // Test contexts
      const testContexts = [
        { a: 5, b: 3, factor: 2 }, // (5+3)*2 = 16
        { a: 10, b: 20, factor: 3 }, // (10+20)*3 = 90
        { a: 0, b: 0, factor: 5 }, // (0+0)*5 = 0
      ];

      // Validate
      const validation = await converter.validate(chain, executePipeline, testContexts);

      expect(validation.isValid).toBe(true);
      expect(validation.errors).toHaveLength(0);
    });

    test('should detect differences in results', async () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');

      // Create a function that produces different results
      async function wrongFunc(initialData) {
        return { result: 999 }; // Always return wrong value
      }

      const testContexts = [{ a: 5, b: 3 }];

      // Validate
      const validation = await converter.validate(chain, wrongFunc, testContexts);

      expect(validation.isValid).toBe(false);
      expect(validation.errors.length).toBeGreaterThan(0);
    });

    test('should handle floating-point tolerance', async () => {
      const chain = new Chain();

      class DivideLink extends Link {
        async call(ctx) {
          const a = ctx.get('a');
          const b = ctx.get('b');
          return ctx.insert('result', a / b);
        }
      }

      chain.addLink(new DivideLink(), 'divide');

      // Create a function with slight floating-point difference
      async function almostSameFunc(initialData) {
        const result = initialData.a / initialData.b;
        return {
          a: initialData.a,
          b: initialData.b,
          result: result + 1e-10,
        };
      }

      const testContexts = [{ a: 10, b: 3 }];

      // Should be invalid with tight tolerance
      const validation1 = await converter.validate(chain, almostSameFunc, testContexts, 1e-11);
      expect(validation1.isValid).toBe(false);

      // Should be valid with larger tolerance
      const validation2 = await converter.validate(chain, almostSameFunc, testContexts, 1e-9);
      expect(validation2.isValid).toBe(true);
    });
  });

  describe('generateOptimizedClass', () => {
    test('should generate an optimized class', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');

      const code = converter.generateOptimizedClass(chain, 'OptimizedPipeline');

      expect(code).toContain('class OptimizedPipeline');
      expect(code).toContain('constructor()');
      expect(code).toContain('async execute(');
      expect(code).toContain('this.addLink');
      expect(code).toContain('this.multiplyLink');
    });

    test('should generate executable class', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');

      const code = converter.generateOptimizedClass(chain, 'OptimizedPipeline');

      // Execute code to create class
      const OptimizedPipeline = new Function(
        'Context',
        'AddNumbersLink',
        'MultiplyLink',
        code + '\nreturn OptimizedPipeline;'
      )(Context, AddNumbersLink, MultiplyLink);

      // Instantiate class
      const pipeline = new OptimizedPipeline();

      expect(pipeline).toBeDefined();
      expect(pipeline.execute).toBeDefined();
      expect(pipeline.addLink).toBeDefined();
      expect(pipeline.multiplyLink).toBeDefined();
    });

    test('should execute correctly', async () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');

      const code = converter.generateOptimizedClass(chain, 'OptimizedPipeline');

      // Execute code to create class
      const OptimizedPipeline = new Function(
        'Context',
        'AddNumbersLink',
        'MultiplyLink',
        code + '\nreturn OptimizedPipeline;'
      )(Context, AddNumbersLink, MultiplyLink);

      // Create instance and test
      const pipeline = new OptimizedPipeline();
      const result = await pipeline.execute({ a: 5, b: 3, factor: 2 });

      expect(result.result).toBe(16); // (5+3)*2 = 16
    });
  });

  describe('end-to-end', () => {
    test('should complete full conversion cycle', async () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');
      chain.addLink(new MultiplyLink(), 'multiply');
      chain.addLink(new FormatResultLink(), 'format');
      chain.connect('add', 'multiply');
      chain.connect('multiply', 'format');

      // Convert to code
      const code = converter.chainToCode(chain);

      // Execute code
      const executePipeline = new Function(
        'Context',
        'AddNumbersLink',
        'MultiplyLink',
        'FormatResultLink',
        code + '\nreturn executePipeline;'
      )(Context, AddNumbersLink, MultiplyLink, FormatResultLink);

      // Test execution
      const result = await executePipeline({ a: 5, b: 5, factor: 3 });

      expect(result.result).toBe(30); // (5+5)*3 = 30
      expect(result.formatted).toBe('Result: 30');
    });
  });

  describe('edge cases', () => {
    test('should handle empty chain', () => {
      const chain = new Chain();
      const code = converter.chainToCode(chain);

      expect(code).toContain('async function executePipeline');
      expect(() => {
        new Function(code);
      }).not.toThrow();
    });

    test('should handle single link', () => {
      const chain = new Chain();
      chain.addLink(new AddNumbersLink(), 'add');

      const code = converter.chainToCode(chain);

      expect(code).toContain('addLink');
      expect(() => {
        new Function(code);
      }).not.toThrow();
    });
  });
});
