/**
 * ES5 Compatibility Tests
 * 
 * Tests to ensure ES5 transpilation works correctly and provides identical functionality
 */

describe('ES5 Compatibility', () => {
  let es5Module, es6Module;

  beforeAll(() => {
    es5Module = require('../index.es5');
    es6Module = require('../index.es6');
  });

  test('should export the same interface for ES5 and ES6', () => {
    const es5Keys = Object.keys(es5Module).sort();
    const es6Keys = Object.keys(es6Module).sort();
    
    expect(es5Keys).toEqual(es6Keys);
  });

  test('should have functional Context class in ES5', () => {
    const { Context } = es5Module;
    
    const ctx = new Context({ name: 'Alice', age: 30 });
    expect(ctx.get('name')).toBe('Alice');
    expect(ctx.get('age')).toBe(30);
    
    const newCtx = ctx.insert('city', 'New York');
    expect(newCtx.get('city')).toBe('New York');
    expect(ctx.get('city')).toBeUndefined(); // Original should remain unchanged
  });

  test('should have functional MutableContext class in ES5', () => {
    const { MutableContext } = es5Module;
    
    const ctx = new MutableContext({ counter: 0 });
    expect(ctx.get('counter')).toBe(0);
    
    ctx.set('counter', 5);
    expect(ctx.get('counter')).toBe(5);
  });

  test('should have functional Link class in ES5', () => {
    const { Link, Context } = es5Module;
    
    class TestLink extends Link {
      async call(ctx) {
        return ctx.insert('processed', true);
      }
    }
    
    const link = new TestLink();
    const ctx = new Context({ input: 'test' });
    
    return link.call(ctx).then(result => {
      expect(result.get('processed')).toBe(true);
      expect(result.get('input')).toBe('test');
    });
  });

  test('should have functional Chain class in ES5', () => {
    const { Chain, Link, Context } = es5Module;
    
    class Step1 extends Link {
      async call(ctx) {
        return ctx.insert('step1', true);
      }
    }
    
    class Step2 extends Link {
      async call(ctx) {
        return ctx.insert('step2', true);
      }
    }
    
    const chain = new Chain();
    chain.addLink(new Step1(), 'step1');
    chain.addLink(new Step2(), 'step2');
    chain.connect('step1', 'step2');
    
    const ctx = new Context({ input: 'test' });
    
    return chain.run(ctx).then(result => {
      expect(result.get('step1')).toBe(true);
      expect(result.get('step2')).toBe(true);
      expect(result.get('input')).toBe('test');
    });
  });

  test('should have functional middleware in ES5', () => {
    const { LoggingMiddleware, TimingMiddleware, ValidationMiddleware } = es5Module;
    
    expect(typeof LoggingMiddleware).toBe('function');
    expect(typeof TimingMiddleware).toBe('function');
    expect(typeof ValidationMiddleware).toBe('function');
    
    const loggingMiddleware = new LoggingMiddleware();
    expect(typeof loggingMiddleware.before).toBe('function');
    expect(typeof loggingMiddleware.after).toBe('function');
  });

  test('should handle async/await correctly in ES5', async () => {
    const { Context, Link } = es5Module;
    
    class AsyncLink extends Link {
      async call(ctx) {
        // Simulate async operation
        await new Promise(resolve => setTimeout(resolve, 10));
        return ctx.insert('async_result', 'completed');
      }
    }
    
    const link = new AsyncLink();
    const ctx = new Context({ input: 'test' });
    
    const result = await link.call(ctx);
    expect(result.get('async_result')).toBe('completed');
  });

  test('should handle object spread equivalent in ES5', () => {
    const { Context } = es5Module;
    
    const ctx1 = new Context({ a: 1, b: 2 });
    const ctx2 = new Context({ c: 3 });
    
    const merged = ctx1.merge(ctx2);
    
    expect(merged.get('a')).toBe(1);
    expect(merged.get('b')).toBe(2);
    expect(merged.get('c')).toBe(3);
  });

  test('should have identical behavior between ES5 and ES6 versions', async () => {
    const { Context: ES5Context, Link: ES5Link } = es5Module;
    const { Context: ES6Context, Link: ES6Link } = es6Module;
    
    class ES5TestLink extends ES5Link {
      async call(ctx) {
        return ctx.insert('result', ctx.get('input') * 2);
      }
    }
    
    class ES6TestLink extends ES6Link {
      async call(ctx) {
        return ctx.insert('result', ctx.get('input') * 2);
      }
    }
    
    const es5Ctx = new ES5Context({ input: 5 });
    const es6Ctx = new ES6Context({ input: 5 });
    
    const es5Link = new ES5TestLink();
    const es6Link = new ES6TestLink();
    
    const [es5Result, es6Result] = await Promise.all([
      es5Link.call(es5Ctx),
      es6Link.call(es6Ctx)
    ]);
    
    expect(es5Result.get('result')).toBe(es6Result.get('result'));
    expect(es5Result.get('input')).toBe(es6Result.get('input'));
  });

  test('should work in simulated ES5 environment', () => {
    // Test that ES5 code doesn't use any ES6+ features
    const es5Code = require('fs').readFileSync(
      require.resolve('../dist/es5/core/context.js'), 
      'utf8'
    );
    
    // Check that ES6+ features are not present in raw form
    expect(es5Code).not.toMatch(/class\s+\w+/); // No native classes
    expect(es5Code).not.toMatch(/=>/); // No arrow functions  
    expect(es5Code).not.toMatch(/\.\.\./); // No spread operator (in native form)
    expect(es5Code).not.toMatch(/`[^`]*`/); // No template literals
    
    // Check that Babel transformation is working
    expect(es5Code).toMatch(/var _classCallCheck/); // Babel class helper should be present
    expect(es5Code).toMatch(/var _createClass/); // Babel class helper should be present
    expect(es5Code).toMatch(/_objectSpread/); // Object spread polyfill should be present
    
    // Should use var instead of const/let (in transformed code areas)
    expect(es5Code).toMatch(/var Context/); // Main class should be var
    expect(es5Code).toMatch(/var MutableContext/); // Main class should be var
  });

  test('auto-detection should choose appropriate version', () => {
    const autoModule = require('../index.auto');
    
    // In modern Node.js environment, should have all expected exports
    expect(autoModule).toHaveProperty('Context');
    expect(autoModule).toHaveProperty('Link');
    expect(autoModule).toHaveProperty('Chain');
    expect(autoModule.version).toBeTruthy();
  });
});

describe('Performance Comparison', () => {
  test('ES5 and ES6 versions should have comparable performance', async () => {
    const { Context: ES5Context, Chain: ES5Chain, Link: ES5Link } = require('../index.es5');
    const { Context: ES6Context, Chain: ES6Chain, Link: ES6Link } = require('../index.es6');
    
    class ES5TestLinkPerf extends ES5Link {
      async call(ctx) {
        return ctx.insert('processed', Date.now());
      }
    }
    
    class ES6TestLinkPerf extends ES6Link {
      async call(ctx) {
        return ctx.insert('processed', Date.now());
      }
    }
    
    const iterations = 100;
    
    // ES5 performance test
    const es5Start = Date.now();
    for (let i = 0; i < iterations; i++) {
      const ctx = new ES5Context({ iteration: i });
      const link = new ES5TestLinkPerf();
      await link.call(ctx);
    }
    const es5Time = Date.now() - es5Start;
    
    // ES6 performance test  
    const es6Start = Date.now();
    for (let i = 0; i < iterations; i++) {
      const ctx = new ES6Context({ iteration: i });
      const link = new ES6TestLinkPerf();
      await link.call(ctx);
    }
    const es6Time = Date.now() - es6Start;
    
    // Performance should be within reasonable bounds (ES5 may be slightly slower due to polyfills)
    const performanceRatio = es6Time === 0 ? 1 : es5Time / es6Time;
    expect(performanceRatio).toBeLessThan(5); // ES5 shouldn't be more than 5x slower
    
    console.log(`Performance comparison - ES5: ${es5Time}ms, ES6: ${es6Time}ms, Ratio: ${performanceRatio.toFixed(2)}`);
  });
});