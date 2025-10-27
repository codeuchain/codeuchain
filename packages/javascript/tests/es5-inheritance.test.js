/**
 * ES5 TypeScript Inheritance Compatibility Tests
 *
 * These tests verify that Link and Chain classes can be properly extended
 * using TypeScript ES5 target compilation output, which generates ES5-style
 * constructor functions with prototypal inheritance.
 *
 * Issue: https://github.com/codeuchain/codeuchain/issues/XXX
 * Root Cause: ES6 class constructors cannot be called with .apply() or .call()
 * Solution: Convert runtime classes to ES5-compatible function constructors
 */

const { Link, Chain, Context } = require('../core');

// Simulate TypeScript ES5 compilation output for extending Link
// This is what TypeScript generates when target is ES5
const __extends = (function () {
  var extendStatics = function (d, b) {
    extendStatics = Object.setPrototypeOf ||
      ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
      function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
    return extendStatics(d, b);
  };
  return function (d, b) {
    if (typeof b !== "function" && b !== null)
      throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
    extendStatics(d, b);
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
  };
})();

describe('ES5 TypeScript Inheritance Compatibility', () => {
  describe('Link ES5 Inheritance', () => {
    test('should allow ES5-style inheritance from Link', () => {
      // Simulate TypeScript ES5 output for: class TestLink extends Link
      var TestLink = /** @class */ (function (_super) {
        __extends(TestLink, _super);
        function TestLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        TestLink.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('es5test', true));
        };
        return TestLink;
      }(Link));

      // Should be able to instantiate without "cannot be invoked without 'new'" error
      expect(() => {
        const link = new TestLink();
      }).not.toThrow();

      const link = new TestLink();
      expect(link).toBeInstanceOf(Link);
      expect(link).toBeInstanceOf(TestLink);
    });

    test('ES5-inherited Link should work correctly', async () => {
      // Create an ES5-style Link extension
      var ProcessingLink = /** @class */ (function (_super) {
        __extends(ProcessingLink, _super);
        function ProcessingLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        ProcessingLink.prototype.call = function (ctx) {
          var value = ctx.get('input');
          return Promise.resolve(ctx.insert('output', value * 2));
        };
        return ProcessingLink;
      }(Link));

      const link = new ProcessingLink();
      const ctx = new Context({ input: 5 });
      const result = await link.call(ctx);

      expect(result.get('output')).toBe(10);
      expect(result.get('input')).toBe(5);
    });

    test('ES5-inherited Link should support getName()', () => {
      var NamedLink = /** @class */ (function (_super) {
        __extends(NamedLink, _super);
        function NamedLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        NamedLink.prototype.call = function (ctx) {
          return Promise.resolve(ctx);
        };
        return NamedLink;
      }(Link));

      const link = new NamedLink();
      expect(link.getName()).toBe('NamedLink');
    });

    test('ES5-inherited Link should support validateContext()', async () => {
      var ValidatingLink = /** @class */ (function (_super) {
        __extends(ValidatingLink, _super);
        function ValidatingLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        ValidatingLink.prototype.call = function (ctx) {
          this.validateContext(ctx, ['requiredField']);
          return Promise.resolve(ctx.insert('validated', true));
        };
        return ValidatingLink;
      }(Link));

      const link = new ValidatingLink();
      const validCtx = new Context({ requiredField: 'value' });
      const invalidCtx = new Context({ otherField: 'value' });

      // Should work with valid context
      const result = await link.call(validCtx);
      expect(result.get('validated')).toBe(true);

      // Should throw with invalid context
      await expect(async () => {
        await link.call(invalidCtx);
      }).rejects.toThrow('Required field \'requiredField\' is missing from context');
    });
  });

  describe('Chain ES5 Inheritance', () => {
    test('should allow ES5-style inheritance from Chain', () => {
      // Simulate TypeScript ES5 output for: class TestChain extends Chain
      var TestChain = /** @class */ (function (_super) {
        __extends(TestChain, _super);
        function TestChain() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        return TestChain;
      }(Chain));

      // Should be able to instantiate without "cannot be invoked without 'new'" error
      expect(() => {
        const chain = new TestChain();
      }).not.toThrow();

      const chain = new TestChain();
      expect(chain).toBeInstanceOf(Chain);
      expect(chain).toBeInstanceOf(TestChain);
    });

    test('ES5-inherited Chain should work correctly', async () => {
      // Create an ES5-style Chain extension
      var CustomChain = /** @class */ (function (_super) {
        __extends(CustomChain, _super);
        function CustomChain() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        CustomChain.prototype.customMethod = function () {
          return 'custom';
        };
        return CustomChain;
      }(Chain));

      // Create a simple Link using ES5 style
      var SimpleLink = /** @class */ (function (_super) {
        __extends(SimpleLink, _super);
        function SimpleLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        SimpleLink.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('processed', true));
        };
        return SimpleLink;
      }(Link));

      const chain = new CustomChain();
      const link = new SimpleLink();
      
      chain.addLink(link);
      expect(chain.customMethod()).toBe('custom');

      const ctx = new Context({ initial: 'data' });
      const result = await chain.run(ctx);

      expect(result.get('processed')).toBe(true);
      expect(result.get('initial')).toBe('data');
    });

    test('ES5-inherited Chain should support all chain methods', () => {
      var EnhancedChain = /** @class */ (function (_super) {
        __extends(EnhancedChain, _super);
        function EnhancedChain() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        return EnhancedChain;
      }(Chain));

      var Link1 = /** @class */ (function (_super) {
        __extends(Link1, _super);
        function Link1() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        Link1.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('step1', true));
        };
        return Link1;
      }(Link));

      var Link2 = /** @class */ (function (_super) {
        __extends(Link2, _super);
        function Link2() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        Link2.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('step2', true));
        };
        return Link2;
      }(Link));

      const chain = new EnhancedChain();
      
      // Test addLink
      expect(() => chain.addLink(new Link1(), 'link1')).not.toThrow();
      expect(() => chain.addLink(new Link2(), 'link2')).not.toThrow();

      // Test connect
      expect(() => chain.connect('link1', 'link2')).not.toThrow();

      // Test getLinkNames
      expect(chain.getLinkNames()).toEqual(['link1', 'link2']);

      // Test useMiddleware and onError
      expect(() => chain.useMiddleware({ before: () => {} })).not.toThrow();
      expect(() => chain.onError(() => {})).not.toThrow();
    });
  });

  describe('Mixed ES6 and ES5 Inheritance', () => {
    test('ES6 class extending from ES5-converted Link should work', async () => {
      // ES6 class syntax extending the ES5-compatible Link
      class ES6Link extends Link {
        async call(ctx) {
          return ctx.insert('es6', true);
        }
      }

      const link = new ES6Link();
      const ctx = new Context({ test: 'data' });
      const result = await link.call(ctx);

      expect(result.get('es6')).toBe(true);
      expect(result.get('test')).toBe('data');
      expect(link).toBeInstanceOf(Link);
      expect(link).toBeInstanceOf(ES6Link);
    });

    test('ES6 class extending from ES5-converted Chain should work', async () => {
      // ES6 class syntax extending the ES5-compatible Chain
      class ES6Chain extends Chain {
        customFeature() {
          return 'es6-feature';
        }
      }

      // ES6 Link
      class ES6Link extends Link {
        async call(ctx) {
          return ctx.insert('processed', 'es6');
        }
      }

      const chain = new ES6Chain();
      const link = new ES6Link();
      
      chain.addLink(link);
      expect(chain.customFeature()).toBe('es6-feature');

      const ctx = new Context({ initial: 'data' });
      const result = await chain.run(ctx);

      expect(result.get('processed')).toBe('es6');
      expect(chain).toBeInstanceOf(Chain);
      expect(chain).toBeInstanceOf(ES6Chain);
    });

    test('Chain.createLinear should work with ES5-inherited Links', async () => {
      var Link1 = /** @class */ (function (_super) {
        __extends(Link1, _super);
        function Link1() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        Link1.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('step1', 1));
        };
        return Link1;
      }(Link));

      var Link2 = /** @class */ (function (_super) {
        __extends(Link2, _super);
        function Link2() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        Link2.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('step2', 2));
        };
        return Link2;
      }(Link));

      var Link3 = /** @class */ (function (_super) {
        __extends(Link3, _super);
        function Link3() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        Link3.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('step3', 3));
        };
        return Link3;
      }(Link));

      const chain = Chain.createLinear(new Link1(), new Link2(), new Link3());
      expect(chain).toBeInstanceOf(Chain);
      expect(chain.getLinkNames()).toHaveLength(3);
    });
  });

  describe('Middleware ES5 Inheritance', () => {
    test('should allow ES5-style inheritance from Middleware', () => {
      var CustomMiddleware = /** @class */ (function (_super) {
        __extends(CustomMiddleware, _super);
        function CustomMiddleware() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        return CustomMiddleware;
      }(require('../core').Middleware));

      // Should be able to instantiate without "cannot be invoked without 'new'" error
      expect(() => {
        const middleware = new CustomMiddleware();
      }).not.toThrow();

      const middleware = new CustomMiddleware();
      expect(middleware).toBeInstanceOf(require('../core').Middleware);
      expect(middleware).toBeInstanceOf(CustomMiddleware);
    });

    test('ES5-inherited Middleware should work correctly', async () => {
      var CountingMiddleware = /** @class */ (function (_super) {
        __extends(CountingMiddleware, _super);
        function CountingMiddleware() {
          var _this = _super !== null && _super.apply(this, arguments) || this;
          _this.beforeCount = 0;
          _this.afterCount = 0;
          _this.errorCount = 0;
          return _this;
        }
        CountingMiddleware.prototype.before = function (link, ctx, linkName) {
          this.beforeCount++;
          return Promise.resolve();
        };
        CountingMiddleware.prototype.after = function (link, ctx, linkName) {
          this.afterCount++;
          return Promise.resolve();
        };
        CountingMiddleware.prototype.onError = function (link, error, ctx, linkName) {
          this.errorCount++;
          return Promise.resolve();
        };
        return CountingMiddleware;
      }(require('../core').Middleware));

      const middleware = new CountingMiddleware();
      const chain = new Chain();
      
      var SuccessLink = /** @class */ (function (_super) {
        __extends(SuccessLink, _super);
        function SuccessLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        SuccessLink.prototype.call = function (ctx) {
          return Promise.resolve(ctx.insert('success', true));
        };
        return SuccessLink;
      }(Link));

      chain.useMiddleware(middleware);
      chain.addLink(new SuccessLink());

      const ctx = new Context({ test: 'data' });
      await chain.run(ctx);

      expect(middleware.beforeCount).toBe(1);
      expect(middleware.afterCount).toBe(1);
      expect(middleware.errorCount).toBe(0);
    });

    test('ES6 class extending from ES5-converted Middleware should work', async () => {
      // ES6 class syntax extending the ES5-compatible Middleware
      class ES6Middleware extends require('../core').Middleware {
        constructor() {
          super();
          this.callCount = 0;
        }

        async before(link, ctx, linkName) {
          this.callCount++;
        }
      }

      const middleware = new ES6Middleware();
      const chain = new Chain();
      
      class TestLink extends Link {
        async call(ctx) {
          return ctx.insert('tested', true);
        }
      }

      chain.useMiddleware(middleware);
      chain.addLink(new TestLink());

      const ctx = new Context({ initial: 'value' });
      await chain.run(ctx);

      expect(middleware.callCount).toBe(1);
      expect(middleware).toBeInstanceOf(require('../core').Middleware);
      expect(middleware).toBeInstanceOf(ES6Middleware);
    });
  });

  describe('Real-world ES5 TypeScript Scenario', () => {
    test('complete workflow with ES5-compiled TypeScript classes', async () => {
      // Simulate a real-world scenario where TypeScript compiles to ES5

      // ValidationLink
      var ValidationLink = /** @class */ (function (_super) {
        __extends(ValidationLink, _super);
        function ValidationLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        ValidationLink.prototype.call = function (ctx) {
          this.validateContext(ctx, ['input']);
          var input = ctx.get('input');
          var isValid = typeof input === 'number' && input > 0;
          return Promise.resolve(ctx.insert('isValid', isValid));
        };
        return ValidationLink;
      }(Link));

      // ProcessingLink
      var ProcessingLink = /** @class */ (function (_super) {
        __extends(ProcessingLink, _super);
        function ProcessingLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        ProcessingLink.prototype.call = function (ctx) {
          var input = ctx.get('input');
          var result = input * 2;
          return Promise.resolve(ctx.insert('result', result));
        };
        return ProcessingLink;
      }(Link));

      // ResultLink
      var ResultLink = /** @class */ (function (_super) {
        __extends(ResultLink, _super);
        function ResultLink() {
          return _super !== null && _super.apply(this, arguments) || this;
        }
        ResultLink.prototype.call = function (ctx) {
          var isValid = ctx.get('isValid');
          var result = ctx.get('result');
          return Promise.resolve(ctx.insert('final', isValid ? result : 0));
        };
        return ResultLink;
      }(Link));

      // Create chain
      const chain = new Chain();
      chain.addLink(new ValidationLink(), 'validate');
      chain.addLink(new ProcessingLink(), 'process');
      chain.addLink(new ResultLink(), 'result');

      chain.connect('validate', 'process', function(ctx) { return ctx.get('isValid'); });
      chain.connect('validate', 'result', function(ctx) { return !ctx.get('isValid'); });
      chain.connect('process', 'result');

      // Test valid input
      const validCtx = new Context({ input: 5 });
      const validResult = await chain.run(validCtx);
      expect(validResult.get('final')).toBe(10);

      // Test invalid input (negative)
      const invalidCtx = new Context({ input: -5 });
      const invalidResult = await chain.run(invalidCtx);
      expect(invalidResult.get('final')).toBe(0);
    });
  });
});
