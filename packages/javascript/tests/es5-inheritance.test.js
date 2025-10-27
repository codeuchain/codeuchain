/**
 * ES5 Inheritance Tests
 * 
 * Tests that verify ES5-style inheritance works correctly with all core classes.
 * This ensures TypeScript projects targeting ES5 can extend CodeUChain classes.
 */

const { Context, MutableContext, Link, Chain, Middleware, LoggingMiddleware, TimingMiddleware, ValidationMiddleware } = require('../core');

describe('ES5 Inheritance Tests', () => {
  describe('Context ES5 Inheritance', () => {
    test('should support ES5-style inheritance for Context', () => {
      // ES5 inheritance pattern
      function CustomContext(data) {
        return Context.call(this, data) || this;
      }
      CustomContext.prototype = Object.create(Context.prototype);
      CustomContext.prototype.constructor = CustomContext;
      CustomContext.prototype.customMethod = function() {
        return 'custom';
      };

      const ctx = new CustomContext({ test: 'value' });
      
      expect(ctx).toBeInstanceOf(Context);
      expect(ctx).toBeInstanceOf(CustomContext);
      expect(ctx.get('test')).toBe('value');
      expect(ctx.customMethod()).toBe('custom');
    });

    test('should support ES5-style inheritance for MutableContext', () => {
      // ES5 inheritance pattern
      function CustomMutableContext(data) {
        return MutableContext.call(this, data) || this;
      }
      CustomMutableContext.prototype = Object.create(MutableContext.prototype);
      CustomMutableContext.prototype.constructor = CustomMutableContext;
      CustomMutableContext.prototype.customMethod = function() {
        return 'custom mutable';
      };

      const ctx = new CustomMutableContext({ test: 'value' });
      
      expect(ctx).toBeInstanceOf(MutableContext);
      expect(ctx).toBeInstanceOf(CustomMutableContext);
      expect(ctx.get('test')).toBe('value');
      expect(ctx.customMethod()).toBe('custom mutable');
      
      ctx.set('test', 'updated');
      expect(ctx.get('test')).toBe('updated');
    });
  });

  describe('Link ES5 Inheritance', () => {
    test('should support ES5-style inheritance for Link', async () => {
      // ES5 inheritance pattern
      function CustomLink() {
        return Link.call(this) || this;
      }
      CustomLink.prototype = Object.create(Link.prototype);
      CustomLink.prototype.constructor = CustomLink;
      CustomLink.prototype.call = async function(ctx) {
        return ctx.insert('processed', true);
      };

      const link = new CustomLink();
      
      expect(link).toBeInstanceOf(Link);
      expect(link).toBeInstanceOf(CustomLink);
      
      const ctx = new Context({ input: 'data' });
      const result = await link.call(ctx);
      
      expect(result.get('processed')).toBe(true);
      expect(result.get('input')).toBe('data');
    });

    test('should maintain getName functionality with ES5 inheritance', () => {
      function ProcessorLink() {
        return Link.call(this) || this;
      }
      ProcessorLink.prototype = Object.create(Link.prototype);
      ProcessorLink.prototype.constructor = ProcessorLink;
      ProcessorLink.prototype.call = async function(ctx) {
        return ctx.insert('result', 'processed');
      };

      const link = new ProcessorLink();
      expect(link.getName()).toBe('ProcessorLink');
    });

    test('should support validateContext with ES5 inheritance', async () => {
      function ValidatingLink() {
        return Link.call(this) || this;
      }
      ValidatingLink.prototype = Object.create(Link.prototype);
      ValidatingLink.prototype.constructor = ValidatingLink;
      ValidatingLink.prototype.call = async function(ctx) {
        this.validateContext(ctx, ['required']);
        return ctx.insert('validated', true);
      };

      const link = new ValidatingLink();
      const validCtx = new Context({ required: 'value' });
      const invalidCtx = new Context({});

      const result = await link.call(validCtx);
      expect(result.get('validated')).toBe(true);

      await expect(link.call(invalidCtx)).rejects.toThrow('Required field \'required\' is missing');
    });
  });

  describe('Chain ES5 Inheritance', () => {
    test('should support ES5-style inheritance for Chain', async () => {
      // ES5 inheritance pattern
      function CustomChain() {
        return Chain.call(this) || this;
      }
      CustomChain.prototype = Object.create(Chain.prototype);
      CustomChain.prototype.constructor = CustomChain;
      CustomChain.prototype.customMethod = function() {
        return 'custom chain';
      };

      const chain = new CustomChain();
      
      expect(chain).toBeInstanceOf(Chain);
      expect(chain).toBeInstanceOf(CustomChain);
      expect(chain.customMethod()).toBe('custom chain');
    });

    test('should work with ES5 custom Link in ES5 custom Chain', async () => {
      function TestLink() {
        return Link.call(this) || this;
      }
      TestLink.prototype = Object.create(Link.prototype);
      TestLink.prototype.constructor = TestLink;
      TestLink.prototype.call = async function(ctx) {
        const value = ctx.get('counter') || 0;
        return ctx.insert('counter', value + 1);
      };

      function TestChain() {
        return Chain.call(this) || this;
      }
      TestChain.prototype = Object.create(Chain.prototype);
      TestChain.prototype.constructor = TestChain;

      const chain = new TestChain();
      chain.addLink(new TestLink(), 'link1');
      chain.addLink(new TestLink(), 'link2');
      chain.connect('link1', 'link2');

      const ctx = new Context({ counter: 0 });
      const result = await chain.run(ctx);

      expect(result.get('counter')).toBe(2);
    });
  });

  describe('Middleware ES5 Inheritance', () => {
    test('should support ES5-style inheritance for Middleware', async () => {
      // ES5 inheritance pattern
      function CustomMiddleware() {
        return Middleware.call(this) || this;
      }
      CustomMiddleware.prototype = Object.create(Middleware.prototype);
      CustomMiddleware.prototype.constructor = CustomMiddleware;
      CustomMiddleware.prototype.before = async function(link, ctx, linkName) {
        return ctx.insert('middlewareCalled', true);
      };

      const middleware = new CustomMiddleware();
      
      expect(middleware).toBeInstanceOf(Middleware);
      expect(middleware).toBeInstanceOf(CustomMiddleware);

      function TestLink() {
        return Link.call(this) || this;
      }
      TestLink.prototype = Object.create(Link.prototype);
      TestLink.prototype.constructor = TestLink;
      TestLink.prototype.call = async function(ctx) {
        return ctx.insert('processed', true);
      };

      const chain = new Chain();
      chain.useMiddleware(middleware);
      chain.addLink(new TestLink());

      const ctx = new Context({});
      const result = await chain.run(ctx);

      expect(result.get('middlewareCalled')).toBe(true);
      expect(result.get('processed')).toBe(true);
    });

    test('should support ES5 inheritance for LoggingMiddleware', () => {
      function CustomLoggingMiddleware() {
        return LoggingMiddleware.call(this) || this;
      }
      CustomLoggingMiddleware.prototype = Object.create(LoggingMiddleware.prototype);
      CustomLoggingMiddleware.prototype.constructor = CustomLoggingMiddleware;

      const middleware = new CustomLoggingMiddleware();
      expect(middleware).toBeInstanceOf(Middleware);
      expect(middleware).toBeInstanceOf(LoggingMiddleware);
      expect(middleware).toBeInstanceOf(CustomLoggingMiddleware);
    });

    test('should support ES5 inheritance for TimingMiddleware', () => {
      function CustomTimingMiddleware() {
        return TimingMiddleware.call(this) || this;
      }
      CustomTimingMiddleware.prototype = Object.create(TimingMiddleware.prototype);
      CustomTimingMiddleware.prototype.constructor = CustomTimingMiddleware;

      const middleware = new CustomTimingMiddleware();
      expect(middleware).toBeInstanceOf(Middleware);
      expect(middleware).toBeInstanceOf(TimingMiddleware);
      expect(middleware).toBeInstanceOf(CustomTimingMiddleware);
      expect(middleware._timings).toBeInstanceOf(Map);
    });

    test('should support ES5 inheritance for ValidationMiddleware', () => {
      function CustomValidationMiddleware(options) {
        return ValidationMiddleware.call(this, options) || this;
      }
      CustomValidationMiddleware.prototype = Object.create(ValidationMiddleware.prototype);
      CustomValidationMiddleware.prototype.constructor = CustomValidationMiddleware;

      const validator = jest.fn();
      const middleware = new CustomValidationMiddleware({ beforeValidator: validator });
      
      expect(middleware).toBeInstanceOf(Middleware);
      expect(middleware).toBeInstanceOf(ValidationMiddleware);
      expect(middleware).toBeInstanceOf(CustomValidationMiddleware);
      expect(middleware.beforeValidator).toBe(validator);
    });
  });

  describe('Complex ES5 Inheritance Scenarios', () => {
    test('should support multi-level ES5 inheritance', async () => {
      // Base custom link
      function BaseProcessorLink() {
        return Link.call(this) || this;
      }
      BaseProcessorLink.prototype = Object.create(Link.prototype);
      BaseProcessorLink.prototype.constructor = BaseProcessorLink;
      BaseProcessorLink.prototype.processData = function(data) {
        return data.toUpperCase();
      };

      // Derived custom link
      function DerivedProcessorLink() {
        return BaseProcessorLink.call(this) || this;
      }
      DerivedProcessorLink.prototype = Object.create(BaseProcessorLink.prototype);
      DerivedProcessorLink.prototype.constructor = DerivedProcessorLink;
      DerivedProcessorLink.prototype.call = async function(ctx) {
        const input = ctx.get('input');
        const processed = this.processData(input);
        return ctx.insert('output', processed);
      };

      const link = new DerivedProcessorLink();
      
      expect(link).toBeInstanceOf(Link);
      expect(link).toBeInstanceOf(BaseProcessorLink);
      expect(link).toBeInstanceOf(DerivedProcessorLink);

      const ctx = new Context({ input: 'test' });
      const result = await link.call(ctx);
      
      expect(result.get('output')).toBe('TEST');
    });

    test('should support ES5 inheritance with constructor parameters', async () => {
      function ConfigurableLink(config) {
        if (!(this instanceof ConfigurableLink)) {
          return new ConfigurableLink(config);
        }
        Link.call(this);
        this.config = config || { multiplier: 1 };
      }
      ConfigurableLink.prototype = Object.create(Link.prototype);
      ConfigurableLink.prototype.constructor = ConfigurableLink;
      ConfigurableLink.prototype.call = async function(ctx) {
        const value = ctx.get('value');
        return ctx.insert('result', value * this.config.multiplier);
      };

      const link = new ConfigurableLink({ multiplier: 5 });
      
      expect(link).toBeInstanceOf(Link);
      expect(link.config.multiplier).toBe(5);

      const ctx = new Context({ value: 10 });
      const result = await link.call(ctx);
      
      expect(result.get('result')).toBe(50);
    });

    test('should work in complete workflow with all ES5 classes', async () => {
      // Custom Context
      function WorkflowContext(data) {
        return Context.call(this, data) || this;
      }
      WorkflowContext.prototype = Object.create(Context.prototype);
      WorkflowContext.prototype.constructor = WorkflowContext;

      // Custom Link
      function WorkflowLink() {
        return Link.call(this) || this;
      }
      WorkflowLink.prototype = Object.create(Link.prototype);
      WorkflowLink.prototype.constructor = WorkflowLink;
      WorkflowLink.prototype.call = async function(ctx) {
        return ctx.insert('step1', 'completed');
      };

      // Custom Chain
      function WorkflowChain() {
        return Chain.call(this) || this;
      }
      WorkflowChain.prototype = Object.create(Chain.prototype);
      WorkflowChain.prototype.constructor = WorkflowChain;

      // Custom Middleware
      function WorkflowMiddleware() {
        return Middleware.call(this) || this;
      }
      WorkflowMiddleware.prototype = Object.create(Middleware.prototype);
      WorkflowMiddleware.prototype.constructor = WorkflowMiddleware;
      WorkflowMiddleware.prototype.before = async function(link, ctx, linkName) {
        return ctx.insert('middlewareActive', true);
      };

      const ctx = new WorkflowContext({ initial: 'data' });
      const chain = new WorkflowChain();
      const middleware = new WorkflowMiddleware();
      const link = new WorkflowLink();

      chain.useMiddleware(middleware);
      chain.addLink(link);

      const result = await chain.run(ctx);

      expect(result).toBeInstanceOf(Context);
      expect(result.get('initial')).toBe('data');
      expect(result.get('middlewareActive')).toBe(true);
      expect(result.get('step1')).toBe('completed');
    });
  });

  describe('TypeScript ES5 Compilation Pattern', () => {
    test('should handle exact TypeScript ES5 output pattern', async () => {
      // This is what TypeScript generates for ES5 target
      var __extends = function (d, b) {
        for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
      };

      var TypeScriptStyleLink = (function (_super) {
        __extends(TypeScriptStyleLink, _super);
        function TypeScriptStyleLink() {
          return _super.call(this) || this;
        }
        TypeScriptStyleLink.prototype.call = async function (ctx) {
          return ctx.insert('typescript', 'works');
        };
        return TypeScriptStyleLink;
      }(Link));

      const link = new TypeScriptStyleLink();
      
      expect(link).toBeInstanceOf(Link);
      
      const ctx = new Context({ test: 'value' });
      const result = await link.call(ctx);
      
      expect(result.get('typescript')).toBe('works');
      expect(result.get('test')).toBe('value');
    });
  });
});
