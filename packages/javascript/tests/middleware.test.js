const { LoggingMiddleware, TimingMiddleware, ValidationMiddleware, Link, Context } = require('../core');

class TestLink extends Link {
  constructor(name, processor = async (ctx) => ctx) {
    super();
    this._name = name;
    this.processor = processor;
  }

  getName() {
    return this._name;
  }

  async call(ctx) {
    return await this.processor(ctx);
  }
}

describe('Middleware', () => {
  describe('LoggingMiddleware', () => {
    let loggingMiddleware;
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      loggingMiddleware = new LoggingMiddleware();
      mockLink = new TestLink('test');
      mockCtx = new Context({ test: 'data' });
    });

    test('should log before link execution', async () => {
      await loggingMiddleware.before(mockLink, mockCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Starting test')
      );
    });

    test('should log after link execution', async () => {
      const resultCtx = new Context({ result: 'success' });
      await loggingMiddleware.after(mockLink, resultCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Completed test')
      );
    });

    test('should log errors', async () => {
      const error = new Error('Test error');
      await loggingMiddleware.onError(mockLink, error, mockCtx, 'test');

      expect(console.error).toHaveBeenCalledWith(
        expect.stringContaining('Error in test: Test error')
      );
    });

    test('should handle missing result in after logging', async () => {
      const resultCtx = new Context({}); // No result field
      await loggingMiddleware.after(mockLink, resultCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Completed test')
      );
    });
  });

  describe('TimingMiddleware', () => {
    let timingMiddleware;
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      timingMiddleware = new TimingMiddleware();
      mockLink = new TestLink('test');
      mockCtx = new Context({ test: 'data' });
    });

    test('should measure execution time', async () => {
      await timingMiddleware.before(mockLink, mockCtx, 'test');

      // Simulate some processing time
      await new Promise(resolve => setTimeout(resolve, 10));

      await timingMiddleware.after(mockLink, mockCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/test executed in \d+ms/)
      );
    });

    test('should handle multiple links independently', async () => {
      const link1 = new TestLink('link1');
      const link2 = new TestLink('link2');

      await timingMiddleware.before(link1, mockCtx, 'link1');
      await timingMiddleware.before(link2, mockCtx, 'link2');

      await timingMiddleware.after(link1, mockCtx, 'link1');
      await timingMiddleware.after(link2, mockCtx, 'link2');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/link1 executed in \d+ms/)
      );
      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/link2 executed in \d+ms/)
      );
    });

    test('should handle missing start time', async () => {
      // Call after without before - should not log
      await timingMiddleware.after(mockLink, mockCtx, 'test');

      expect(console.log).not.toHaveBeenCalled();
    });
  });

  describe('ValidationMiddleware', () => {
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      mockLink = new TestLink('test');
      mockCtx = new Context({ name: 'Alice', email: 'alice@test.com' });
    });

    test('should validate before execution', async () => {
      const beforeValidator = jest.fn();
      const validationMiddleware = new ValidationMiddleware({
        beforeValidator
      });

      await validationMiddleware.before(mockLink, mockCtx, 'test');

      expect(beforeValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should validate after execution', async () => {
      const afterValidator = jest.fn();
      const validationMiddleware = new ValidationMiddleware({
        afterValidator
      });

      await validationMiddleware.after(mockLink, mockCtx, 'test');

      expect(afterValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should throw error on before validation failure', async () => {
      const beforeValidator = jest.fn(() => {
        throw new Error('Validation failed');
      });
      const validationMiddleware = new ValidationMiddleware({
        beforeValidator
      });

      await expect(
        validationMiddleware.before(mockLink, mockCtx, 'test')
      ).rejects.toThrow('Pre-validation failed for test: Validation failed');
    });

    test('should throw error on after validation failure', async () => {
      const afterValidator = jest.fn(() => {
        throw new Error('Post-validation failed');
      });
      const validationMiddleware = new ValidationMiddleware({
        afterValidator
      });

      await expect(
        validationMiddleware.after(mockLink, mockCtx, 'test')
      ).rejects.toThrow('Post-validation failed for test: Post-validation failed');
    });

    test('should handle async validators', async () => {
      const beforeValidator = jest.fn(async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return true;
      });

      const validationMiddleware = new ValidationMiddleware({
        beforeValidator
      });

      await validationMiddleware.before(mockLink, mockCtx, 'test');

      expect(beforeValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should work without validators', async () => {
      const validationMiddleware = new ValidationMiddleware();

      await expect(
        validationMiddleware.before(mockLink, mockCtx, 'test')
      ).resolves.toBeUndefined();

      await expect(
        validationMiddleware.after(mockLink, mockCtx, 'test')
      ).resolves.toBeUndefined();
    });
  });

  describe('Middleware Integration', () => {
    test('should combine multiple middleware', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx.insert('processed', true));
      chain.addLink(link, 'test');

      // Add multiple middleware
      chain.useMiddleware(new LoggingMiddleware());
      chain.useMiddleware(new TimingMiddleware());

      const ctx = new Context({ input: 'test' });
      const result = await chain.run(ctx);

      expect(result.get('processed')).toBe(true);

      // Both middleware should have been called
      expect(console.log).toHaveBeenCalledTimes(3); // before, after, timing
    });

    test('should handle middleware order', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const callOrder = [];

      const middleware1 = {
        before: async () => callOrder.push('before1'),
        after: async () => callOrder.push('after1')
      };

      const middleware2 = {
        before: async () => callOrder.push('before2'),
        after: async () => callOrder.push('after2')
      };

      chain.useMiddleware(middleware1);
      chain.useMiddleware(middleware2);

      const ctx = new Context();
      await chain.run(ctx);

      expect(callOrder).toEqual(['before1', 'before2', 'after1', 'after2']);
    });

    test('should handle middleware errors gracefully', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const errorMiddleware = {
        before: async () => {
          throw new Error('Middleware error');
        }
      };

      chain.useMiddleware(errorMiddleware);

      const ctx = new Context();
      await expect(chain.run(ctx)).rejects.toThrow('Middleware error');
    });
  });

  describe('Middleware Error Handling', () => {
    test('should call onError when link fails', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const failingLink = new TestLink('failing', async () => {
        throw new Error('Link failed');
      });
      chain.addLink(failingLink, 'failing');

      const errorSpy = jest.fn();
      chain.useMiddleware({
        onError: errorSpy
      });

      const ctx = new Context();
      await expect(chain.run(ctx)).rejects.toThrow('Link failed');

      expect(errorSpy).toHaveBeenCalledWith(
        failingLink,
        expect.any(Error),
        expect.any(Object),
        'failing'
      );
    });

    test('should continue with other middleware on error', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const failingLink = new TestLink('failing', async () => {
        throw new Error('Link failed');
      });
      chain.addLink(failingLink, 'failing');

      const beforeSpy = jest.fn();
      const errorSpy = jest.fn();
      const afterSpy = jest.fn();

      chain.useMiddleware({
        before: beforeSpy,
        onError: errorSpy,
        after: afterSpy
      });

      const ctx = new Context();
      await expect(chain.run(ctx)).rejects.toThrow('Link failed');

      expect(beforeSpy).toHaveBeenCalled();
      expect(errorSpy).toHaveBeenCalled();
      expect(afterSpy).not.toHaveBeenCalled(); // Should not be called on error
    });
  });

  describe('Middleware Context Access', () => {
    test('should provide context to middleware', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx.insert('result', 'success'));
      chain.addLink(link, 'test');

      const middleware = {
        before: jest.fn(),
        after: jest.fn()
      };

      chain.useMiddleware(middleware);

      const initialCtx = new Context({ input: 'test' });
      await chain.run(initialCtx);

      expect(middleware.before).toHaveBeenCalledWith(
        link,
        initialCtx,
        'test'
      );

      expect(middleware.after).toHaveBeenCalledWith(
        link,
        expect.objectContaining({
          _data: expect.objectContaining({
            input: 'test',
            result: 'success'
          })
        }),
        'test'
      );
    });

    test('should handle context modifications in middleware', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const middleware = {
        before: async (link, ctx, linkName) => {
          // Middleware can modify context before link execution
          return ctx.insert('middleware', 'modified');
        }
      };

      chain.useMiddleware(middleware);

      const ctx = new Context({ original: 'value' });
      const result = await chain.run(ctx);

      expect(result.get('original')).toBe('value');
      expect(result.get('middleware')).toBeUndefined(); // Middleware modifications don't persist
    });
  });
});