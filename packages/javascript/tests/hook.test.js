const { LoggingHook, TimingHook, ValidationHook, Link, State } = require('../core');

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

describe('Hook', () => {
  describe('LoggingHook', () => {
    let loggingHook;
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      loggingHook = new LoggingHook();
      mockLink = new TestLink('test');
      mockCtx = new State({ test: 'data' });
    });

    test('should log before link execution', async () => {
      await loggingHook.before(mockLink, mockCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Starting test')
      );
    });

    test('should log after link execution', async () => {
      const resultCtx = new State({ result: 'success' });
      await loggingHook.after(mockLink, resultCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Completed test')
      );
    });

    test('should log errors', async () => {
      const error = new Error('Test error');
      await loggingHook.onError(mockLink, error, mockCtx, 'test');

      expect(console.error).toHaveBeenCalledWith(
        expect.stringContaining('Error in test: Test error')
      );
    });

    test('should handle missing result in after logging', async () => {
      const resultCtx = new State({}); // No result field
      await loggingHook.after(mockLink, resultCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringContaining('Completed test')
      );
    });
  });

  describe('TimingHook', () => {
    let timingHook;
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      timingHook = new TimingHook();
      mockLink = new TestLink('test');
      mockCtx = new State({ test: 'data' });
    });

    test('should measure execution time', async () => {
      await timingHook.before(mockLink, mockCtx, 'test');

      // Simulate some processing time
      await new Promise(resolve => setTimeout(resolve, 10));

      await timingHook.after(mockLink, mockCtx, 'test');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/test executed in \d+ms/)
      );
    });

    test('should handle multiple links independently', async () => {
      const link1 = new TestLink('link1');
      const link2 = new TestLink('link2');

      await timingHook.before(link1, mockCtx, 'link1');
      await timingHook.before(link2, mockCtx, 'link2');

      await timingHook.after(link1, mockCtx, 'link1');
      await timingHook.after(link2, mockCtx, 'link2');

      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/link1 executed in \d+ms/)
      );
      expect(console.log).toHaveBeenCalledWith(
        expect.stringMatching(/link2 executed in \d+ms/)
      );
    });

    test('should handle missing start time', async () => {
      // Call after without before - should not log
      await timingHook.after(mockLink, mockCtx, 'test');

      expect(console.log).not.toHaveBeenCalled();
    });
  });

  describe('ValidationHook', () => {
    let mockLink;
    let mockCtx;

    beforeEach(() => {
      mockLink = new TestLink('test');
      mockCtx = new State({ name: 'Alice', email: 'alice@test.com' });
    });

    test('should validate before execution', async () => {
      const beforeValidator = jest.fn();
      const validationHook = new ValidationHook({
        beforeValidator
      });

      await validationHook.before(mockLink, mockCtx, 'test');

      expect(beforeValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should validate after execution', async () => {
      const afterValidator = jest.fn();
      const validationHook = new ValidationHook({
        afterValidator
      });

      await validationHook.after(mockLink, mockCtx, 'test');

      expect(afterValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should throw error on before validation failure', async () => {
      const beforeValidator = jest.fn(() => {
        throw new Error('Validation failed');
      });
      const validationHook = new ValidationHook({
        beforeValidator
      });

      await expect(
        validationHook.before(mockLink, mockCtx, 'test')
      ).rejects.toThrow('Pre-validation failed for test: Validation failed');
    });

    test('should throw error on after validation failure', async () => {
      const afterValidator = jest.fn(() => {
        throw new Error('Post-validation failed');
      });
      const validationHook = new ValidationHook({
        afterValidator
      });

      await expect(
        validationHook.after(mockLink, mockCtx, 'test')
      ).rejects.toThrow('Post-validation failed for test: Post-validation failed');
    });

    test('should handle async validators', async () => {
      const beforeValidator = jest.fn(async () => {
        await new Promise(resolve => setTimeout(resolve, 10));
        return true;
      });

      const validationHook = new ValidationHook({
        beforeValidator
      });

      await validationHook.before(mockLink, mockCtx, 'test');

      expect(beforeValidator).toHaveBeenCalledWith(mockCtx, 'test');
    });

    test('should work without validators', async () => {
      const validationHook = new ValidationHook();

      await expect(
        validationHook.before(mockLink, mockCtx, 'test')
      ).resolves.toBeUndefined();

      await expect(
        validationHook.after(mockLink, mockCtx, 'test')
      ).resolves.toBeUndefined();
    });
  });

  describe('Hook Integration', () => {
    test('should combine multiple hook', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx.insert('processed', true));
      chain.addLink(link, 'test');

      // Add multiple hook
      chain.useHook(new LoggingHook());
      chain.useHook(new TimingHook());

      const ctx = new State({ input: 'test' });
      const result = await chain.run(ctx);

      expect(result.get('processed')).toBe(true);

      // Both hook should have been called
      expect(console.log).toHaveBeenCalledTimes(3); // before, after, timing
    });

    test('should handle hook order', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const callOrder = [];

      const hook1 = {
        before: async () => callOrder.push('before1'),
        after: async () => callOrder.push('after1')
      };

      const hook2 = {
        before: async () => callOrder.push('before2'),
        after: async () => callOrder.push('after2')
      };

      chain.useHook(hook1);
      chain.useHook(hook2);

      const ctx = new State();
      await chain.run(ctx);

      expect(callOrder).toEqual(['before1', 'before2', 'after1', 'after2']);
    });

    test('should handle hook errors gracefully', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const errorHook = {
        before: async () => {
          throw new Error('Hook error');
        }
      };

      chain.useHook(errorHook);

      const ctx = new State();
      await expect(chain.run(ctx)).rejects.toThrow('Hook error');
    });
  });

  describe('Hook Error Handling', () => {
    test('should call onError when link fails', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const failingLink = new TestLink('failing', async () => {
        throw new Error('Link failed');
      });
      chain.addLink(failingLink, 'failing');

      const errorSpy = jest.fn();
      chain.useHook({
        onError: errorSpy
      });

      const ctx = new State();
      await expect(chain.run(ctx)).rejects.toThrow('Link failed');

      expect(errorSpy).toHaveBeenCalledWith(
        failingLink,
        expect.any(Error),
        expect.any(Object),
        'failing'
      );
    });

    test('should continue with other hook on error', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const failingLink = new TestLink('failing', async () => {
        throw new Error('Link failed');
      });
      chain.addLink(failingLink, 'failing');

      const beforeSpy = jest.fn();
      const errorSpy = jest.fn();
      const afterSpy = jest.fn();

      chain.useHook({
        before: beforeSpy,
        onError: errorSpy,
        after: afterSpy
      });

      const ctx = new State();
      await expect(chain.run(ctx)).rejects.toThrow('Link failed');

      expect(beforeSpy).toHaveBeenCalled();
      expect(errorSpy).toHaveBeenCalled();
      expect(afterSpy).not.toHaveBeenCalled(); // Should not be called on error
    });
  });

  describe('Hook State Access', () => {
    test('should provide state to hook', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx.insert('result', 'success'));
      chain.addLink(link, 'test');

      const hook = {
        before: jest.fn(),
        after: jest.fn()
      };

      chain.useHook(hook);

      const initialCtx = new State({ input: 'test' });
      await chain.run(initialCtx);

      expect(hook.before).toHaveBeenCalledWith(
        link,
        initialCtx,
        'test'
      );

      expect(hook.after).toHaveBeenCalledWith(
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

    test('should handle state modifications in hook', async () => {
      const { Chain } = require('../core');
      const chain = new Chain();

      const link = new TestLink('test', async (ctx) => ctx);
      chain.addLink(link, 'test');

      const hook = {
        before: async (link, ctx, linkName) => {
          // Hook can modify state before link execution
          return ctx.insert('hook', 'modified');
        }
      };

      chain.useHook(hook);

      const ctx = new State({ original: 'value' });
      const result = await chain.run(ctx);

      expect(result.get('original')).toBe('value');
      expect(result.get('hook')).toBe('modified'); // Hook modifications now persist
    });
  });
});