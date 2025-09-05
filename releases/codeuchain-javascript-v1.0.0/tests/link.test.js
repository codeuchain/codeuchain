const { Link, Context } = require('../core');

describe('Link', () => {
  class TestLink extends Link {
    constructor(processor = async (ctx) => ctx) {
      super();
      this.processor = processor;
    }

    async call(ctx) {
      return await this.processor(ctx);
    }
  }

  describe('Base Link Functionality', () => {
    test('should create link instance', () => {
      const link = new TestLink();
      expect(link).toBeInstanceOf(Link);
      expect(link).toBeInstanceOf(TestLink);
    });

    test('should have default name', () => {
      const link = new TestLink();
      expect(link.getName()).toBe('TestLink');
    });

    test('should call processor function', async () => {
      const processor = jest.fn(async (ctx) => ctx.insert('processed', true));
      const link = new TestLink(processor);
      const ctx = new Context({ input: 'test' });

      const result = await link.call(ctx);

      expect(processor).toHaveBeenCalledWith(ctx);
      expect(result.get('processed')).toBe(true);
      expect(result.get('input')).toBe('test');
    });

    test('should validate context with required fields', () => {
      const link = new TestLink();
      const validCtx = new Context({ name: 'Alice', email: 'alice@test.com' });
      const invalidCtx = new Context({ name: 'Alice' });

      expect(() => {
        link.validateContext(validCtx, ['name', 'email']);
      }).not.toThrow();

      expect(() => {
        link.validateContext(invalidCtx, ['name', 'email']);
      }).toThrow('Required field \'email\' is missing from context');
    });

    test('should handle empty required fields array', () => {
      const link = new TestLink();
      const ctx = new Context({});

      expect(() => {
        link.validateContext(ctx, []);
      }).not.toThrow();
    });
  });

  describe('Link Error Handling', () => {
    test('should throw error for unimplemented call method', async () => {
      class BrokenLink extends Link {
        // No call method implemented
      }

      const link = new BrokenLink();
      const ctx = new Context();

      await expect(link.call(ctx)).rejects.toThrow('Link.call() must be implemented by subclass');
    });

    test('should handle async errors in processor', async () => {
      const processor = jest.fn(async () => {
        throw new Error('Processor failed');
      });
      const link = new TestLink(processor);
      const ctx = new Context();

      await expect(link.call(ctx)).rejects.toThrow('Processor failed');
    });
  });

  describe('Link Composition', () => {
    test('should chain multiple links', async () => {
      const link1 = new TestLink(async (ctx) => ctx.insert('step1', true));
      const link2 = new TestLink(async (ctx) => ctx.insert('step2', true));
      const link3 = new TestLink(async (ctx) => ctx.insert('final', 'done'));

      let ctx = new Context({ input: 'start' });
      ctx = await link1.call(ctx);
      ctx = await link2.call(ctx);
      ctx = await link3.call(ctx);

      expect(ctx.get('input')).toBe('start');
      expect(ctx.get('step1')).toBe(true);
      expect(ctx.get('step2')).toBe(true);
      expect(ctx.get('final')).toBe('done');
    });

    test('should handle conditional processing', async () => {
      const conditionalLink = new TestLink(async (ctx) => {
        const shouldProcess = ctx.get('process');
        if (shouldProcess) {
          return ctx.insert('result', 'processed');
        }
        return ctx.insert('result', 'skipped');
      });

      const ctx1 = new Context({ process: true });
      const ctx2 = new Context({ process: false });

      const result1 = await conditionalLink.call(ctx1);
      const result2 = await conditionalLink.call(ctx2);

      expect(result1.get('result')).toBe('processed');
      expect(result2.get('result')).toBe('skipped');
    });
  });

  describe('Link Data Transformation', () => {
    test('should transform data types', async () => {
      const transformLink = new TestLink(async (ctx) => {
        const number = ctx.get('number');
        const doubled = number * 2;
        return ctx.insert('doubled', doubled);
      });

      const ctx = new Context({ number: 5 });
      const result = await transformLink.call(ctx);

      expect(result.get('number')).toBe(5);
      expect(result.get('doubled')).toBe(10);
    });

    test('should handle complex object transformations', async () => {
      const transformLink = new TestLink(async (ctx) => {
        const user = ctx.get('user');
        const processedUser = {
          ...user,
          fullName: `${user.firstName} ${user.lastName}`,
          processedAt: new Date().toISOString()
        };
        return ctx.insert('processedUser', processedUser);
      });

      const ctx = new Context({
        user: { firstName: 'Alice', lastName: 'Johnson', age: 30 }
      });
      const result = await transformLink.call(ctx);

      const processedUser = result.get('processedUser');
      expect(processedUser.firstName).toBe('Alice');
      expect(processedUser.lastName).toBe('Johnson');
      expect(processedUser.fullName).toBe('Alice Johnson');
      expect(processedUser.processedAt).toBeDefined();
    });

    test('should handle array transformations', async () => {
      const arrayLink = new TestLink(async (ctx) => {
        const numbers = ctx.get('numbers');
        const doubled = numbers.map(n => n * 2);
        const sum = doubled.reduce((a, b) => a + b, 0);
        return ctx.insert('doubled', doubled).insert('sum', sum);
      });

      const ctx = new Context({ numbers: [1, 2, 3, 4] });
      const result = await arrayLink.call(ctx);

      expect(result.get('doubled')).toEqual([2, 4, 6, 8]);
      expect(result.get('sum')).toBe(20);
    });
  });

  describe('Link Validation', () => {
    test('should validate email format', async () => {
      const emailValidator = new TestLink(async (ctx) => {
        const email = ctx.get('email');
        if (!email || !email.includes('@')) {
          throw new Error('Invalid email format');
        }
        return ctx.insert('emailValid', true);
      });

      const validCtx = new Context({ email: 'alice@test.com' });
      const invalidCtx = new Context({ email: 'invalid-email' });

      const validResult = await emailValidator.call(validCtx);
      expect(validResult.get('emailValid')).toBe(true);

      await expect(emailValidator.call(invalidCtx)).rejects.toThrow('Invalid email format');
    });

    test('should validate required fields presence', async () => {
      const link = new TestLink(async (ctx) => {
        link.validateContext(ctx, ['name', 'email', 'age']);
        return ctx.insert('validated', true);
      });

      const validCtx = new Context({
        name: 'Alice',
        email: 'alice@test.com',
        age: 30
      });
      const invalidCtx = new Context({
        name: 'Alice',
        email: 'alice@test.com'
        // missing age
      });

      const validResult = await link.call(validCtx);
      expect(validResult.get('validated')).toBe(true);

      await expect(link.call(invalidCtx)).rejects.toThrow('Required field \'age\' is missing from context');
    });
  });
});