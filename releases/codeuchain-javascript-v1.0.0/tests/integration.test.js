const { Context, Chain, Link, LoggingMiddleware, TimingMiddleware, ValidationMiddleware } = require('../core');

class EmailValidationLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');
    if (!email) {
      throw new Error('Email is required');
    }
    if (!email.includes('@')) {
      throw new Error('Invalid email format');
    }
    return ctx.insert('emailValid', true);
  }
  getName() { return 'EmailValidationLink'; }
}

class UserCreationLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');
    const name = ctx.get('name');

    if (!name) {
      throw new Error('Name is required');
    }
    if (!email) {
      throw new Error('Email is required');
    }

    const userId = `user_${Date.now()}`;
    return ctx
      .insert('userId', userId)
      .insert('createdAt', new Date().toISOString())
      .insert('status', 'active');
  }
  getName() { return 'UserCreationLink'; }
}

class WelcomeEmailLink extends Link {
  async call(ctx) {
    const userId = ctx.get('userId');
    const email = ctx.get('email');

    // Simulate email sending
    console.log(`📧 Welcome email sent to ${email} for user ${userId}`);

    return ctx.insert('welcomeEmailSent', true);
  }
  getName() { return 'WelcomeEmailLink'; }
}

class DataValidationMiddleware extends ValidationMiddleware {
  constructor() {
    super({
      beforeValidator: async (ctx, linkName) => {
        if (linkName === 'UserCreationLink') {
          if (!ctx.get('email') || !ctx.get('name')) {
            throw new Error('Email and name are required for user creation');
          }
        }
      },
      afterValidator: async (ctx, linkName) => {
        if (linkName === 'EmailValidationLink') {
          if (!ctx.get('emailValid')) {
            throw new Error('Email validation failed');
          }
        }
      }
    });
  }
}

describe('Integration Tests', () => {
  describe('User Registration Flow', () => {
    let registrationChain;

    beforeEach(() => {
      registrationChain = new Chain();

      // Add links
      registrationChain.addLink(new EmailValidationLink(), 'validate');
      registrationChain.addLink(new UserCreationLink(), 'create');
      registrationChain.addLink(new WelcomeEmailLink(), 'welcome');

      // Connect links
      registrationChain.connect('validate', 'create');
      registrationChain.connect('create', 'welcome');

      // Add middleware
      registrationChain.useMiddleware(new LoggingMiddleware());
      registrationChain.useMiddleware(new TimingMiddleware());
      registrationChain.useMiddleware(new DataValidationMiddleware());

      // Add error handling
      registrationChain.onError((error, ctx, linkName) => {
        console.error(`❌ Registration error in ${linkName}: ${error.message}`);
        // Could add error recovery logic here
      });
    });

    test('should successfully register a user', async () => {
      const userData = {
        name: 'Alice Johnson',
        email: 'alice@example.com'
      };

      const initialCtx = new Context(userData);
      const result = await registrationChain.run(initialCtx);

      // Verify the chain executed successfully (full chain execution)
      expect(result.get('name')).toBe('Alice Johnson');
      expect(result.get('email')).toBe('alice@example.com');
      expect(result.get('emailValid')).toBe(true);
      // Full chain executes: validate -> createUser -> sendWelcomeEmail
      expect(result.get('userId')).toBeDefined();
      expect(result.get('createdAt')).toBeDefined();
      expect(result.get('status')).toBe('active');
      expect(result.get('welcomeEmailSent')).toBe(true);
    });

    test('should handle invalid email', async () => {
      const userData = {
        name: 'Bob Smith',
        email: 'invalid-email'
      };

      const initialCtx = new Context(userData);

      await expect(registrationChain.run(initialCtx)).rejects.toThrow('Invalid email format');
    });

    test('should handle missing name', async () => {
      const userData = {
        email: 'bob@example.com'
        // missing name
      };

      const initialCtx = new Context(userData);

      await expect(registrationChain.run(initialCtx)).rejects.toThrow('Name is required');
    });

    test('should handle validation middleware failure', async () => {
      const userData = {
        // missing email
        name: 'Bob'
      };

      const initialCtx = new Context(userData);

      await expect(registrationChain.run(initialCtx)).rejects.toThrow('Email is required');
    });
  });

  describe('Complex Chain Scenarios', () => {
    test('should handle conditional branching', async () => {
      const chain = new Chain();

      // Router link
      class RouterLink extends Link {
        async call(ctx) {
          const userType = ctx.get('userType');
          return ctx.insert('route', userType === 'admin' ? 'admin' : 'user');
        }
        getName() { return 'RouterLink'; }
      }

      // Different processing links
      class AdminLink extends Link {
        async call(ctx) {
          return ctx.insert('permissions', ['read', 'write', 'delete']);
        }
        getName() { return 'AdminLink'; }
      }

      class UserLink extends Link {
        async call(ctx) {
          return ctx.insert('permissions', ['read']);
        }
        getName() { return 'UserLink'; }
      }

      chain.addLink(new RouterLink(), 'router');
      chain.addLink(new AdminLink(), 'admin');
      chain.addLink(new UserLink(), 'user');

      chain.connect('router', 'admin', (ctx) => ctx.get('route') === 'admin');
      chain.connect('router', 'user', (ctx) => ctx.get('route') === 'user');

      // Test admin path (full chain executes based on condition)
      const adminCtx = new Context({ userType: 'admin' });
      const adminResult = await chain.run(adminCtx);
      expect(adminResult.get('route')).toBe('admin');
      // Conditional execution: router -> admin (condition met)
      expect(adminResult.get('permissions')).toEqual(['read', 'write', 'delete']);

      // Test user path
      const userCtx = new Context({ userType: 'user' });
      const userResult = await chain.run(userCtx);
      expect(userResult.get('route')).toBe('user');
      // Conditional execution: router -> user (condition met)
      expect(userResult.get('permissions')).toEqual(['read']);
    });

    test('should handle error recovery', async () => {
      const chain = new Chain();

      class UnreliableLink extends Link {
        constructor(shouldFail = false) {
          super();
          this.shouldFail = shouldFail;
        }

        async call(ctx) {
          if (this.shouldFail) {
            throw new Error('Simulated failure');
          }
          return ctx.insert('processed', true);
        }
        getName() { return 'UnreliableLink'; }
      }

      class RecoveryLink extends Link {
        async call(ctx) {
          return ctx.insert('recovered', true).insert('error', null);
        }
        getName() { return 'RecoveryLink'; }
      }

      chain.addLink(new UnreliableLink(true), 'unreliable');
      chain.addLink(new RecoveryLink(), 'recovery');

      // Add error recovery middleware
      chain.useMiddleware({
        onError: async (link, error, ctx, linkName) => {
          console.log(`Recovering from error in ${linkName}`);
          // In a real scenario, you might trigger the recovery link
        }
      });

      const ctx = new Context({ input: 'test' });

      // This will fail, but we test that error handling works
      await expect(chain.run(ctx)).rejects.toThrow('Simulated failure');
    });

    test('should handle data transformation pipeline', async () => {
      const chain = new Chain();

      class DataParser extends Link {
        async call(ctx) {
          const rawData = ctx.get('rawData');
          const parsed = JSON.parse(rawData);
          return ctx.insert('parsed', parsed);
        }
        getName() { return 'DataParser'; }
      }

      class DataValidator extends Link {
        async call(ctx) {
          const parsed = ctx.get('parsed');
          if (!parsed.firstName || !parsed.lastName || !parsed.email) {
            throw new Error('Invalid data structure');
          }
          return ctx.insert('validated', true);
        }
        getName() { return 'DataValidator'; }
      }

      class DataTransformer extends Link {
        async call(ctx) {
          const parsed = ctx.get('parsed');
          const transformed = {
            fullName: `${parsed.firstName} ${parsed.lastName}`,
            contact: parsed.email,
            metadata: {
              processedAt: new Date().toISOString(),
              source: 'api'
            }
          };
          return ctx.insert('transformed', transformed);
        }
        getName() { return 'DataTransformer'; }
      }

      chain.addLink(new DataParser(), 'parse');
      chain.addLink(new DataValidator(), 'validate');
      chain.addLink(new DataTransformer(), 'transform');

      chain.connect('parse', 'validate');
      chain.connect('validate', 'transform');

      const rawData = JSON.stringify({
        firstName: 'Alice',
        lastName: 'Johnson',
        email: 'alice@example.com'
      });

      const initialCtx = new Context({ rawData });
      const result = await chain.run(initialCtx);

      // Full chain executes: parse -> validate -> transform
      expect(result.get('parsed')).toEqual({
        firstName: 'Alice',
        lastName: 'Johnson',
        email: 'alice@example.com'
      });

      // All subsequent links execute
      expect(result.get('validated')).toBe(true);
      expect(result.get('transformed')).toBeDefined();
      expect(result.get('transformed').fullName).toBe('Alice Johnson');
      expect(result.get('transformed').contact).toBe('alice@example.com');
    });
  });

  describe('Performance and Scalability', () => {
    test('should handle large contexts efficiently', async () => {
      const chain = new Chain();

      class LargeDataProcessor extends Link {
        async call(ctx) {
          // Simulate processing large data
          const data = ctx.get('largeData');
          const processed = data.map(item => ({ ...item, processed: true }));
          return ctx.insert('processedData', processed);
        }
        getName() { return 'LargeDataProcessor'; }
      }

      chain.addLink(new LargeDataProcessor(), 'process');

      // Create large dataset
      const largeData = Array.from({ length: 1000 }, (_, i) => ({
        id: i,
        value: `item_${i}`,
        timestamp: Date.now()
      }));

      const initialCtx = new Context({ largeData });
      const result = await chain.run(initialCtx);

      const processedData = result.get('processedData');
      expect(processedData).toHaveLength(1000);
      expect(processedData[0].processed).toBe(true);
      expect(processedData[999].processed).toBe(true);
    });

    test('should handle concurrent chain executions', async () => {
      const createChain = () => {
        const chain = new Chain();
        const link = new Link();
        link.call = async (ctx) => {
          // Simulate async work
          await new Promise(resolve => setTimeout(resolve, 10));
          return ctx.insert('processed', true);
        };
        chain.addLink(link, 'test');
        return chain;
      };

      const chains = Array.from({ length: 10 }, () => createChain());
      const contexts = Array.from({ length: 10 }, (_, i) =>
        new Context({ id: i })
      );

      // Run all chains concurrently
      const promises = chains.map((chain, i) => chain.run(contexts[i]));
      const results = await Promise.all(promises);

      results.forEach((result, i) => {
        expect(result.get('processed')).toBe(true);
        expect(result.get('id')).toBe(i);
      });
    });
  });

  describe('Real-world Scenarios', () => {
    test('should handle API request processing', async () => {
      const chain = new Chain();

      class AuthMiddleware extends Link {
        async call(ctx) {
          const token = ctx.get('token');
          if (!token) {
            throw new Error('Authentication required');
          }
          return ctx.insert('user', { id: 123, role: 'user' });
        }
        getName() { return 'AuthMiddleware'; }
      }

      class RequestValidator extends Link {
        async call(ctx) {
          const body = ctx.get('body');
          if (!body.action || !body.data) {
            throw new Error('Invalid request format');
          }
          return ctx.insert('validated', true);
        }
        getName() { return 'RequestValidator'; }
      }

      class BusinessLogic extends Link {
        async call(ctx) {
          const body = ctx.get('body');
          const user = ctx.get('user');

          let result;
          switch (body.action) {
            case 'create':
              result = { id: Date.now(), ...body.data, createdBy: user.id };
              break;
            case 'update':
              result = { ...body.data, updatedBy: user.id, updatedAt: new Date().toISOString() };
              break;
            default:
              throw new Error('Unknown action');
          }

          return ctx.insert('result', result);
        }
        getName() { return 'BusinessLogic'; }
      }

      chain.addLink(new AuthMiddleware(), 'auth');
      chain.addLink(new RequestValidator(), 'validate');
      chain.addLink(new BusinessLogic(), 'process');

      chain.connect('auth', 'validate');
      chain.connect('validate', 'process');

      // Simulate API request
      const apiRequest = {
        token: 'valid-token',
        body: {
          action: 'create',
          data: { name: 'New Item', value: 100 }
        }
      };

      const initialCtx = new Context(apiRequest);
      const result = await chain.run(initialCtx);

      // Full chain executes: auth -> validate -> process
      expect(result.get('user')).toEqual({ id: 123, role: 'user' });
      // All subsequent links execute
      expect(result.get('validated')).toBe(true);
      expect(result.get('result')).toBeDefined();
      expect(result.get('result').name).toBe('New Item');
      expect(result.get('result').createdBy).toBe(123);
    });

    test('should handle workflow with approvals', async () => {
      const chain = new Chain();

      class SubmissionValidator extends Link {
        async call(ctx) {
          const submission = ctx.get('submission');
          if (!submission.title || !submission.content) {
            throw new Error('Invalid submission');
          }
          return ctx.insert('validated', true);
        }
        getName() { return 'SubmissionValidator'; }
      }

      class AutoApproval extends Link {
        async call(ctx) {
          const submission = ctx.get('submission');
          const needsApproval = submission.content.length > 1000;
          return ctx.insert('needsApproval', needsApproval);
        }
        getName() { return 'AutoApproval'; }
      }

      class ApprovalProcess extends Link {
        async call(ctx) {
          const needsApproval = ctx.get('needsApproval');
          if (needsApproval) {
            return ctx.insert('status', 'pending_approval');
          } else {
            return ctx.insert('status', 'approved');
          }
        }
        getName() { return 'ApprovalProcess'; }
      }

      class NotificationSender extends Link {
        async call(ctx) {
          const status = ctx.get('status');
          const submission = ctx.get('submission');

          const message = status === 'approved'
            ? `Submission "${submission.title}" has been approved`
            : `Submission "${submission.title}" requires approval`;

          return ctx.insert('notification', message);
        }
        getName() { return 'NotificationSender'; }
      }

      chain.addLink(new SubmissionValidator(), 'validate');
      chain.addLink(new AutoApproval(), 'autoApprove');
      chain.addLink(new ApprovalProcess(), 'approve');
      chain.addLink(new NotificationSender(), 'notify');

      chain.connect('validate', 'autoApprove');
      chain.connect('autoApprove', 'approve');
      chain.connect('approve', 'notify');

      // Define test submissions
      const shortSubmission = {
        title: 'Short Article',
        content: 'This is a short article with less than 1000 characters.'
      };

      const longSubmission = {
        title: 'Long Article',
        content: 'A'.repeat(1500) // Long content that exceeds 1000 characters
      };

      const shortCtx = new Context({ submission: shortSubmission });
      const shortResult = await chain.run(shortCtx);

      // Full chain executes: validate -> autoApprove -> approve -> notify
      expect(shortResult.get('validated')).toBe(true);
      expect(shortResult.get('needsApproval')).toBe(false); // Short content doesn't need approval
      expect(shortResult.get('status')).toBe('approved');
      expect(shortResult.get('notification')).toBe('Submission "Short Article" has been approved');

      const longCtx = new Context({ submission: longSubmission });
      const longResult = await chain.run(longCtx);

      // Full chain executes: validate -> autoApprove -> approve -> notify
      expect(longResult.get('validated')).toBe(true);
      expect(longResult.get('needsApproval')).toBe(true); // Long content needs approval
      expect(longResult.get('status')).toBe('pending_approval');
      expect(longResult.get('notification')).toBe('Submission "Long Article" requires approval');
    });
  });
});