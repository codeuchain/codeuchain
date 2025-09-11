/**
 * Saga with Compensations Example
 *
 * Demonstrates the Saga pattern with compensations from ASCII_PIPELINES.txt:
 * ```
 * (Do Step 1) -> (Do Step 2) -> (Do Step 3)
 *      |             |             |
 *      v             v             v
 *  (Push C1)    (Push C2)     (Push C3)
 *
 * On failure -> Pop & run compensations: C3, C2, C1
 * ```
 *
 * This example shows how to implement distributed transactions
 * with compensation logic for rollback scenarios.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class SagaOrchestrator {
  constructor() {
    this.compensationStack = [];
    this.steps = [];
  }

  addStep(stepLink, compensationLink) {
    this.steps.push({ step: stepLink, compensation: compensationLink });
  }

  async execute(ctx) {
    console.log('🎭 Starting Saga execution...');

    for (let i = 0; i < this.steps.length; i++) {
      const { step, compensation } = this.steps[i];
      const stepName = step.constructor.name;

      try {
        console.log(`📍 Executing step ${i + 1}: ${stepName}`);
        const resultCtx = await step.call(ctx);

        // Push compensation onto stack (LIFO order)
        this.compensationStack.push(compensation);
        console.log(`💾 Compensation ${compensation.constructor.name} pushed to stack`);

        ctx = resultCtx;

      } catch (error) {
        console.log(`❌ Step ${stepName} failed: ${error.message}`);
        console.log('🔄 Initiating compensation sequence...');

        // Execute compensations in reverse order
        await this._executeCompensations(ctx);
        throw error;
      }
    }

    console.log('✅ Saga completed successfully!');
    return ctx;
  }

  async _executeCompensations(ctx) {
    while (this.compensationStack.length > 0) {
      const compensation = this.compensationStack.pop();
      const compName = compensation.constructor.name;

      try {
        console.log(`🔧 Executing compensation: ${compName}`);
        ctx = await compensation.call(ctx);
        console.log(`✅ Compensation ${compName} completed`);
      } catch (compError) {
        console.log(`⚠️ Compensation ${compName} failed: ${compError.message}`);
        // Continue with next compensation even if one fails
      }
    }

    console.log('🔚 Compensation sequence completed');
  }
}

// Saga Steps
class CreateUserAccountLink extends Link {
  async call(ctx) {
    const userData = ctx.get('userData');
    console.log(`👤 Creating user account for: ${userData.email}`);

    // Simulate account creation
    const accountId = `acc_${Date.now()}`;
    console.log(`✅ Account created: ${accountId}`);

    return ctx.insert('accountId', accountId);
  }
}

class AllocateResourcesLink extends Link {
  async call(ctx) {
    const accountId = ctx.get('accountId');
    console.log(`📦 Allocating resources for account: ${accountId}`);

    // Simulate resource allocation
    const resources = {
      storage: '10GB',
      bandwidth: '100GB/month',
      apiCalls: 10000
    };

    console.log(`✅ Resources allocated: ${JSON.stringify(resources)}`);
    return ctx.insert('resources', resources);
  }
}

class SendWelcomeEmailLink extends Link {
  async call(ctx) {
    const userData = ctx.get('userData');
    const accountId = ctx.get('accountId');
    console.log(`📧 Sending welcome email to: ${userData.email}`);

    // Simulate email sending
    const emailId = `email_${Date.now()}`;
    console.log(`✅ Welcome email sent: ${emailId}`);

    return ctx.insert('welcomeEmailId', emailId);
  }
}

class ProcessPaymentLink extends Link {
  async call(ctx) {
    const accountId = ctx.get('accountId');
    console.log(`💳 Processing payment for account: ${accountId}`);

    // Simulate payment processing
    const paymentId = `pay_${Date.now()}`;
    console.log(`✅ Payment processed: ${paymentId}`);

    return ctx.insert('paymentId', paymentId);
  }
}

// Compensation Links
class DeleteUserAccountCompensation extends Link {
  async call(ctx) {
    const accountId = ctx.get('accountId');
    console.log(`🗑️ Compensating: Deleting account ${accountId}`);

    // Simulate account deletion
    console.log(`✅ Account ${accountId} deleted`);
    return ctx;
  }
}

class DeallocateResourcesCompensation extends Link {
  async call(ctx) {
    const resources = ctx.get('resources');
    console.log(`🔄 Compensating: Deallocating resources`);

    // Simulate resource deallocation
    console.log(`✅ Resources deallocated: ${JSON.stringify(resources)}`);
    return ctx;
  }
}

class CancelWelcomeEmailCompensation extends Link {
  async call(ctx) {
    const emailId = ctx.get('welcomeEmailId');
    console.log(`🔄 Compensating: Canceling welcome email ${emailId}`);

    // Simulate email cancellation
    console.log(`✅ Welcome email ${emailId} canceled`);
    return ctx;
  }
}

class RefundPaymentCompensation extends Link {
  async call(ctx) {
    const paymentId = ctx.get('paymentId');
    console.log(`💸 Compensating: Refunding payment ${paymentId}`);

    // Simulate payment refund
    console.log(`✅ Payment ${paymentId} refunded`);
    return ctx;
  }
}

async function main() {
  console.log('🎭 CodeUChain: Saga with Compensations Example');
  console.log('=' * 50);
  console.log();

  // Test scenarios
  const testScenarios = [
    {
      name: 'Successful Saga',
      userData: { email: 'success@example.com', name: 'Success User' },
      shouldFail: false
    },
    {
      name: 'Saga Failing at Email Step',
      userData: { email: 'fail@example.com', name: 'Fail User' },
      shouldFail: true,
      failAtStep: 2 // 0-indexed
    },
    {
      name: 'Saga Failing at Payment Step',
      userData: { email: 'payment-fail@example.com', name: 'Payment Fail User' },
      shouldFail: true,
      failAtStep: 3
    }
  ];

  for (const scenario of testScenarios) {
    console.log(`\n🧪 Testing: ${scenario.name}`);
    console.log('='.repeat(50));

    // Create saga orchestrator
    const saga = new SagaOrchestrator();

    // Add steps with their compensations
    saga.addStep(
      new CreateUserAccountLink(),
      new DeleteUserAccountCompensation()
    );

    saga.addStep(
      new AllocateResourcesLink(),
      new DeallocateResourcesCompensation()
    );

    saga.addStep(
      new SendWelcomeEmailLink(),
      new CancelWelcomeEmailCompensation()
    );

    saga.addStep(
      new ProcessPaymentLink(),
      new RefundPaymentCompensation()
    );

    // Override step to fail if needed
    if (scenario.shouldFail) {
      const originalStep = saga.steps[scenario.failAtStep].step;
      const failingStep = {
        call: async (ctx) => {
          console.log(`💥 Intentionally failing at step ${scenario.failAtStep + 1}`);
          throw new Error(`SIMULATED_FAILURE: Step ${scenario.failAtStep + 1} failed`);
        }
      };
      saga.steps[scenario.failAtStep].step = failingStep;
    }

    try {
      const initialCtx = new Context({ userData: scenario.userData });
      const resultCtx = await saga.execute(initialCtx);

      console.log('✅ Saga completed successfully!');
      console.log('📊 Final context keys:', Object.keys(resultCtx.toObject()));

    } catch (error) {
      console.log('❌ Saga failed and was compensated:', error.message);
    }

    console.log('─'.repeat(60));
  }

  console.log('\n✨ Saga with Compensations Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Saga pattern for distributed transactions');
  console.log('• Compensation logic for rollback scenarios');
  console.log('• LIFO (Last In, First Out) compensation execution');
  console.log('• Failure recovery and cleanup');
  console.log('• Maintaining data consistency across multiple steps');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };