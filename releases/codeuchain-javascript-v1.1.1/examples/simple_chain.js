/**
 * Simple Chain Example
 *
 * Demonstrates basic CodeUChain usage in JavaScript with a user registration flow.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class EmailValidationLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');

    if (!email) {
      throw new Error('Email is required');
    }

    if (!email.includes('@') || !email.includes('.')) {
      throw new Error('Invalid email format');
    }

    console.log(`✅ Email ${email} is valid`);
    return ctx.insert('emailValid', true);
  }
}

class UserCreationLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');
    const name = ctx.get('name');

    if (!name) {
      throw new Error('Name is required');
    }

    // Simulate user creation
    const userId = `user_${Date.now()}`;

    console.log(`👤 Created user ${name} with ID ${userId}`);

    return ctx
      .insert('userId', userId)
      .insert('createdAt', new Date().toISOString())
      .insert('status', 'active');
  }
}

class WelcomeEmailLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');
    const name = ctx.get('name');
    const userId = ctx.get('userId');

    // Simulate sending welcome email
    console.log(`📧 Sent welcome email to ${name} at ${email}`);
    console.log(`   User ID: ${userId}`);

    return ctx.insert('welcomeEmailSent', true);
  }
}

async function main() {
  console.log('🚀 Starting CodeUChain JavaScript Example\n');

  // ===== NEW WAY: Automatic Naming =====
  console.log('✨ Using NEW automatic naming:');
  const autoChain = new Chain();

  // Add links with automatic naming (uses class names)
  autoChain.addLink(new EmailValidationLink());  // → "EmailValidationLink"
  autoChain.addLink(new UserCreationLink());     // → "UserCreationLink"
  autoChain.addLink(new WelcomeEmailLink());     // → "WelcomeEmailLink"

  // Connect using auto-generated names
  autoChain.connect('EmailValidationLink', 'UserCreationLink');
  autoChain.connect('UserCreationLink', 'WelcomeEmailLink');

  console.log('🔗 Auto-named links:', autoChain.getLinkNames());

  // ===== OLD WAY: Manual Naming (still supported) =====
  console.log('\n📝 Using OLD manual naming:');
  const manualChain = new Chain();

  // Add links with manual naming (new signature: link first, name second)
  manualChain.addLink(new EmailValidationLink(), 'validate');
  manualChain.addLink(new UserCreationLink(), 'create');
  manualChain.addLink(new WelcomeEmailLink(), 'welcome');

  // Connect links in sequence
  manualChain.connect('validate', 'create');
  manualChain.connect('create', 'welcome');

  console.log('🔗 Manually named links:', manualChain.getLinkNames());

  // ===== MIXED APPROACH =====
  console.log('\n🎯 Using MIXED naming:');
  const mixedChain = new Chain();

  // Mix automatic and custom naming
  mixedChain.addLink(new EmailValidationLink());        // Auto: "EmailValidationLink"
  mixedChain.addLink(new UserCreationLink(), 'user_creator'); // Custom: "user_creator"
  mixedChain.addLink(new WelcomeEmailLink());           // Auto: "WelcomeEmailLink"

  // Connect using the names
  mixedChain.connect('EmailValidationLink', 'user_creator');
  mixedChain.connect('user_creator', 'WelcomeEmailLink');

  console.log('🔗 Mixed named links:', mixedChain.getLinkNames());

  // Add middleware and error handling to the mixed chain
  mixedChain.useMiddleware(new LoggingMiddleware());
  mixedChain.onError((error, ctx, linkName) => {
    console.error(`❌ Error in ${linkName}: ${error.message}`);
  });

  // Test with mixed chain
  const testUsers = [
    { name: 'Alice Johnson', email: 'alice@example.com' },
    { name: 'Bob Smith', email: 'bob@example.com' },
    { name: 'Charlie Brown', email: 'invalid-email' }, // This will fail
  ];

  console.log('\n🧪 Testing with mixed naming chain:');
  for (const user of testUsers) {
    console.log(`\n📝 Processing user: ${user.name}`);

    try {
      const initialCtx = new Context(user);
      const resultCtx = await mixedChain.run(initialCtx);

      console.log('✅ Registration completed successfully!');
      console.log('📊 Final context keys:', Object.keys(resultCtx.toObject()));
    } catch (error) {
      console.log('❌ Registration failed:', error.message);
    }

    console.log('─'.repeat(50));
  }

  console.log('\n✨ CodeUChain JavaScript example completed!');
  console.log('\n📚 Key Improvements:');
  console.log('  • addLink(link) - automatic naming using class name');
  console.log('  • addLink(link, "custom") - custom naming when needed');
  console.log('  • Backward compatibility maintained');
  console.log('  • Less typing, better developer experience!');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };