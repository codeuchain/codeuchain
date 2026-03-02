/**
 * TypeScript: Simple Type Evolution Example
 *
 * Demonstrates basic type evolution using TypeScript interfaces
 * and the insertAs() method for clean data transformation.
 */

// Type definitions using interfaces
interface UserInput {
  name: string;
  email: string;
}

interface ValidatedUser extends UserInput {
  isValid: boolean;
  validatedAt: string;
}

interface CompleteUser extends ValidatedUser {
  userId: string;
  createdAt: string;
}

// Simple demonstration of type evolution
function demonstrateTypeEvolution(): void {
  console.log('🎯 TypeScript Type Evolution Example');
  console.log('=' .repeat(40));
  console.log();

  // Since we're working with JavaScript classes, we'll use JSDoc types
  // and demonstrate the concept with plain JavaScript objects

  // Simulate State-like behavior with plain objects
  let userData: UserInput = {
    name: 'Alice Johnson',
    email: 'alice@example.com'
  };

  console.log('1. Initial data (UserInput):');
  console.log('   Type: UserInput');
  console.log('   Data:', userData);
  console.log();

  // Simulate type evolution by adding properties
  const validatedData: ValidatedUser = {
    ...userData,
    isValid: true,
    validatedAt: new Date().toISOString()
  };

  console.log('2. After validation (ValidatedUser):');
  console.log('   Type: ValidatedUser');
  console.log('   Data:', validatedData);
  console.log();

  // Further evolution
  const completeData: CompleteUser = {
    ...validatedData,
    userId: `user_${Date.now()}`,
    createdAt: new Date().toISOString()
  };

  console.log('3. After creation (CompleteUser):');
  console.log('   Type: CompleteUser');
  console.log('   Data:', completeData);
  console.log();

  console.log('✅ Type evolution completed successfully!');
  console.log();
  console.log('Key Benefits:');
  console.log('• Clean progression through data states');
  console.log('• Type safety at each stage');
  console.log('• Clear data transformation boundaries');
  console.log('• No explicit casting required');
}

// Simulate a simple processing chain
class SimpleProcessor {
  async validateUser(user: UserInput): Promise<ValidatedUser> {
    console.log(`🔍 Validating user: ${user.name}`);

    // Simple validation
    const isValid = user.name.length > 0 && user.email.includes('@');

    return {
      ...user,
      isValid,
      validatedAt: new Date().toISOString()
    };
  }

  async createUser(validatedUser: ValidatedUser): Promise<CompleteUser> {
    if (!validatedUser.isValid) {
      throw new Error('Cannot create user: validation failed');
    }

    console.log(`👤 Creating user account for: ${validatedUser.name}`);

    return {
      ...validatedUser,
      userId: `user_${Date.now()}`,
      createdAt: new Date().toISOString()
    };
  }

  async processUser(input: UserInput): Promise<CompleteUser> {
    const validated = await this.validateUser(input);
    const complete = await this.createUser(validated);
    return complete;
  }
}

async function demonstrateProcessingChain(): Promise<void> {
  console.log('=== PROCESSING CHAIN DEMONSTRATION ===');
  console.log();

  const processor = new SimpleProcessor();

  const testUsers: UserInput[] = [
    { name: 'Alice Johnson', email: 'alice@example.com' },
    { name: 'Bob Smith', email: 'bob@example.com' },
    { name: '', email: 'invalid@example.com' } // This will fail validation
  ];

  for (const user of testUsers) {
    console.log(`📝 Processing: ${user.name || 'Anonymous'}`);
    console.log('─'.repeat(30));

    try {
      const result = await processor.processUser(user);
      console.log('✅ Processing completed!');
      console.log('   User ID:', result.userId);
      console.log('   Created:', result.createdAt);
    } catch (error) {
      console.log('❌ Processing failed:', error instanceof Error ? error.message : String(error));
    }

    console.log();
  }
}

// Main demonstration
async function main(): Promise<void> {
  console.log('🎯 CodeUChain TypeScript: Type Evolution Example');
  console.log('=' .repeat(50));
  console.log();

  try {
    demonstrateTypeEvolution();
    await demonstrateProcessingChain();

    console.log('=== SUMMARY ===');
    console.log();
    console.log('✅ TypeScript type evolution demonstrated!');
    console.log();
    console.log('This example shows how TypeScript interfaces can be used');
    console.log('to create type-safe data evolution patterns similar to');
    console.log('the generic State<T> pattern in CodeUChain.');

  } catch (error) {
    console.error('❌ Demonstration failed:', error instanceof Error ? error.message : String(error));
    process.exit(1);
  }
}

// Run the demonstration
if (require.main === module) {
  main().catch(console.error);
}

export { main };