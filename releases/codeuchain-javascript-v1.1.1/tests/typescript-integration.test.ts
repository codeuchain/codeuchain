/**
 * CodeUChain TypeScript Integration Tests
 *
 * Tests that validate TypeScript compilation, type imports, and generic type safety.
 * These tests ensure that the TypeScript definitions work correctly and provide
 * proper type checking and IntelliSense support.
 */

// Import types from definition files (I-prefixed named imports)
import type { IState as State, IMutableState as MutableState, ILink as Link, IChain as Chain, IHook as Hook } from '../types';
import { ILoggingHook as LoggingHook, ITimingHook as TimingHook, IValidationHook as ValidationHook } from '../types';

// Import runtime values from JavaScript files
import { State as StateClass, MutableState as MutableStateClass, Link as LinkClass, Chain as ChainClass, Hook as HookClass } from '../core';
import { LoggingHook as LoggingHookClass, TimingHook as TimingHookClass, ValidationHook as ValidationHookClass } from '../core';

// =============================================================================
// TYPE DEFINITIONS FOR TESTING
// =============================================================================

interface UserInput {
  name: string;
  email: string;
}

interface UserValidated extends UserInput {
  isValid: boolean;
  emailVerified: boolean;
}

interface UserProcessed extends UserValidated {
  userId: string;
  age: number;
  profileComplete: boolean;
}

interface ProcessingResult {
  success: boolean;
  message: string;
  data?: any;
}

// =============================================================================
// TEST DATA
// =============================================================================

const testUserInput: UserInput = {
  name: 'Alice Johnson',
  email: 'alice@example.com'
};

const testUserValidated: UserValidated = {
  name: 'Alice Johnson',
  email: 'alice@example.com',
  isValid: true,
  emailVerified: true
};

const testUserProcessed: UserProcessed = {
  name: 'Alice Johnson',
  email: 'alice@example.com',
  isValid: true,
  emailVerified: true,
  userId: 'user_12345',
  age: 28,
  profileComplete: true
};

// =============================================================================
// TYPE-SAFE LINK IMPLEMENTATIONS
// =============================================================================

class ValidateUserLink extends LinkClass<UserInput, UserValidated> {
  async call(ctx: State<UserInput>): Promise<State<UserValidated>> {
    const name = ctx.get('name');
    const email = ctx.get('email');

    if (!name || !email) {
      throw new Error('Name and email are required');
    }

    const isValid = name.length > 0 && email.includes('@');
    const emailVerified = await this.verifyEmail(email);

    return ctx.insertAs<UserValidated>('isValid', isValid)
               .insertAs('emailVerified', emailVerified);
  }

  private async verifyEmail(email: string): Promise<boolean> {
    // Mock email verification
    return email.endsWith('@example.com');
  }
}

class ProcessUserLink extends LinkClass<UserValidated, UserProcessed> {
  async call(ctx: State<UserValidated>): Promise<State<UserProcessed>> {
    const isValid = ctx.get('isValid');
    const emailVerified = ctx.get('emailVerified');

    if (!isValid || !emailVerified) {
      throw new Error('User must be validated and email verified');
    }

    return ctx.insertAs<UserProcessed>('userId', 'user_' + Date.now())
               .insertAs('age', 28)
               .insertAs('profileComplete', true);
  }
}

class ResultLink extends LinkClass<UserProcessed, ProcessingResult> {
  async call(ctx: State<UserProcessed>): Promise<State<ProcessingResult>> {
    const userId = ctx.get('userId');
    const profileComplete = ctx.get('profileComplete');

    return ctx.insertAs<ProcessingResult>('success', profileComplete)
               .insertAs('message', `User ${userId} processed successfully`)
               .insertAs('data', {
                 userId,
                 name: ctx.get('name'),
                 email: ctx.get('email')
               });
  }
}

// =============================================================================
// TYPE-SAFE MIDDLEWARE IMPLEMENTATIONS
// =============================================================================

class TypeValidationHook extends HookClass {
  async before(link: Link<any, any>, ctx: State<any>, linkName: string): Promise<void> {
    // TypeScript should catch type mismatches here
    if (linkName === 'ValidateUserLink') {
      const userCtx = ctx as State<UserInput>;
      const name: string = userCtx.get('name'); // Should be typed as string
      const email: string = userCtx.get('email'); // Should be typed as string
    }
  }

  async after(link: Link<any, any>, ctx: State<any>, linkName: string): Promise<void> {
    // Validate that the state has the expected shape after processing
    if (linkName === 'ProcessUserLink') {
      const processedCtx = ctx as State<UserProcessed>;
      const userId: string = processedCtx.get('userId');
      const age: number = processedCtx.get('age');
      const profileComplete: boolean = processedCtx.get('profileComplete');
    }
  }
}

// =============================================================================
// JEST TEST SUITES
// =============================================================================

describe('TypeScript Import Tests', () => {
  test('should import all types correctly', () => {
    // Test that all expected runtime classes are available
    expect(StateClass).toBeDefined();
    expect(MutableStateClass).toBeDefined();
    expect(LinkClass).toBeDefined();
    expect(ChainClass).toBeDefined();
    expect(HookClass).toBeDefined();
    expect(LoggingHookClass).toBeDefined();
    expect(TimingHookClass).toBeDefined();
    expect(ValidationHookClass).toBeDefined();
  });

  test('should create typed states', () => {
    const userCtx: State<UserInput> = new StateClass(testUserInput);
    const validatedCtx: State<UserValidated> = new StateClass(testUserValidated);
    const processedCtx: State<UserProcessed> = new StateClass(testUserProcessed);

    expect(userCtx).toBeInstanceOf(StateClass);
    expect(validatedCtx).toBeInstanceOf(StateClass);
    expect(processedCtx).toBeInstanceOf(StateClass);
  });

  test('should support generic type inference', () => {
    const inferredCtx = StateClass.from(testUserInput);
    // TypeScript should infer this as State<UserInput>
    const name: string = inferredCtx.get('name');
    const email: string = inferredCtx.get('email');

    expect(typeof name).toBe('string');
    expect(typeof email).toBe('string');
  });
});

describe('Type Evolution Tests', () => {
  test('should support clean type evolution with insertAs', () => {
    const userCtx = new StateClass<UserInput>(testUserInput);

    // TypeScript should enforce that we can only access UserInput properties
    const name: string = userCtx.get('name');
    const email: string = userCtx.get('email');

    // Type evolution to UserValidated
    const validatedCtx = userCtx.insertAs<UserValidated>('isValid', true)
                               .insertAs('emailVerified', true);

    // Now TypeScript knows this state has UserValidated shape
    const isValid: boolean = validatedCtx.get('isValid');
    const emailVerified: boolean = validatedCtx.get('emailVerified');

    expect(isValid).toBe(true);
    expect(emailVerified).toBe(true);
  });

  test('should maintain type safety through multiple evolutions', () => {
    const userCtx = new StateClass<UserInput>(testUserInput);

    // Chain multiple type evolutions
    const finalCtx = userCtx
      .insertAs<UserValidated>('isValid', true)
      .insertAs('emailVerified', true)
      .insertAs<UserProcessed>('userId', 'user_123')
      .insertAs('age', 28)
      .insertAs('profileComplete', true);

    // TypeScript should know all these properties exist
    const name: string = finalCtx.get('name');
    const isValid: boolean = finalCtx.get('isValid');
    const userId: string = finalCtx.get('userId');
    const age: number = finalCtx.get('age');
    const profileComplete: boolean = finalCtx.get('profileComplete');

    expect(name).toBe('Alice Johnson');
    expect(isValid).toBe(true);
    expect(userId).toBe('user_123');
    expect(age).toBe(28);
    expect(profileComplete).toBe(true);
  });

  test('should support mixed typed and untyped operations', () => {
    const typedCtx = new StateClass<UserInput>(testUserInput);

    // TypeScript allows untyped operations but loses type safety
    const untypedCtx = typedCtx.insert('dynamicField', 'any value');

    // This should still work at runtime
    expect(untypedCtx.get('dynamicField')).toBe('any value');
    expect(untypedCtx.get('name')).toBe('Alice Johnson');
  });
});

describe('Generic Link Tests', () => {
  test('should create type-safe links', async () => {
    const validateLink = new ValidateUserLink();
    const processLink = new ProcessUserLink();
    const resultLink = new ResultLink();

    expect(validateLink).toBeInstanceOf(LinkClass);
    expect(processLink).toBeInstanceOf(LinkClass);
    expect(resultLink).toBeInstanceOf(LinkClass);
  });

  test('should enforce type safety in link execution', async () => {
    const validateLink = new ValidateUserLink();
    const userCtx = new StateClass<UserInput>(testUserInput);

    // TypeScript should enforce that input matches UserInput interface
    const resultCtx = await validateLink.call(userCtx);

    // Result should be State<UserValidated>
    const isValid: boolean = resultCtx.get('isValid');
    const emailVerified: boolean = resultCtx.get('emailVerified');

    expect(isValid).toBe(true);
    expect(emailVerified).toBe(true);
  });

  test('should support link chaining with type evolution', async () => {
    const validateLink = new ValidateUserLink();
    const processLink = new ProcessUserLink();
    const resultLink = new ResultLink();

    const userCtx = new StateClass<UserInput>(testUserInput);

    // Chain links with proper type evolution
    const validatedCtx = await validateLink.call(userCtx);
    const processedCtx = await processLink.call(validatedCtx);
    const finalCtx = await resultLink.call(processedCtx);

    // TypeScript should know the final result type
    const success: boolean = finalCtx.get('success');
    const message: string = finalCtx.get('message');
    const data = finalCtx.get('data');

    expect(success).toBe(true);
    expect(message).toContain('processed successfully');
    expect(data).toHaveProperty('userId');
  });
});

describe('Generic Chain Tests', () => {
  test('should create type-safe chains', async () => {
    const chain = new ChainClass<UserInput, ProcessingResult>();

    chain.addLink(new ValidateUserLink(), 'validate');
    chain.addLink(new ProcessUserLink(), 'process');
    chain.addLink(new ResultLink(), 'result');

    chain.connect('validate', 'process');
    chain.connect('process', 'result');

    const userCtx = new StateClass<UserInput>(testUserInput);
    const resultCtx = await chain.run(userCtx);

    // TypeScript should know this is ProcessingResult
    const success: boolean = resultCtx.get('success');
    const message: string = resultCtx.get('message');

    expect(success).toBe(true);
    expect(typeof message).toBe('string');
  });

  test('should support hook with type safety', async () => {
    const chain = new ChainClass<UserInput, UserValidated>();
    const hook = new TypeValidationHook();

    chain.addLink(new ValidateUserLink(), 'validate');
    chain.useHook(hook);

    const userCtx = new StateClass<UserInput>(testUserInput);
    const resultCtx = await chain.run(userCtx);

    // Hook should have been applied
    const isValid: boolean = resultCtx.get('isValid');
    expect(isValid).toBe(true);
  });
});

describe('Type Safety Validation Tests', () => {
  test('should prevent type mismatches at compile time', () => {
    const userCtx = new StateClass<UserInput>(testUserInput);

    // These should work fine
    const name: string = userCtx.get('name');
    const email: string = userCtx.get('email');

    // This would cause a TypeScript error if uncommented:
    // const age: number = userCtx.get('age'); // Error: 'age' does not exist on UserInput

    expect(name).toBe('Alice Johnson');
    expect(email).toBe('alice@example.com');
  });

  test('should validate interface compliance', () => {
    // This should work - matches UserInput interface
    const validUser: UserInput = {
      name: 'Bob Smith',
      email: 'bob@example.com'
    };

    const ctx = new StateClass<UserInput>(validUser);
    expect(ctx.get('name')).toBe('Bob Smith');

    // This would cause TypeScript errors if uncommented:
    // const invalidUser = {
    //   name: 'Charlie Brown',
    //   // missing email - TypeScript error
    // };
  });

  test('should support optional properties correctly', () => {
    interface UserWithOptional {
      name: string;
      email?: string;
      age?: number;
    }

    const userWithOptional: UserWithOptional = {
      name: 'Optional User'
      // email and age are optional
    };

    const ctx = new StateClass<UserWithOptional>(userWithOptional);

    // TypeScript should allow these (may be undefined)
    const name: string = ctx.get('name');
    const email: string | undefined = ctx.get('email');
    const age: number | undefined = ctx.get('age');

    expect(name).toBe('Optional User');
    expect(email).toBeUndefined();
    expect(age).toBeUndefined();
  });
});

describe('Runtime Type Compatibility Tests', () => {
  test('should maintain runtime compatibility with untyped code', () => {
    const typedCtx = new StateClass<UserInput>(testUserInput);
    const untypedCtx = new StateClass(testUserInput);

    // Both should behave identically at runtime
    expect(typedCtx.toObject()).toEqual(untypedCtx.toObject());
    expect(typedCtx.get('name')).toBe(untypedCtx.get('name'));
  });

  test('should support dynamic property access', () => {
    const ctx = new StateClass<UserInput>(testUserInput);

    // TypeScript allows dynamic access but loses type safety
    const dynamicKey = 'name' as keyof UserInput;
    const value: string | undefined = ctx.get(dynamicKey);

    expect(value).toBe('Alice Johnson');
  });

  test('should handle complex nested types', () => {
    interface ComplexUser {
      name: string;
      profile: {
        age: number;
        preferences: string[];
        metadata: Record<string, any>;
      };
    }

    const complexUser: ComplexUser = {
      name: 'Complex User',
      profile: {
        age: 30,
        preferences: ['typescript', 'testing'],
        metadata: { source: 'test', version: '1.0' }
      }
    };

    const ctx = new StateClass<ComplexUser>(complexUser);

    // TypeScript should provide full type safety for nested access
    const profile = ctx.get('profile');
    const age: number = profile.age;
    const preferences: string[] = profile.preferences;
    const metadata = profile.metadata;

    expect(age).toBe(30);
    expect(preferences).toEqual(['typescript', 'testing']);
    expect(metadata.source).toBe('test');
  });
});
