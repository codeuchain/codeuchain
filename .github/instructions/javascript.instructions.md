---
applyTo: 'packages/javascript/**'
---

# JavaScript/TypeScript Development Instructions

## 📦 JavaScript/TypeScript-Specific Guidelines

CodeUChain's JavaScript/TypeScript implementation provides strong typing support while maintaining full JavaScript compatibility.

## Project Structure

```
packages/javascript/
├── src/
│   ├── core/          # Core framework (Context, Link, Chain)
│   ├── middleware/    # Middleware implementations
│   └── types/         # TypeScript type definitions
├── tests/            # Jest test suite
├── examples/         # Example implementations
└── dist/            # Compiled output
```

## Development Setup

### Prerequisites
- Node.js 16+ or 18+
- npm or yarn

### Installation
```bash
cd packages/javascript
npm install              # Install dependencies
npm run build           # Build the project
```

### Running Tests
```bash
cd packages/javascript
npm test                # Run all tests
npm test -- --watch     # Watch mode
npm test -- test_file   # Run specific test
npm run test:coverage   # Run with coverage
```

### Running Linters
```bash
cd packages/javascript
npm run lint            # Check for issues
npm run lint:fix        # Auto-fix issues
npm run format          # Format with Prettier
```

## Code Style

### TypeScript Configuration
- Use strict mode
- Enable all strict checks
- Target ES2020 or higher

### Imports
```typescript
// Node built-ins
import { readFile } from 'fs/promises';

// Third-party
import { describe, test, expect } from '@jest/globals';

// Local
import { Context, Link, Chain } from '../core';
import type { InputData, OutputData } from '../types';
```

### Type Definitions
```typescript
// Use interfaces for public APIs
interface ILink<TInput = any, TOutput = any> {
  call(ctx: Context<TInput>): Promise<Context<TOutput>>;
}

// Use types for data shapes
type InputData = {
  numbers: number[];
  metadata?: Record<string, unknown>;
};

// Export types
export type { InputData, OutputData };
```

### JSDoc (for JavaScript files)
```javascript
/**
 * Process input data and transform context.
 * @param {Context<InputData>} ctx - Context with input data
 * @returns {Promise<Context<OutputData>>} Context with result
 * @example
 * const ctx = new Context({ numbers: [1, 2, 3] });
 * const result = await processData(ctx);
 * console.log(result.get('result')); // 6
 */
async function processData(ctx) {
  // Implementation
}
```

## Testing Patterns

### Basic Test Structure (Jest)
```typescript
import { describe, test, expect, beforeEach } from '@jest/globals';
import { Context, Link } from '../core';

describe('MyLink', () => {
  let link: MyLink;
  
  beforeEach(() => {
    link = new MyLink();
  });
  
  test('should process input correctly', async () => {
    // Arrange
    const ctx = new Context({ input: 'value' });
    
    // Act
    const result = await link.call(ctx);
    
    // Assert
    expect(result.get('output')).toBe('expected');
  });
});
```

### Async Tests
```typescript
describe('AsyncLink', () => {
  test('should handle async operations', async () => {
    const ctx = new Context({ data: 'test' });
    const link = new AsyncLink();
    
    const result = await link.call(ctx);
    
    expect(result.get('processed')).toBeDefined();
  });
});
```

### Type Tests
```typescript
import type { InputData, OutputData } from '../types';

describe('TypedLink', () => {
  test('should work with typed context', async () => {
    const ctx = new Context<InputData>({ 
      numbers: [1, 2, 3] 
    });
    const link = new SumLink();
    
    const result = await link.call(ctx);
    
    expect(result.get('result')).toBe(6);
  });
});
```

## Common Patterns

### Creating a Simple Link (TypeScript)
```typescript
import { Link, Context } from '../core';

export class MyLink implements Link {
  async call(ctx: Context): Promise<Context> {
    // Get value from context
    const value = ctx.get('input_key');
    
    // Process value
    const result = this.process(value);
    
    // Return new context with result
    return ctx.insert('output_key', result);
  }
  
  private process(value: any): any {
    // Your processing logic
    return value;
  }
}
```

### Creating a Generic Link (TypeScript)
```typescript
import { Link, Context } from '../core';

type InputData = {
  numbers: number[];
};

type OutputData = {
  result: number;
};

export class SumLink implements Link<InputData, OutputData> {
  async call(ctx: Context<InputData>): Promise<Context<OutputData>> {
    const numbers = ctx.get('numbers') as number[];
    const total = numbers.reduce((sum, n) => sum + n, 0);
    
    return ctx.insertAs<OutputData>('result', total);
  }
}
```

### Creating a Link (JavaScript)
```javascript
export class MyLink {
  async call(ctx) {
    const value = ctx.get('input_key');
    const result = this.process(value);
    return ctx.insert('output_key', result);
  }
  
  process(value) {
    return value;
  }
}
```

### Creating a Chain
```typescript
import { Chain } from '../core';

// Simple chain
const chain = new Chain([
  new Link1(),
  new Link2(),
  new Link3()
]);

// Execute chain
const initialCtx = new Context({ input: 'data' });
const result = await chain.execute(initialCtx);
```

### Using Middleware
```typescript
import { Chain } from '../core';
import { LoggingMiddleware } from '../middleware';

const chain = new Chain(
  [new Link1(), new Link2()],
  { middleware: [new LoggingMiddleware()] }
);
```

## Type Evolution

### Using insert() - Preserves Type
```typescript
const ctx: Context<InputData> = new Context({ numbers: [1, 2, 3] });
// Type is still Context<InputData>
const ctx2 = ctx.insert('extra', 'value');
```

### Using insertAs() - Changes Type
```typescript
const ctx: Context<InputData> = new Context({ numbers: [1, 2, 3] });
// Type is now Context<OutputData>
const ctx2 = ctx.insertAs<OutputData>('result', 6);
```

## Error Handling

### Link-Level Errors
```typescript
export class MyLink implements Link {
  async call(ctx: Context): Promise<Context> {
    try {
      const result = await this.riskyOperation(ctx);
      return ctx.insert('result', result);
    } catch (error) {
      if (error instanceof ValueError) {
        return ctx.insert('error', error.message);
      }
      throw error;
    }
  }
}
```

### Chain-Level Errors
```typescript
try {
  const result = await chain.execute(ctx);
} catch (error) {
  if (error instanceof ChainExecutionError) {
    console.error('Chain failed:', error.message);
  }
}
```

## Best Practices

### Context Management
- **Immutable**: Always create new Context instances
- **Type Safety**: Use TypeScript for type checking
- **Clear Keys**: Use descriptive string keys

### Link Design
- **Single Responsibility**: Each link does one thing
- **Async/Await**: Use promises consistently
- **Error Handling**: Handle errors appropriately

### TypeScript Usage
- **Strict Mode**: Enable strict TypeScript checking
- **Type Inference**: Let TypeScript infer when possible
- **Explicit Types**: Use explicit types for public APIs
- **Avoid `any`**: Use `unknown` instead of `any` when type is truly unknown

### Performance
- **Async Operations**: Use async/await for I/O
- **Minimal Copying**: Context uses efficient structure
- **Lazy Evaluation**: Compute only when needed

## Debugging

### Console Logging
```typescript
const ctx = new Context({ key: 'value' });
console.log(ctx.toObject());  // View all context data
```

### Debug Link Execution
```typescript
export class DebugLink implements Link {
  async call(ctx: Context): Promise<Context> {
    console.log('Before:', ctx.toObject());
    const result = await this.process(ctx);
    console.log('After:', result.toObject());
    return result;
  }
}
```

### Using Node Debugger
```typescript
export class MyLink implements Link {
  async call(ctx: Context): Promise<Context> {
    debugger;  // Breakpoint
    return ctx.insert('result', 'value');
  }
}
```

## Common Issues

### Module Resolution
- Check `tsconfig.json` paths
- Ensure `package.json` exports are correct
- Use `.js` extensions in imports for ESM

### Type Errors
- Run `npm run type-check` or `tsc --noEmit`
- Check generic type parameters
- Ensure correct type imports

### Jest Configuration
- ESLint knows about Jest globals (describe, test, expect)
- Use `@jest/globals` if needed
- Check `jest.config.js` for proper setup

### Linting Issues
- Run `npm run lint:fix` to auto-fix
- Check `.eslintrc.json` configuration
- For test files, Jest globals are recognized

## Building and Distribution

### Build Project
```bash
npm run build           # Build TypeScript to JavaScript
npm run build:watch     # Watch mode
```

### Publishing
```bash
npm run prepublish      # Run before publishing
npm publish            # Publish to npm
```

## Reference

- **Core Implementation**: `src/core/`
- **Test Examples**: `tests/`
- **Type Definitions**: `src/types/`
- **JavaScript Documentation**: `packages/javascript/README.md`
