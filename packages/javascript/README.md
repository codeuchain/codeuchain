# @codeuchain/javascript

**Interactive Playground**: Event-driven, ubiquitous JavaScript patterns for modern development.

CodeUChain for JavaScript brings the power of chained processing to the world's most ubiquitous runtime. With Node.js ubiquity and browser compatibility, JavaScript implementations excel in event-driven architectures, real-time processing, and web-first applications.

## 📦 Installation

```bash
npm install codeuchain
```

## 🤖 LLM *"In the ecosystem of programming languages, JavaScript is the universal translator that makes CodeUChain work across every platform and environment."*

## 🚀 Quick StartThis package supports the [llm.txt standard](https://codeuchain.github.io/codeuchain/javascript/llm.txt) for easy AI/LLM integration. See [llm-full.txt](https://codeuchain.github.io/codeuchain/javascript/llm-full.txt) for comprehensive documentation.

## 🌟 JavaScript's Strength: Event-Driven Architecture

JavaScript brings **universal reach** to CodeUChain:
- **Ubiquitous runtime**: Browser, server, mobile, IoT
- **Event-driven architecture**: Perfect for async chains
- **Dynamic flexibility**: Runtime adaptation and introspection
- **Ecosystem richness**: NPM's vast library ecosystem

## 💝 Simple JavaScript Chain

### The Loving Context
```javascript
const { Context, MutableContext } = require('@codeuchain/javascript');

// Immutable context with selfless love
const ctx = new Context({
  user: 'alice',
  email: 'alice@example.com'
});

// Get data with gentle care
const user = ctx.get('user'); // 'alice'

// Add data with selfless safety
const newCtx = ctx.insert('verified', true);

// Mutable context for performance-critical sections
const mutable = ctx.withMutation();
mutable.set('temp', 'value');
const finalCtx = mutable.toImmutable();
```

### The Selfless Link
```javascript
const { Link } = require('@codeuchain/javascript');

class EmailValidationLink extends Link {
  async call(ctx) {
    const email = ctx.get('email');

    if (!email || !email.includes('@')) {
      throw new Error('Invalid email format');
    }

    // Return transformed context
    return ctx.insert('emailValid', true);
  }
}

class UserCreationLink extends Link {
  async call(ctx) {
    const user = ctx.get('user');
    const email = ctx.get('email');

    // Simulate user creation
    const userId = `user_${Date.now()}`;

    return ctx
      .insert('userId', userId)
      .insert('created', new Date().toISOString());
  }
}
```

### The Harmonious Chain
```javascript
const { Chain } = require('@codeuchain/javascript');

async function createUserRegistrationChain() {
  const chain = new Chain();

  // Add links
  chain.addLink('validate', new EmailValidationLink());
  chain.addLink('create', new UserCreationLink());

  // Connect with conditions
  chain.connect('validate', 'create', (ctx) => ctx.get('emailValid'));

  return chain;
}

// Usage
const registrationChain = await createUserRegistrationChain();

const initialCtx = new Context({
  user: 'alice',
  email: 'alice@example.com'
});

const resultCtx = await registrationChain.run(initialCtx);
console.log('User ID:', resultCtx.get('userId'));
```

### The Gentle Middleware
```javascript
const { LoggingMiddleware, TimingMiddleware } = require('@codeuchain/javascript');

const chain = new Chain();

// Add middleware
chain.useMiddleware(new LoggingMiddleware());
chain.useMiddleware(new TimingMiddleware());

// Add error handling
chain.onError((error, ctx, linkName) => {
  console.error(`Chain error in ${linkName}:`, error.message);
  // Handle error gracefully
});
```

## � Opt-in Typed Features

**JavaScript CodeUChain now supports opt-in generic typing** for enhanced developer experience and type safety. These features are completely optional and maintain 100% backward compatibility.

### Generic Context with Type Evolution

```javascript
const { Context } = require('@codeuchain/javascript');

/**
 * @typedef {Object} UserInput
 * @property {string} name - User's name
 * @property {string} email - User's email
 */

/**
 * @typedef {UserInput & Object} UserValidated
 * @property {string} name - User's name
 * @property {string} email - User's email
 * @property {boolean} isValid - Validation status
 */

// Create typed context
/** @type {UserInput} */
const userData = { name: 'Alice', email: 'alice@example.com' };
const ctx = new Context(userData);

// Type evolution with insertAs() - clean transformation
/** @type {Context<UserValidated>} */
const validatedCtx = ctx.insertAs('isValid', true);

// Original data preserved, new field added
console.log(validatedCtx.get('name')); // 'Alice'
console.log(validatedCtx.get('isValid')); // true
```

### Generic Link Interfaces

```javascript
const { Link } = require('@codeuchain/javascript');

/**
 * Link for validating user input
 * @extends {Link<UserInput, UserValidated>}
 */
class ValidationLink extends Link {
  /**
   * @param {Context<UserInput>} ctx
   * @returns {Promise<Context<UserValidated>>}
   */
  async call(ctx) {
    const email = ctx.get('email');

    if (!email.includes('@')) {
      throw new Error('Invalid email');
    }

    // Type evolution: UserInput -> UserValidated
    return ctx.insertAs('isValid', true);
  }
}

/**
 * Link for processing validated users
 * @extends {Link<UserValidated, UserProcessed>}
 */
class ProcessingLink extends Link {
  /**
   * @param {Context<UserValidated>} ctx
   * @returns {Promise<Context<UserProcessed>>}
   */
  async call(ctx) {
    const isValid = ctx.get('isValid');
    if (!isValid) throw new Error('User not validated');

    return ctx
      .insertAs('userId', `user_${Date.now()}`)
      .insertAs('status', 'active');
  }
}
```

### Generic Chain Processing

```javascript
const { Chain } = require('@codeuchain/javascript');

/**
 * Typed user registration chain
 * @extends {Chain<UserInput, UserProcessed>}
 */
class UserRegistrationChain extends Chain {
  constructor() {
    super();

    // Add typed links
    this.addLink(new ValidationLink());
    this.addLink(new ProcessingLink());

    // Connect with type safety
    this.connect('ValidationLink', 'ProcessingLink');
  }

  /**
   * Register user with full type safety
   * @param {Context<UserInput>} initialCtx
   * @returns {Promise<Context<UserProcessed>>}
   */
  async registerUser(initialCtx) {
    return await this.run(initialCtx);
  }
}

// Usage with type safety
const chain = new UserRegistrationChain();
const inputCtx = new Context({ name: 'Alice', email: 'alice@example.com' });
const resultCtx = await chain.registerUser(inputCtx);

console.log(resultCtx.get('userId')); // TypeScript knows this exists
console.log(resultCtx.get('status')); // TypeScript knows this exists
```

### TypeScript Definitions

For full TypeScript support, use the included type definitions:

```typescript
import { Context, Link, Chain } from '@codeuchain/javascript';

// Full TypeScript generic support
interface UserInput {
  name: string;
  email: string;
}

interface UserProcessed extends UserInput {
  isValid: boolean;
  userId: string;
  status: string;
}

// Type-safe operations
const ctx: Context<UserInput> = new Context({ name: 'Alice', email: 'alice@example.com' });
const result: Context<UserProcessed> = ctx.insertAs('isValid', true)
  .insertAs('userId', 'user_123')
  .insertAs('status', 'active');

// TypeScript provides full IntelliSense and type checking
```

### ES5 TypeScript Compatibility

CodeUChain fully supports TypeScript projects targeting ES5, making it compatible with legacy environments and older browsers.

**TypeScript Configuration (tsconfig.json):**
```json
{
  "compilerOptions": {
    "target": "ES5",
    "module": "commonjs",
    "lib": ["ES2015"]
  }
}
```

**TypeScript Source Code:**
```typescript
import { Link, Chain, Context } from 'codeuchain';

class ValidateLink extends Link {
  async call(ctx: Context): Promise<Context> {
    const value = ctx.get('value');
    return ctx.insertAs('isValid', value > 0);
  }
}

// TypeScript compiles this to ES5-compatible code that works seamlessly
const link = new ValidateLink();
```

**Key ES5 Features:**
- ✅ Full inheritance support (extends Link, Chain, Middleware)
- ✅ ES5 constructor function output compatibility
- ✅ Works with TypeScript targets: ES5, ES3, ES2015+
- ✅ No runtime errors with ES5 compilation
- ✅ Complete backward compatibility

See `examples/es5_typescript_inheritance.js` for a complete working example.

### Key Benefits of Typed Features

- **Enhanced IDE Support**: Full IntelliSense, autocomplete, and refactoring
- **Type Safety**: Catch errors at development time
- **Clean Type Evolution**: `insertAs()` method for seamless transformations
- **Zero Runtime Cost**: Typing is compile-time only, no performance impact
- **100% Backward Compatible**: Existing code continues to work unchanged
- **Mixed Usage**: Typed and untyped code can coexist seamlessly

### When to Use Typed Features

**Use typed features when:**
- Building complex processing pipelines
- Working in teams with multiple developers
- Needing enhanced IDE support and refactoring
- Wanting to catch type-related errors early

**Continue using untyped features when:**
- Rapid prototyping and exploration
- Simple, straightforward processing
- Maximum runtime flexibility needed
- Working with highly dynamic data structures

## �🌈 Complete JavaScript Example

### Real-Time Event Processing Chain
```javascript
const { Context, Chain, Link, LoggingMiddleware } = require('@codeuchain/javascript');

class EventValidationLink extends Link {
  async call(ctx) {
    const event = ctx.get('event');

    if (!event || !event.type) {
      throw new Error('Invalid event: missing type');
    }

    return ctx.insert('validated', true);
  }
}

class EventProcessingLink extends Link {
  async call(ctx) {
    const event = ctx.get('event');

    // Process based on event type
    switch (event.type) {
      case 'user_login':
        return ctx.insert('action', 'authenticate');
      case 'data_update':
        return ctx.insert('action', 'sync');
      default:
        return ctx.insert('action', 'unknown');
    }
  }
}

class EventLoggingLink extends Link {
  async call(ctx) {
    const event = ctx.get('event');
    const action = ctx.get('action');

    console.log(`Processing ${event.type} -> ${action}`);

    return ctx.insert('logged', true);
  }
}

// Create real-time processing chain
const eventChain = new Chain();
eventChain.addLink('validate', new EventValidationLink());
eventChain.addLink('process', new EventProcessingLink());
eventChain.addLink('log', new EventLoggingLink());

eventChain.connect('validate', 'process');
eventChain.connect('process', 'log');

eventChain.useMiddleware(new LoggingMiddleware());

// Process events in real-time
async function processEvent(event) {
  const ctx = new Context({ event });
  return await eventChain.run(ctx);
}

// Usage
const loginEvent = { type: 'user_login', userId: 123 };
const result = await processEvent(loginEvent);
console.log('Processing result:', result.toObject());
```

## 💡 JavaScript-Specific Optimizations

### Promise-Based Async Chains
```javascript
// Leverage JavaScript's promise ecosystem
const asyncChain = Chain.createLinear(
  { name: 'fetch', link: new DataFetchLink() },
  { name: 'process', link: new DataProcessLink() },
  { name: 'store', link: new DataStoreLink() }
);

// Run with promise composition
asyncChain.run(initialCtx)
  .then(result => console.log('Success:', result.toObject()))
  .catch(error => console.error('Chain failed:', error));
```

### Event-Driven Middleware
```javascript
class EventEmitterMiddleware extends Middleware {
  constructor(emitter) {
    super();
    this.emitter = emitter;
  }

  async before(link, ctx, linkName) {
    this.emitter.emit('link:before', { linkName, ctx: ctx.toObject() });
  }

  async after(link, ctx, linkName) {
    this.emitter.emit('link:after', { linkName, ctx: ctx.toObject() });
  }

  async onError(link, error, ctx, linkName) {
    this.emitter.emit('link:error', { linkName, error: error.message });
  }
}
```

### Dynamic Link Creation
```javascript
// Create links dynamically based on configuration
function createLinksFromConfig(config) {
  return config.map(item => ({
    name: item.name,
    link: new DynamicLink(item.handler)
  }));
}

class DynamicLink extends Link {
  constructor(handler) {
    super();
    this.handler = handler;
  }

  async call(ctx) {
    return await this.handler(ctx);
  }
}
```

## 🌟 JavaScript's Key Advantages

### For Real-Time Applications
- **Event-driven**: Perfect for WebSocket, streaming, real-time updates
- **Async/await**: Clean asynchronous chain execution
- **Browser compatibility**: Same code runs everywhere
- **Hot reloading**: Development with instant feedback

### For Microservices
- **Lightweight**: Minimal runtime footprint
- **NPM ecosystem**: Rich integration options
- **Serverless ready**: Perfect for AWS Lambda, Vercel, Netlify
- **JSON native**: Seamless data serialization

### For Prototyping
- **Rapid development**: Quick iteration cycles
- **Dynamic typing**: Flexible during exploration
- **Rich tooling**: DevTools, debugging, profiling
- **Community**: Vast knowledge base and examples

## 💭 JavaScript's Role in CodeUChain

**JavaScript brings ubiquity and flexibility as a universal translator to CodeUChain.** It runs everywhere, adapts to any environment, and connects diverse systems with seamless integration.

As a bridge between worlds, JavaScript makes CodeUChain accessible to every developer and deployable to every platform, fostering universal understanding and connection.

*"In the ecosystem of programming languages, JavaScript is the universal translator that makes CodeUChain work across every platform and environment."*

## 📦 Installation

```bash
npm install @codeuchain/javascript
```

## 🚀 Quick Start

```javascript
const { Context, Chain, Link } = require('@codeuchain/javascript');

class HelloLink extends Link {
  async call(ctx) {
    const name = ctx.get('name') || 'World';
    return ctx.insert('message', `Hello, ${name}!`);
  }
}

const chain = new Chain();
chain.addLink('hello', new HelloLink());

const result = await chain.run(new Context({ name: 'CodeUChain' }));
console.log(result.get('message')); // "Hello, CodeUChain!"
```

## 📚 API Reference

- **Context**: Immutable data container with careful handling
- **MutableContext**: Mutable sibling for performance-critical sections
- **Link**: Base class for context processors
- **Chain**: Orchestrator for link execution
- **Middleware**: Enhancement hooks with sensible defaults

## 🤝 Contributing

We welcome contributions that enhance JavaScript's role in the universal CodeUChain ecosystem.