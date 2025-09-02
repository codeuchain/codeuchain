# @codeuchain/javascript

**Interactive Playground**: Event-driven, ubiquitous JavaScript patterns with agape love.

CodeUChain for JavaScript brings the harmony of chained processing to the world's most ubiquitous runtime. With Node.js ubiquity and browser compatibility, JavaScript implementations shine in event-driven architectures, real-time processing, and web-first applications.

## 🌟 JavaScript's Heart: Event-Driven Love

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

## 🌈 Complete JavaScript Example

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

## 🌟 JavaScript's Agape Advantages

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

## 💭 JavaScript Philosophy in CodeUChain

**JavaScript brings the ubiquity and flexibility of a universal translator to CodeUChain.** It runs everywhere, adapts to any environment, and connects diverse systems with seamless integration.

Like a loving bridge between worlds, JavaScript makes CodeUChain accessible to every developer and deployable to every platform, fostering universal understanding and connection.

*"In the ecosystem of programming languages, JavaScript is the loving universal translator that makes CodeUChain speak every language and run on every platform."*

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

- **Context**: Immutable data container with loving care
- **MutableContext**: Mutable sibling for performance-critical sections
- **Link**: Base class for context processors
- **Chain**: Orchestrator for link execution
- **Middleware**: Enhancement hooks with gentle defaults

## 🤝 Contributing

With agape love, we welcome contributions that enhance JavaScript's role in the universal CodeUChain ecosystem.