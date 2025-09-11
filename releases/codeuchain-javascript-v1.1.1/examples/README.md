# CodeUChain JavaScript Examples

This directory contains comprehensive examples demonstrating various CodeUChain patterns and features in JavaScript and TypeScript.

## 📁 Available Examples

### Core Patterns (Based on ASCII_PIPELINES.txt)

#### 1. **Branch + Merge Pipeline** (`branch_merge_pipeline.js`)
Demonstrates the fan-out/fan-in pattern where data is split into parallel branches and merged back together.

**Pattern:**
```
              +-> (Normalize A) -+
[Input] -> (Fan)                (Merge) -> (Aggregate) -> [Output]
              +-> (Normalize B) -+
```

**Features:**
- Parallel processing of data branches
- Different normalization strategies per branch
- Result aggregation and merging
- Performance optimization through concurrency

#### 2. **Error Classification Side Path** (`error_classification_pipeline.js`)
Shows how to handle errors by routing them through classification and recovery paths.

**Pattern:**
```
(Link) -X-> [Error?]--yes--> (Classify) -> (Retry or Fail)
   | no
   v
 Next Link
```

**Features:**
- Error type classification (temporary, validation, auth, unknown)
- Conditional routing based on error type
- Retry logic for recoverable errors
- Permanent failure handling

#### 3. **Parallel Fan-Out & Join** (`parallel_fanout_join.js`)
Demonstrates splitting work into parallel branches and synchronizing results.

**Pattern:**
```
          +-> (Link A) --+
[Ctx] -> ( Split  )      ( Join ) -> [Ctx']
          +-> (Link B) --+
```

**Features:**
- Work distribution across parallel branches
- Concurrent processing with Promise.all
- Result synchronization and joining
- Performance metrics and load balancing

#### 4. **Middleware Wrap** (`middleware_wrap_pipeline.js`)
Shows how to wrap links with cross-cutting concerns using middleware.

**Pattern:**
```
[Ctx] -> [Before MW] -> (Link) -> [After MW] -> [Ctx']
              | error
              v
          [OnError MW]
```

**Features:**
- Timing middleware for performance monitoring
- Validation middleware for pre/post conditions
- Metrics collection middleware
- Error handling middleware

#### 5. **Saga with Compensations** (`saga_compensations.js`)
Implements distributed transactions with compensation logic for rollback.

**Pattern:**
```
(Do Step 1) -> (Do Step 2) -> (Do Step 3)
     |             |             |
     v             v             v
 (Push C1)    (Push C2)     (Push C3)

On failure -> Pop & run compensations: C3, C2, C1
```

**Features:**
- Saga orchestrator with compensation stack
- LIFO compensation execution
- Failure recovery and cleanup
- Transaction-like behavior for distributed operations

#### 6. **Retry with Backoff** (`retry_with_backoff.js`)
Demonstrates retry logic with exponential backoff for transient failures.

**Pattern:**
```
+---------+  failure   +-----------+
| Attempt | ---------> | Backoff n | --+
+----+----+            +-----------+   |
     ^                                  |
     +-------------- success <----------+
```

**Features:**
- Exponential backoff with jitter
- Configurable retry limits
- Different failure types (temporary vs persistent)
- Metrics collection and analysis

### Type System Examples

#### 7. **Typed Features Demo** (`typed_features_demo.js`)
Comprehensive demonstration of opt-in typed features in JavaScript.

**Features:**
- JSDoc annotations for TypeScript-like experience
- Generic Context<T> with type evolution
- Generic Link<TInput, TOutput> interfaces
- Type-safe insertAs() method
- Backward compatibility with untyped code

#### 8. **Simple Type Evolution** (`simple_type_evolution.ts`)
TypeScript example showing clean type evolution through processing layers.

**Features:**
- TypeScript interface definitions
- Clean type progression (UserInput -> ValidatedUser -> CompleteUser)
- Type-safe data transformation
- Simple processing chain demonstration

### Basic Examples

#### 9. **Simple Chain** (`simple_chain.js`)
Basic CodeUChain usage with user registration flow.

**Features:**
- Basic Link and Chain usage
- Manual and automatic link naming
- Middleware integration
- Error handling

## 🚀 Running the Examples

Each example can be run independently:

```bash
# Run a specific example
node examples/branch_merge_pipeline.js
node examples/error_classification_pipeline.js
node examples/parallel_fanout_join.js
node examples/middleware_wrap_pipeline.js
node examples/saga_compensations.js
node examples/retry_with_backoff.js
node examples/typed_features_demo.js

# For TypeScript examples
npx ts-node examples/simple_type_evolution.ts
```

## 📚 Key Concepts Demonstrated

### Pipeline Patterns
- **Linear Processing**: Sequential link execution
- **Branching**: Conditional and parallel processing paths
- **Error Handling**: Classification, retry, and recovery patterns
- **Middleware**: Cross-cutting concerns and aspect-oriented programming

### Type System Features
- **Opt-in Typing**: Optional type safety without breaking changes
- **Type Evolution**: Clean transformation between data shapes
- **Generic Interfaces**: Type-safe Link<TInput, TOutput> patterns
- **Backward Compatibility**: Mixed typed/untyped usage

### Advanced Patterns
- **Saga Transactions**: Distributed operations with compensation
- **Retry Logic**: Exponential backoff and failure recovery
- **Parallel Processing**: Work distribution and synchronization
- **Metrics Collection**: Performance monitoring and analysis

## 🎯 Learning Path

1. **Start Here**: `simple_chain.js` - Basic concepts
2. **Type System**: `typed_features_demo.js` + `simple_type_evolution.ts`
3. **Pipeline Patterns**: Branch/merge, error handling, middleware
4. **Advanced Topics**: Saga, retry, parallel processing

## 🔧 Requirements

- Node.js 14+
- For TypeScript examples: `npm install -g ts-node typescript`

## 📖 Related Documentation

- [ASCII Pipeline Diagrams](../../docs/diagrams/ASCII_PIPELINES.txt)
- [Typed Features Specification](../../docs/TYPED_FEATURES_SPECIFICATION.md)
- [Core API Documentation](../core/)

---

*These examples showcase CodeUChain's flexibility and power across different processing patterns while maintaining clean, maintainable code.*