# CodeUChain Dart

CodeUChain provides a robust framework for chaining processing links with hook support and comprehensive error handling. Enhanced with generic typing for type-safe workflows following Dart's language idioms.

[![Dart](https://img.shields.io/badge/Dart-3.9+-blue)](https://dart.dev/)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

**Status**: 🧪 **Beta** - Feature complete with comprehensive test coverage. May shift slightly pending usage and optimization.

> **Beta Notice**: This implementation is feature-complete and thoroughly tested, but is marked as beta to allow for potential API refinements based on real-world usage and performance optimization feedback. The core concepts and patterns are stable and follow CodeUChain's universal design principles.

## 🌐 Part of CodeUChain Universal Framework

Visit us at **[codeuchain.com](https://codeuchain.com)** for the complete cross-language framework.

## ✨ Features

- **🎯 Generic Types**: `Link<TInput, TOutput>` and `State<T>` for compile-time safety
- **🔄 Type Evolution**: Transform between related types without casting via `insertAs<U>()`
- **⚡ Zero Performance Impact**: Identical runtime behavior with or without typing
- **📈 Gradual Adoption**: Add typing incrementally to existing code
- **🛡️ Comprehensive Error Handling**: Built-in error routing and retry logic
- **🔍 Rich Hook**: Logging, performance monitoring, and custom observers
- **🚀 Dart Idioms**: Leverages null safety, async/await, and strong typing
- **🔗 Universal Patterns**: Same mental model as other CodeUChain implementations

## 📦 Installation

Add this to your package's `pubspec.yaml` file:

```yaml
dependencies:
  codeuchain: ^1.0.0-beta.1
```

Then run:

```bash
dart pub get
```

## 🚀 Quick Start

```dart
import 'package:codeuchain/codeuchain.dart';

// Define a simple link
class AddNumbersLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  AddNumbersLink() : super('AddNumbers');

  @override
  Future<State<Map<String, dynamic>>> execute(State<Map<String, dynamic>> state) async {
    final a = state.get('a') as int;
    final b = state.get('b') as int;
    return state.insert('result', a + b);
  }
}

void main() async {
  // Create and run a chain
  final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
    .add(AddNumbersLink())
    .use(LoggingHook());

  final result = await chain.run(State({'a': 10, 'b': 20}));
  print(result.get('result')); // 30
}
```

## 🎯 Core Concepts

### State: The Data Container

```dart
// Immutable by default
final state = State({'name': 'Alice', 'age': 30});
final updated = state.insert('email', 'alice@example.com');

// Type evolution for clean transformations
final evolved = state.insertAs<UserProfile>('profile', UserProfile(...));

// Mutable for performance-critical operations
final mutable = state.toMutable();
mutable.set('temp', 'value');
final backToImmutable = mutable.toImmutable();
```

### Link: The Processing Unit

```dart
// Class-based links
class ValidateUserLink extends BaseLink<UserInput, UserInput> {
  ValidateUserLink() : super('ValidateUser');

  @override
  Future<State<UserInput>> execute(State<UserInput> state) async {
    // Validation logic here
    return state.insert('validated', true);
  }
}

// Function-based links for quick prototyping
final quickLink = FunctionLink('ProcessData', (state) async {
  final data = state.get('data');
  return state.insert('processed', processData(data));
});

// Synchronous links
final syncLink = Chain()
  .addSyncFunction('Transform', (state) {
    return state.insert('transformed', true);
  });
```

### Chain: The Orchestrator

```dart
final userChain = Chain<UserInput, UserOutput>('UserProcessing')
  .add(ValidateUserLink())
  .add(ProcessUserLink())
  .add(SaveUserLink())
  .use(LoggingHook())
  .use(PerformanceHook());

final result = await userChain.run(initialState);
```

### Hook: The Enhancer

```dart
// Built-in logging
.use(LoggingHook(logLevel: LogLevel.debug))

// Performance monitoring
.use(PerformanceHook(
  slowExecutionThreshold: 1000,
  onSlowExecution: (name, duration) => print('Slow: $name'),
))

// Custom hook
.use(FunctionHook(
  'CustomMonitor',
  beforeLink: (execution) async => print('Starting ${execution.linkName}'),
  afterLink: (execution) async => print('Completed ${execution.linkName}'),
))
```

## 🎨 Typed Features

### Generic Link Interfaces

```dart
// Strongly typed link interface
class UserProcessor extends BaseLink<UserInput, ProcessedUser> {
  UserProcessor() : super('UserProcessor');

  @override
  Future<State<ProcessedUser>> execute(State<UserInput> state) async {
    final input = state.get('userData') as UserInput;
    final processed = ProcessedUser.fromInput(input);
    return state.insertAs<ProcessedUser>('processedUser', processed);
  }
}

// Type-safe chain composition
final typedChain = Chain<UserInput, ProcessedUser>()
  .add(ValidateUserLink())      // UserInput → UserInput  
  .add(UserProcessor())         // UserInput → ProcessedUser
  .add(SaveProcessedLink());    // ProcessedUser → ProcessedUser
```

### Type Evolution

```dart
// Clean type transformations without casting
final inputState = State<UserInput>({'userData': userData});
final processedState = inputState.insertAs<ProcessedUser>('result', processedUser);

// Maintains both old and new data
print(processedState.get('userData'));  // Original UserInput
print(processedState.get('result'));    // New ProcessedUser
```

For more examples, see the [example directory](example/) which includes comprehensive demonstrations of all features.

## 📚 Additional Information

- **Documentation**: Complete API documentation and examples in this repository
- **Issues**: Please file issues at [GitHub Issues](https://github.com/codeuchain/codeuchain/issues)
- **Contributing**: See [Contributing Guidelines](https://github.com/codeuchain/codeuchain/blob/main/CONTRIBUTING.md)
- **License**: Apache 2.0 - see [LICENSE](LICENSE) file

## 🌟 Related Implementations

- **[CodeUChain Python](https://github.com/codeuchain/codeuchain/tree/main/packages/python)** - Reference implementation
- **[CodeUChain Go](https://github.com/codeuchain/codeuchain/tree/main/packages/go)** - High-performance implementation  
- **[CodeUChain JavaScript](https://github.com/codeuchain/codeuchain/tree/main/packages/javascript)** - Web and Node.js implementation
- **[CodeUChain C#](https://github.com/codeuchain/codeuchain/tree/main/packages/csharp)** - Enterprise .NET implementation
- **[CodeUChain Rust](https://github.com/codeuchain/codeuchain/tree/main/packages/rust)** - Memory-safe implementation

---

*CodeUChain Dart: Where strong typing meets elegant composition 🎯*
