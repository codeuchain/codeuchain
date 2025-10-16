/// CodeUChain Dart - A universal framework for composable software
/// 
/// CodeUChain provides a robust framework for chaining processing links 
/// with middleware support and comprehensive error handling.
/// Enhanced with generic typing for type-safe workflows.
/// 
/// ## Core Concepts
/// 
/// - **Context**: Immutable data container that flows through the chain
/// - **Link**: Individual processing unit with single responsibility
/// - **Chain**: Ordered sequence of Links with orchestration
/// - **Middleware**: Cross-cutting concerns like logging and error handling
/// 
/// ## Features
/// 
/// - 🎯 **Generic Types**: `Link<TInput, TOutput>` and `Context<T>` for compile-time safety
/// - 🔄 **Type Evolution**: Transform between related types without casting
/// - ⚡ **Zero Performance Impact**: Identical runtime behavior with or without typing
/// - 📈 **Gradual Adoption**: Add typing incrementally to existing code
/// - 🛡️ **Comprehensive Error Handling**: Built-in error routing and retry logic
/// - 🔍 **Rich Middleware**: Logging, performance monitoring, and custom observers
/// 
/// ## Quick Start
/// 
/// ```dart
/// import 'package:codeuchain/codeuchain.dart';
/// 
/// // Define a simple link
/// class AddNumbersLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
///   AddNumbersLink() : super('AddNumbers');
/// 
///   @override
///   Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
///     final a = context.get('a') as int;
///     final b = context.get('b') as int;
///     return context.insert('result', a + b);
///   }
/// }
/// 
/// // Create and run a chain
/// final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
///   .add(AddNumbersLink())
///   .use(LoggingMiddleware());
/// 
/// final result = await chain.run(Context({'a': 10, 'b', 20}));
/// print(result.get('result')); // 30
/// ```
library;

// Core exports
export 'src/context.dart';
export 'src/link.dart';
export 'src/chain.dart';
export 'src/middleware.dart';
