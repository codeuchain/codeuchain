/// Link: The Processing Unit
///
/// Individual processing unit with single responsibility.
/// Accepts Context input → Returns modified Context output.
/// Enhanced with generic typing for type-safe workflows.
library;

import 'dart:async';
import 'context.dart';

/// Generic interface for processing links with typed input/output
/// 
/// A Link processes data by taking a Context of input type and returning
/// a Context of output type. This enables type-safe composition and transformation.
abstract class Link<TInput, TOutput> {
  /// The name of this link for debugging and logging
  String get name;

  /// Executes the link's processing logic
  /// 
  /// [context] - The input context containing data to process
  /// Returns a Future containing the transformed context
  /// 
  /// This method should be implemented by concrete link classes to define
  /// their specific processing behavior.
  Future<Context<TOutput>> call(Context<TInput> context);
}

/// Base implementation of Link for common patterns
/// 
/// Provides a foundation for implementing concrete links with
/// consistent error handling and logging patterns.
abstract class BaseLink<TInput, TOutput> implements Link<TInput, TOutput> {
  /// The name of this link for debugging and logging
  @override
  final String name;

  /// Creates a new base link with the given name
  BaseLink(this.name);

  @override
  Future<Context<TOutput>> call(Context<TInput> context) async {
    try {
      return await execute(context);
    } catch (error, stackTrace) {
      throw LinkExecutionException(
        'Link "$name" failed during execution',
        error,
        stackTrace,
      );
    }
  }

  /// Implement this method to define the link's processing logic
  /// 
  /// [context] - The input context containing data to process
  /// Returns a Future containing the transformed context
  Future<Context<TOutput>> execute(Context<TInput> context);

  @override
  String toString() => 'Link<$TInput, $TOutput>($name)';
}

/// Simple function-based link implementation
/// 
/// Allows creating links from simple functions without defining classes.
/// Useful for quick prototyping and simple transformations.
class FunctionLink<TInput, TOutput> implements Link<TInput, TOutput> {
  @override
  final String name;
  final Future<Context<TOutput>> Function(Context<TInput>) _function;

  /// Creates a new function-based link
  /// 
  /// [name] - The name of this link for debugging
  /// [function] - The function that performs the transformation
  FunctionLink(this.name, this._function);

  @override
  Future<Context<TOutput>> call(Context<TInput> context) async {
    try {
      return await _function(context);
    } catch (error, stackTrace) {
      throw LinkExecutionException(
        'FunctionLink "$name" failed during execution',
        error,
        stackTrace,
      );
    }
  }

  @override
  String toString() => 'FunctionLink<$TInput, $TOutput>($name)';
}

/// Synchronous link interface for non-async operations
/// 
/// For links that don't need async operations, this provides
/// a simpler interface while maintaining type safety.
abstract class SyncLink<TInput, TOutput> {
  /// The name of this link for debugging and logging
  String get name;

  /// Executes the link's processing logic synchronously
  /// 
  /// [context] - The input context containing data to process
  /// Returns the transformed context
  Context<TOutput> callSync(Context<TInput> context);

  /// Converts this sync link to an async link
  Link<TInput, TOutput> toAsync() => _SyncLinkAdapter(this);
}

/// Adapter to convert sync links to async links
class _SyncLinkAdapter<TInput, TOutput> implements Link<TInput, TOutput> {
  final SyncLink<TInput, TOutput> _syncLink;

  _SyncLinkAdapter(this._syncLink);

  @override
  String get name => _syncLink.name;

  @override
  Future<Context<TOutput>> call(Context<TInput> context) async {
    return _syncLink.callSync(context);
  }

  @override
  String toString() => 'SyncLinkAdapter(${_syncLink.toString()})';
}

/// Exception thrown when link execution fails
class LinkExecutionException implements Exception {
  /// The error message
  final String message;
  
  /// The original error that caused this exception
  final Object? originalError;
  
  /// The stack trace where the error occurred
  final StackTrace? stackTrace;

  /// Creates a new link execution exception
  LinkExecutionException(this.message, [this.originalError, this.stackTrace]);

  @override
  String toString() {
    if (originalError != null) {
      return 'LinkExecutionException: $message\nCaused by: $originalError';
    }
    return 'LinkExecutionException: $message';
  }
}