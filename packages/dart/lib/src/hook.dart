/// Middleware: The Enhancer
///
/// Observes and enhances Chain execution.
/// Operates outside main processing flow for cross-cutting concerns.
library;

import 'dart:async';
import 'chain.dart';

/// Abstract middleware interface for observing and enhancing chain execution
/// 
/// Middleware provides hooks into the chain execution lifecycle to implement
/// cross-cutting concerns like logging, metrics, error handling, authentication,
/// and caching. Follows the ABC pattern where all methods have no-op defaults.
abstract class Middleware {
  /// Called before the entire chain starts executing
  /// 
  /// [execution] - Information about the chain execution that's about to start
  Future<void> beforeExecution(ChainExecution execution) async {}

  /// Called after the entire chain completes successfully
  /// 
  /// [execution] - Information about the completed chain execution
  Future<void> afterExecution(ChainExecution execution) async {}

  /// Called before each individual link executes
  /// 
  /// [execution] - Information about the link that's about to execute
  Future<void> beforeLink(LinkExecution execution) async {}

  /// Called after each individual link executes successfully
  /// 
  /// [execution] - Information about the completed link execution
  Future<void> afterLink(LinkExecution execution) async {}

  /// Called when an error occurs in a link or chain
  /// 
  /// [execution] - Information about the failed link execution
  Future<void> onError(LinkExecution execution) async {}
}

/// Base middleware implementation with helpful utilities
/// 
/// Provides common functionality for implementing middleware,
/// including name management and helper methods.
abstract class BaseMiddleware implements Middleware {
  /// The name of this middleware for debugging and logging
  final String name;

  /// Creates a new base middleware with the given name
  BaseMiddleware(this.name);

  @override
  String toString() => 'Middleware($name)';
}

/// Logging middleware for debugging and monitoring
/// 
/// Logs chain and link execution events with configurable detail levels.
/// Useful for debugging chain execution and monitoring performance.
class LoggingMiddleware extends BaseMiddleware {
  /// The level of detail to log
  final LogLevel logLevel;
  
  /// Function to use for logging (defaults to print)
  final void Function(String) logFunction;

  /// Creates a new logging middleware
  /// 
  /// [logLevel] - The level of detail to log
  /// [logFunction] - Optional custom logging function
  LoggingMiddleware({
    this.logLevel = LogLevel.info,
    this.logFunction = print,
  }) : super('LoggingMiddleware');

  @override
  Future<void> beforeExecution(ChainExecution execution) async {
    if (logLevel.index <= LogLevel.info.index) {
      logFunction('🚀 Starting chain: ${execution.chainName}');
    }
    if (logLevel.index <= LogLevel.debug.index) {
      logFunction('📊 Initial context: ${execution.initialContext.toMap()}');
    }
  }

  @override
  Future<void> afterExecution(ChainExecution execution) async {
    if (logLevel.index <= LogLevel.info.index) {
      logFunction('✅ Completed chain: ${execution.chainName} in ${execution.duration.inMilliseconds}ms');
    }
    if (logLevel.index <= LogLevel.debug.index && execution.result != null) {
      logFunction('📊 Final context: ${execution.result!.toMap()}');
    }
  }

  @override
  Future<void> beforeLink(LinkExecution execution) async {
    if (logLevel.index <= LogLevel.debug.index) {
      logFunction('🔗 Executing link ${execution.linkIndex + 1}/${execution.totalLinks}: ${execution.linkName}');
    }
  }

  @override
  Future<void> afterLink(LinkExecution execution) async {
    if (logLevel.index <= LogLevel.debug.index) {
      logFunction('✅ Completed link: ${execution.linkName} in ${execution.duration.inMilliseconds}ms');
    }
  }

  @override
  Future<void> onError(LinkExecution execution) async {
    if (logLevel.index <= LogLevel.error.index) {
      logFunction('❌ Error in ${execution.linkName}: ${execution.error}');
    }
    if (logLevel.index <= LogLevel.debug.index && execution.stackTrace != null) {
      logFunction('📍 Stack trace: ${execution.stackTrace}');
    }
  }
}

/// Performance monitoring middleware
/// 
/// Tracks execution times and performance metrics for chains and links.
/// Useful for identifying bottlenecks and monitoring system performance.
class PerformanceMiddleware extends BaseMiddleware {
  final Map<String, List<Duration>> _chainMetrics = {};
  final Map<String, List<Duration>> _linkMetrics = {};
  
  /// Threshold for slow execution warnings (in milliseconds)
  final int slowExecutionThreshold;
  
  /// Function to call when slow execution is detected
  final void Function(String, Duration)? onSlowExecution;

  /// Creates a new performance monitoring middleware
  /// 
  /// [slowExecutionThreshold] - Threshold in milliseconds for slow execution warnings
  /// [onSlowExecution] - Optional callback for slow execution events
  PerformanceMiddleware({
    this.slowExecutionThreshold = 1000,
    this.onSlowExecution,
  }) : super('PerformanceMiddleware');

  @override
  Future<void> beforeExecution(ChainExecution execution) async {
    // No action needed before execution for performance monitoring
  }

  @override
  Future<void> beforeLink(LinkExecution execution) async {
    // No action needed before link execution for performance monitoring
  }

  @override
  Future<void> onError(LinkExecution execution) async {
    // Record error execution time if needed
    final duration = execution.duration;
    final linkName = execution.linkName;
    _linkMetrics.putIfAbsent(linkName, () => []).add(duration);
  }

  @override
  Future<void> afterExecution(ChainExecution execution) async {
    final duration = execution.duration;
    final chainName = execution.chainName;
    
    _chainMetrics.putIfAbsent(chainName, () => []).add(duration);
    
    if (duration.inMilliseconds > slowExecutionThreshold) {
      onSlowExecution?.call('Chain $chainName', duration);
    }
  }

  @override
  Future<void> afterLink(LinkExecution execution) async {
    final duration = execution.duration;
    final linkName = execution.linkName;
    
    _linkMetrics.putIfAbsent(linkName, () => []).add(duration);
    
    if (duration.inMilliseconds > slowExecutionThreshold) {
      onSlowExecution?.call('Link $linkName', duration);
    }
  }

  /// Gets average execution time for a chain
  Duration? getAverageChainTime(String chainName) {
    final metrics = _chainMetrics[chainName];
    if (metrics == null || metrics.isEmpty) return null;
    
    final totalMs = metrics.fold<int>(0, (sum, duration) => sum + duration.inMilliseconds);
    return Duration(milliseconds: totalMs ~/ metrics.length);
  }

  /// Gets average execution time for a link
  Duration? getAverageLinkTime(String linkName) {
    final metrics = _linkMetrics[linkName];
    if (metrics == null || metrics.isEmpty) return null;
    
    final totalMs = metrics.fold<int>(0, (sum, duration) => sum + duration.inMilliseconds);
    return Duration(milliseconds: totalMs ~/ metrics.length);
  }

  /// Gets all performance metrics
  Map<String, dynamic> getMetrics() {
    return {
      'chains': Map.fromEntries(_chainMetrics.entries.map((entry) => 
        MapEntry(entry.key, {
          'executions': entry.value.length,
          'averageMs': getAverageChainTime(entry.key)?.inMilliseconds,
          'totalMs': entry.value.fold<int>(0, (sum, d) => sum + d.inMilliseconds),
        })
      )),
      'links': Map.fromEntries(_linkMetrics.entries.map((entry) => 
        MapEntry(entry.key, {
          'executions': entry.value.length,
          'averageMs': getAverageLinkTime(entry.key)?.inMilliseconds,
          'totalMs': entry.value.fold<int>(0, (sum, d) => sum + d.inMilliseconds),
        })
      )),
    };
  }

  /// Resets all metrics
  void reset() {
    _chainMetrics.clear();
    _linkMetrics.clear();
  }
}

/// Error handling middleware
/// 
/// Provides centralized error handling and recovery for chains.
/// Can implement retry logic, error routing, and failure recovery.
class ErrorHandlingMiddleware extends BaseMiddleware {
  /// Maximum number of retry attempts
  final int maxRetries;
  
  /// Delay between retry attempts
  final Duration retryDelay;
  
  /// Function to determine if an error should trigger a retry
  final bool Function(Object error)? shouldRetry;
  
  /// Function to handle unrecoverable errors
  final Future<void> Function(LinkExecution execution)? onUnrecoverableError;

  /// Creates a new error handling middleware
  /// 
  /// [maxRetries] - Maximum number of retry attempts
  /// [retryDelay] - Delay between retry attempts
  /// [shouldRetry] - Function to determine if an error should trigger a retry
  /// [onUnrecoverableError] - Function to handle unrecoverable errors
  ErrorHandlingMiddleware({
    this.maxRetries = 3,
    this.retryDelay = const Duration(milliseconds: 100),
    this.shouldRetry,
    this.onUnrecoverableError,
  }) : super('ErrorHandlingMiddleware');

  @override
  Future<void> beforeExecution(ChainExecution execution) async {
    // No action needed before execution for error handling
  }

  @override
  Future<void> afterExecution(ChainExecution execution) async {
    // No action needed after successful execution
  }

  @override
  Future<void> beforeLink(LinkExecution execution) async {
    // No action needed before link execution for error handling
  }

  @override
  Future<void> afterLink(LinkExecution execution) async {
    // No action needed after successful link execution
  }

  @override
  Future<void> onError(LinkExecution execution) async {
    final error = execution.error;
    if (error == null) return;

    // Check if we should retry
    if (shouldRetry?.call(error) ?? _defaultShouldRetry(error)) {
      // Implementation would go here for retry logic
      // This is a simplified version for demonstration
      await onUnrecoverableError?.call(execution);
    } else {
      await onUnrecoverableError?.call(execution);
    }
  }

  bool _defaultShouldRetry(Object error) {
    // Default retry logic - could be expanded based on error types
    return error is! ArgumentError; // Don't retry argument errors
  }
}

/// Log levels for the logging middleware
enum LogLevel {
  /// Only log errors
  error,
  
  /// Log warnings and errors
  warn,
  
  /// Log info, warnings, and errors
  info,
  
  /// Log everything including debug information
  debug,
}

/// Function-based middleware for simple cases
/// 
/// Allows creating middleware from simple functions without defining classes.
/// Useful for quick prototyping and simple monitoring.
class FunctionMiddleware implements Middleware {
  final String name;
  final Future<void> Function(ChainExecution)? _beforeExecution;
  final Future<void> Function(ChainExecution)? _afterExecution;
  final Future<void> Function(LinkExecution)? _beforeLink;
  final Future<void> Function(LinkExecution)? _afterLink;
  final Future<void> Function(LinkExecution)? _onError;

  /// Creates a new function-based middleware
  /// 
  /// [name] - The name of this middleware
  /// [beforeExecution] - Function to call before chain execution
  /// [afterExecution] - Function to call after chain execution
  /// [beforeLink] - Function to call before link execution
  /// [afterLink] - Function to call after link execution
  /// [onError] - Function to call on error
  FunctionMiddleware(
    this.name, {
    Future<void> Function(ChainExecution)? beforeExecution,
    Future<void> Function(ChainExecution)? afterExecution,
    Future<void> Function(LinkExecution)? beforeLink,
    Future<void> Function(LinkExecution)? afterLink,
    Future<void> Function(LinkExecution)? onError,
  })  : _beforeExecution = beforeExecution,
        _afterExecution = afterExecution,
        _beforeLink = beforeLink,
        _afterLink = afterLink,
        _onError = onError;

  @override
  Future<void> beforeExecution(ChainExecution execution) async {
    await _beforeExecution?.call(execution);
  }

  @override
  Future<void> afterExecution(ChainExecution execution) async {
    await _afterExecution?.call(execution);
  }

  @override
  Future<void> beforeLink(LinkExecution execution) async {
    await _beforeLink?.call(execution);
  }

  @override
  Future<void> afterLink(LinkExecution execution) async {
    await _afterLink?.call(execution);
  }

  @override
  Future<void> onError(LinkExecution execution) async {
    await _onError?.call(execution);
  }

  @override
  String toString() => 'FunctionMiddleware($name)';
}