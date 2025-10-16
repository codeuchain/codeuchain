/// Chain: The Orchestrator
///
/// Ordered sequence of Links in a pipeline.
/// Manages Context flow between Links with error handling and middleware support.
library;

import 'dart:async';
import 'context.dart';
import 'link.dart';
import 'middleware.dart';

/// A chain of processing links with middleware support
/// 
/// The Chain orchestrates the execution of multiple Links in sequence,
/// passing the Context from one link to the next. It supports middleware
/// for cross-cutting concerns like logging, error handling, and metrics.
class Chain<TInput, TOutput> {
  final List<_ChainLink> _links = [];
  final List<Middleware> _middleware = [];
  final String? name;

  /// Creates a new chain with an optional name
  Chain([this.name]);

  /// Adds a link to the end of the chain
  /// 
  /// [link] - The link to add to the chain
  /// Returns this chain for method chaining
  Chain<TInput, TNewOutput> add<TNewOutput>(Link<dynamic, TNewOutput> link, [String? linkName]) {
    final name = linkName ?? link.name;
    _links.add(_ChainLink(link, name));
    return Chain<TInput, TNewOutput>(this.name)
      .._links.addAll(_links)
      .._middleware.addAll(_middleware);
  }

  /// Adds a function-based link to the chain
  /// 
  /// [name] - The name of the link for debugging
  /// [function] - The function that performs the transformation
  /// Returns this chain for method chaining
  Chain<TInput, TNewOutput> addFunction<TNewOutput>(
    String name,
    Future<Context<TNewOutput>> Function(Context) function,
  ) {
    return add(FunctionLink(name, function), name);
  }

  /// Adds a synchronous function-based link to the chain
  /// 
  /// [name] - The name of the link for debugging
  /// [function] - The synchronous function that performs the transformation
  /// Returns this chain for method chaining
  Chain<TInput, TNewOutput> addSyncFunction<TNewOutput>(
    String name,
    Context<TNewOutput> Function(Context) function,
  ) {
    return add(
      FunctionLink(name, (context) async => function(context)),
      name,
    );
  }

  /// Adds middleware to the chain
  /// 
  /// [middleware] - The middleware to add
  /// Returns this chain for method chaining
  Chain<TInput, TOutput> use(Middleware middleware) {
    _middleware.add(middleware);
    return this;
  }

  /// Executes the chain with the given initial context
  /// 
  /// [initialContext] - The starting context for the chain
  /// Returns the final transformed context
  /// 
  /// The execution flow:
  /// 1. Calls beforeExecution on all middleware
  /// 2. For each link:
  ///    - Calls beforeLink on all middleware
  ///    - Executes the link
  ///    - Calls afterLink on all middleware
  /// 3. Calls afterExecution on all middleware
  /// 4. Returns the final context
  Future<Context<TOutput>> run(Context<TInput> initialContext) async {
    final execution = ChainExecution(name ?? 'Chain', initialContext);
    
    // Notify middleware of execution start
    for (final middleware in _middleware) {
      await middleware.beforeExecution(execution);
    }

    Context<dynamic> currentContext = initialContext;

    try {
      for (int i = 0; i < _links.length; i++) {
        final chainLink = _links[i];
        final linkExecution = LinkExecution(
          chainLink.name,
          currentContext,
          i,
          _links.length,
        );

        // Notify middleware before link execution
        for (final middleware in _middleware) {
          await middleware.beforeLink(linkExecution);
        }

        try {
          // Execute the link
          currentContext = await chainLink.link.call(currentContext);
          linkExecution.result = currentContext;

          // Notify middleware after successful link execution
          for (final middleware in _middleware) {
            await middleware.afterLink(linkExecution);
          }
        } catch (error, stackTrace) {
          linkExecution.error = error;
          linkExecution.stackTrace = stackTrace;

          // Notify middleware of error
          for (final middleware in _middleware) {
            await middleware.onError(linkExecution);
          }

          // Re-throw the error
          rethrow;
        }
      }

      execution.result = currentContext;

      // Notify middleware of successful execution
      for (final middleware in _middleware) {
        await middleware.afterExecution(execution);
      }

      return currentContext as Context<TOutput>;
    } catch (error, stackTrace) {
      execution.error = error;
      execution.stackTrace = stackTrace;

      // Notify middleware of chain-level error
      for (final middleware in _middleware) {
        await middleware.onError(LinkExecution('Chain', currentContext, -1, _links.length)
          ..error = error
          ..stackTrace = stackTrace);
      }

      rethrow;
    }
  }

  /// Returns the number of links in the chain
  int get length => _links.length;

  /// Checks if the chain is empty
  bool get isEmpty => _links.isEmpty;

  /// Checks if the chain is not empty
  bool get isNotEmpty => _links.isNotEmpty;

  /// Returns the names of all links in the chain
  List<String> get linkNames => _links.map((link) => link.name).toList();

  @override
  String toString() => 'Chain<$TInput, $TOutput>(${name ?? 'unnamed'}, ${_links.length} links)';
}

/// Internal representation of a link in a chain
class _ChainLink {
  final Link<dynamic, dynamic> link;
  final String name;

  _ChainLink(this.link, this.name);

  @override
  String toString() => 'ChainLink($name)';
}

/// Represents the execution context of an entire chain
class ChainExecution {
  /// The name of the chain being executed
  final String chainName;
  
  /// The initial context passed to the chain
  final Context<dynamic> initialContext;
  
  /// The start time of the execution
  final DateTime startTime;
  
  /// The result context (set after successful execution)
  Context<dynamic>? result;
  
  /// Any error that occurred during execution
  Object? error;
  
  /// Stack trace if an error occurred
  StackTrace? stackTrace;

  ChainExecution(this.chainName, this.initialContext)
      : startTime = DateTime.now();

  /// Duration of the execution (if completed)
  Duration get duration => DateTime.now().difference(startTime);

  /// Whether the execution was successful
  bool get isSuccess => error == null && result != null;

  /// Whether the execution failed
  bool get isError => error != null;

  @override
  String toString() => 'ChainExecution($chainName, ${isSuccess ? 'success' : 'error'})';
}

/// Represents the execution context of a single link
class LinkExecution {
  /// The name of the link being executed
  final String linkName;
  
  /// The input context for the link
  final Context<dynamic> inputContext;
  
  /// The index of this link in the chain
  final int linkIndex;
  
  /// The total number of links in the chain
  final int totalLinks;
  
  /// The start time of the link execution
  final DateTime startTime;
  
  /// The result context (set after successful execution)
  Context<dynamic>? result;
  
  /// Any error that occurred during link execution
  Object? error;
  
  /// Stack trace if an error occurred
  StackTrace? stackTrace;

  LinkExecution(this.linkName, this.inputContext, this.linkIndex, this.totalLinks)
      : startTime = DateTime.now();

  /// Duration of the link execution (if completed)
  Duration get duration => DateTime.now().difference(startTime);

  /// Whether the link execution was successful
  bool get isSuccess => error == null && result != null;

  /// Whether the link execution failed
  bool get isError => error != null;

  @override
  String toString() => 'LinkExecution($linkName, ${isSuccess ? 'success' : 'error'})';
}