import 'package:codeuchain/codeuchain.dart';
import 'package:test/test.dart';

// Test helper links
class AddLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  AddLink() : super('AddLink');

  @override
  Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
    final a = context.get('a') as int? ?? 0;
    final b = context.get('b') as int? ?? 0;
    return context.insert('result', a + b);
  }
}

class MultiplyLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  final int factor;
  
  MultiplyLink(this.factor) : super('MultiplyLink');

  @override
  Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
    final value = context.get('result') as int? ?? 0;
    return context.insert('result', value * factor);
  }
}

class ErrorLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  ErrorLink() : super('ErrorLink');

  @override
  Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
    throw Exception('Test error');
  }
}

void main() {
  group('Context Tests', () {
    test('should create empty context', () {
      final context = Context<Map<String, dynamic>>();
      expect(context.isEmpty, isTrue);
      expect(context.length, equals(0));
    });

    test('should create context with initial data', () {
      final context = Context<Map<String, dynamic>>({'name': 'Alice', 'age': 30});
      expect(context.get('name'), equals('Alice'));
      expect(context.get('age'), equals(30));
      expect(context.length, equals(2));
    });

    test('should maintain immutability on insert', () {
      final original = Context<Map<String, dynamic>>({'name': 'Alice'});
      final updated = original.insert('age', 30);
      
      expect(original.get('age'), isNull);
      expect(updated.get('age'), equals(30));
      expect(updated.get('name'), equals('Alice'));
    });

    test('should support type evolution with insertAs', () {
      final inputContext = Context<Map<String, dynamic>>({'numbers': [1, 2, 3]});
      final outputContext = inputContext.insertAs<Map<String, dynamic>>('result', 6);
      
      expect(outputContext.get('result'), equals(6));
      expect(outputContext.get('numbers'), equals([1, 2, 3]));
    });

    test('should merge contexts correctly', () {
      final context1 = Context<Map<String, dynamic>>({'a': 1, 'b': 2});
      final context2 = Context<Map<String, dynamic>>({'c': 3, 'b': 20}); // b should be overwritten
      final merged = context1.merge(context2);
      
      expect(merged.get('a'), equals(1));
      expect(merged.get('b'), equals(20)); // From context2
      expect(merged.get('c'), equals(3));
    });

    test('should convert to and from mutable context', () {
      final immutable = Context<Map<String, dynamic>>({'name': 'Alice'});
      final mutable = immutable.toMutable();
      
      mutable.set('age', 30);
      expect(mutable.get('age'), equals(30));
      expect(immutable.get('age'), isNull); // Original unchanged
      
      final backToImmutable = mutable.toImmutable();
      expect(backToImmutable.get('age'), equals(30));
    });
  });

  group('Link Tests', () {
    test('should execute simple link', () async {
      final link = AddLink();
      final context = Context<Map<String, dynamic>>({'a': 10, 'b': 20});
      final result = await link.call(context);
      
      expect(result.get('result'), equals(30));
      expect(result.get('a'), equals(10));
      expect(result.get('b'), equals(20));
    });

    test('should handle function links', () async {
      final link = FunctionLink<Map<String, dynamic>, Map<String, dynamic>>(
        'SquareLink',
        (context) async {
          final value = context.get('value') as int;
          return context.insert('result', value * value);
        },
      );
      
      final context = Context<Map<String, dynamic>>({'value': 5});
      final result = await link.call(context);
      
      expect(result.get('result'), equals(25));
    });

    test('should handle link errors', () async {
      final link = ErrorLink();
      final context = Context<Map<String, dynamic>>();
      
      expect(
        () async => await link.call(context),
        throwsA(isA<LinkExecutionException>()),
      );
    });
  });

  group('Chain Tests', () {
    test('should execute simple chain', () async {
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
        .add(AddLink())
        .add(MultiplyLink(2));
      
      final context = Context<Map<String, dynamic>>({'a': 10, 'b': 20});
      final result = await chain.run(context);
      
      expect(result.get('result'), equals(60)); // (10 + 20) * 2
    });

    test('should support function-based links in chain', () async {
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
        .addSyncFunction('SetValue', (context) => context.insert('value', 42))
        .addFunction('DoubleValue', (context) async {
          final value = context.get('value') as int;
          return context.insert('doubled', value * 2);
        });
      
      final result = await chain.run(Context<Map<String, dynamic>>());
      
      expect(result.get('value'), equals(42));
      expect(result.get('doubled'), equals(84));
    });

    test('should propagate errors in chain', () async {
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
        .add(AddLink())
        .add(ErrorLink());
      
      final context = Context<Map<String, dynamic>>({'a': 10, 'b': 20});
      
      expect(
        () async => await chain.run(context),
        throwsA(isA<Exception>()),
      );
    });

    test('should provide chain information', () {
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>('TestChain')
        .add(AddLink(), 'AddLink')
        .add(MultiplyLink(2), 'MultiplyLink');
      
      expect(chain.length, equals(2));
      expect(chain.isNotEmpty, isTrue);
      expect(chain.linkNames, equals(['AddLink', 'MultiplyLink']));
    });
  });

  group('Middleware Tests', () {
    test('should execute logging middleware', () async {
      final logs = <String>[];
      final middleware = LoggingMiddleware(
        logLevel: LogLevel.debug,
        logFunction: (message) => logs.add(message),
      );
      
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>('TestChain')
        .add(AddLink(), 'AddLink')
        .use(middleware);
      
      final context = Context<Map<String, dynamic>>({'a': 10, 'b': 20});
      await chain.run(context);
      
      expect(logs, isNotEmpty);
      // Check for any of the expected log messages
      final hasExecutionLogs = logs.any((log) => 
        log.contains('🚀') || 
        log.contains('✅ Completed') ||
        log.contains('🔗 Executing') ||
        log.contains('📊'));
      expect(hasExecutionLogs, isTrue);
    });

    test('should track performance metrics', () async {
      final performanceMiddleware = PerformanceMiddleware();
      
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>('TestChain')
        .add(AddLink(), 'AddLink')
        .use(performanceMiddleware);
      
      final context = Context<Map<String, dynamic>>({'a': 10, 'b': 20});
      await chain.run(context);
      
      final metrics = performanceMiddleware.getMetrics();
      expect(metrics['chains']['TestChain']?['executions'], equals(1));
      expect(metrics['links']['AddLink']?['executions'], equals(1));
    });

    test('should handle errors in middleware', () async {
      final errorLogs = <String>[];
      final middleware = FunctionMiddleware(
        'TestMiddleware',
        onError: (execution) async {
          errorLogs.add('Error in ${execution.linkName}: ${execution.error}');
        },
      );
      
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
        .add(ErrorLink(), 'ErrorLink')
        .use(middleware);
      
      final context = Context<Map<String, dynamic>>();
      
      try {
        await chain.run(context);
      } catch (e) {
        // Expected to throw
      }
      
      expect(errorLogs, isNotEmpty);
      expect(errorLogs.first, contains('Error in ErrorLink'));
    });
  });

  group('Typed Features Tests', () {
    test('should support generic type evolution', () {
      final inputContext = Context<Map<String, dynamic>>({'numbers': [1, 2, 3]});
      final outputContext = inputContext.insertAs<Map<String, dynamic>>('result', 6.0);
      
      expect(outputContext.get('result'), equals(6.0));
      expect(outputContext.get('numbers'), equals([1, 2, 3]));
    });

    test('should maintain type safety in links', () async {
      // This test demonstrates type safety at compile time
      // The type system prevents incorrect type assignments
      final link = FunctionLink<Map<String, dynamic>, Map<String, dynamic>>(
        'TypedLink',
        (context) async {
          final numbers = context.get('numbers') as List<int>;
          final sum = numbers.fold<int>(0, (a, b) => a + b);
          return context.insertAs<Map<String, dynamic>>('sum', sum);
        },
      );
      
      final context = Context<Map<String, dynamic>>({'numbers': [1, 2, 3, 4, 5]});
      final result = await link.call(context);
      
      expect(result.get('sum'), equals(15));
    });

    test('should support chain type evolution', () async {
      final chain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
        .addFunction('ProcessNumbers', (context) async {
          final numbers = context.get('numbers') as List<int>;
          final sum = numbers.fold<int>(0, (a, b) => a + b);
          return context.insertAs<Map<String, dynamic>>('sum', sum);
        })
        .addFunction('CalculateAverage', (context) async {
          final sum = context.get('sum') as int;
          final numbers = context.get('numbers') as List<int>;
          final average = sum / numbers.length;
          return context.insertAs<Map<String, dynamic>>('average', average);
        });
      
      final input = Context<Map<String, dynamic>>({'numbers': [1, 2, 3, 4, 5]});
      final result = await chain.run(input);
      
      expect(result.get('sum'), equals(15));
      expect(result.get('average'), equals(3.0));
      expect(result.get('numbers'), equals([1, 2, 3, 4, 5]));
    });
  });

  group('Runtime Compatibility Tests', () {
    test('should work without explicit typing', () async {
      // Test that the library works without generic type annotations
      final context = Context({'data': 'test'});
      final link = FunctionLink(
        'SimpleLink',
        (ctx) async => ctx.insert('processed', true),
      );
      
      final result = await link.call(context);
      expect(result.get('processed'), isTrue);
      expect(result.get('data'), equals('test'));
    });

    test('should support mixed typed and untyped usage', () async {
      // Typed link
      final typedLink = FunctionLink<dynamic, dynamic>(
        'TypedLink',
        (context) async => context.insert('typed', true),
      );
      
      // Untyped link
      final untypedLink = FunctionLink<dynamic, dynamic>(
        'UntypedLink',
        (context) async => context.insert('untyped', true),
      );
      
      final chain = Chain<dynamic, dynamic>()
        .add(typedLink)
        .add(untypedLink);
      
      final result = await chain.run(Context({'start': true}));
      
      expect(result.get('start'), isTrue);
      expect(result.get('typed'), isTrue);
      expect(result.get('untyped'), isTrue);
    });
  });
}
