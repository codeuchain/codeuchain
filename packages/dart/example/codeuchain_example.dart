import 'package:codeuchain/codeuchain.dart';

/// Example demonstrating CodeUChain Dart usage
/// 
/// This example shows how to create links, chains, and use middleware
/// with typed features support.

// Define data types for typed features
class UserData {
  final String name;
  final int age;
  
  UserData(this.name, this.age);
  
  Map<String, dynamic> toMap() => {'name': name, 'age': age};
}

class ProcessedUser {
  final String name;
  final int age;
  final String status;
  final DateTime processedAt;
  
  ProcessedUser(this.name, this.age, this.status, this.processedAt);
  
  Map<String, dynamic> toMap() => {
    'name': name,
    'age': age, 
    'status': status,
    'processedAt': processedAt.toIso8601String(),
  };
}

// Define processing links
class ValidateUserLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  ValidateUserLink() : super('ValidateUser');

  @override
  Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
    final name = context.get('name') as String?;
    final age = context.get('age') as int?;
    
    if (name == null || name.isEmpty) {
      throw ArgumentError('Name is required');
    }
    
    if (age == null || age < 0 || age > 150) {
      throw ArgumentError('Valid age is required (0-150)');
    }
    
    return context.insert('validated', true);
  }
}

class ProcessUserLink extends BaseLink<Map<String, dynamic>, Map<String, dynamic>> {
  ProcessUserLink() : super('ProcessUser');

  @override
  Future<Context<Map<String, dynamic>>> execute(Context<Map<String, dynamic>> context) async {
    final age = context.get('age') as int;
    
    String status;
    if (age < 18) {
      status = 'minor';
    } else if (age < 65) {
      status = 'adult';
    } else {
      status = 'senior';
    }
    
    return context
      .insert('status', status)
      .insert('processedAt', DateTime.now().toIso8601String());
  }
}

void main() async {
  print('🎯 CodeUChain Dart Example');
  print('==========================\n');

  // Example 1: Simple Chain with Typed Features
  print('📋 Example 1: Basic Chain Processing');
  
  final userChain = Chain<Map<String, dynamic>, Map<String, dynamic>>('UserProcessing')
    .add(ValidateUserLink())
    .add(ProcessUserLink())
    .use(LoggingMiddleware(logLevel: LogLevel.info));

  final userData = Context<Map<String, dynamic>>({
    'name': 'Alice Smith',
    'age': 28,
  });

  try {
    final result = await userChain.run(userData);
    print('✅ User processed successfully:');
    print('   Name: ${result.get('name')}');
    print('   Age: ${result.get('age')}');
    print('   Status: ${result.get('status')}');
    print('   Processed At: ${result.get('processedAt')}');
  } catch (e) {
    print('❌ Processing failed: $e');
  }

  print('\n📊 Example 2: Function-based Links');
  
  // Example 2: Function-based links for quick prototyping
  final mathChain = Chain<Map<String, dynamic>, Map<String, dynamic>>('MathOperations')
    .addSyncFunction('Add', (context) {
      final a = context.get('a') as int? ?? 0;
      final b = context.get('b') as int? ?? 0;
      return context.insert('sum', a + b);
    })
    .addFunction('Square', (context) async {
      final sum = context.get('sum') as int;
      await Future.delayed(Duration(milliseconds: 10)); // Simulate async work
      return context.insert('squared', sum * sum);
    })
    .addSyncFunction('FormatResult', (context) {
      final sum = context.get('sum') as int;
      final squared = context.get('squared') as int;
      return context.insert('message', '$sum squared is $squared');
    });

  final mathData = Context<Map<String, dynamic>>({'a': 5, 'b': 7});
  final mathResult = await mathChain.run(mathData);
  
  print('🧮 Math Result: ${mathResult.get('message')}');

  print('\n🔍 Example 3: Middleware and Performance Monitoring');
  
  // Example 3: Advanced middleware usage
  final performanceMiddleware = PerformanceMiddleware(
    slowExecutionThreshold: 5,
    onSlowExecution: (name, duration) {
      print('⚠️  Slow execution detected: $name took ${duration.inMilliseconds}ms');
    },
  );

  final monitoredChain = Chain<Map<String, dynamic>, Map<String, dynamic>>('MonitoredChain')
    .addFunction('SlowOperation', (context) async {
      await Future.delayed(Duration(milliseconds: 10)); // Simulate slow work
      return context.insert('slow_result', 'completed');
    })
    .addFunction('FastOperation', (context) async {
      return context.insert('fast_result', 'completed');
    })
    .use(performanceMiddleware)
    .use(LoggingMiddleware(logLevel: LogLevel.debug));

  await monitoredChain.run(Context<Map<String, dynamic>>({'input': 'test'}));
  
  print('📈 Performance Metrics:');
  final metrics = performanceMiddleware.getMetrics();
  print('   Chains executed: ${metrics['chains'].length}');
  print('   Links executed: ${metrics['links'].length}');

  print('\n🎨 Example 4: Type Evolution');
  
  // Example 4: Type evolution with insertAs
  final inputContext = Context<Map<String, dynamic>>({'numbers': [1, 2, 3, 4, 5]});
  
  final typeEvolutionChain = Chain<Map<String, dynamic>, Map<String, dynamic>>()
    .addFunction('CalculateSum', (context) async {
      final numbers = context.get('numbers') as List<int>;
      final sum = numbers.fold<int>(0, (a, b) => a + b);
      return context.insertAs<Map<String, dynamic>>('sum', sum);
    })
    .addFunction('CalculateStats', (context) async {
      final numbers = context.get('numbers') as List<int>;
      final sum = context.get('sum') as int;
      final average = sum / numbers.length;
      final max = numbers.reduce((a, b) => a > b ? a : b);
      final min = numbers.reduce((a, b) => a < b ? a : b);
      
      return context.insertAs<Map<String, dynamic>>('stats', {
        'sum': sum,
        'average': average,
        'max': max,
        'min': min,
        'count': numbers.length,
      });
    });

  final statsResult = await typeEvolutionChain.run(inputContext);
  final stats = statsResult.get('stats') as Map<String, dynamic>;
  
  print('🔢 Statistics for ${statsResult.get('numbers')}:');
  print('   Sum: ${stats['sum']}');
  print('   Average: ${stats['average']}');
  print('   Max: ${stats['max']}');
  print('   Min: ${stats['min']}');
  print('   Count: ${stats['count']}');

  print('\n✨ Example 5: Error Handling');
  
  // Example 5: Error handling and recovery
  final errorHandlingChain = Chain<Map<String, dynamic>, Map<String, dynamic>>('ErrorHandling')
    .addFunction('NormalOperation', (context) async {
      return context.insert('step1', 'completed');
    })
    .addFunction('FaultyOperation', (context) async {
      final shouldError = context.get('causeError') as bool? ?? false;
      if (shouldError) {
        throw Exception('Simulated error');
      }
      return context.insert('step2', 'completed');
    })
    .addFunction('FinalOperation', (context) async {
      return context.insert('step3', 'completed');
    })
    .use(ErrorHandlingMiddleware())
    .use(FunctionMiddleware(
      'ErrorLogger',
      onError: (execution) async {
        print('🚨 Error in ${execution.linkName}: ${execution.error}');
      },
    ));

  // Test successful execution
  print('🟢 Testing successful execution:');
  final successResult = await errorHandlingChain.run(
    Context<Map<String, dynamic>>({'input': 'test', 'causeError': false})
  );
  print('   Results: step1=${successResult.get('step1')}, step2=${successResult.get('step2')}, step3=${successResult.get('step3')}');

  // Test error handling
  print('🔴 Testing error handling:');
  try {
    await errorHandlingChain.run(
      Context<Map<String, dynamic>>({'input': 'test', 'causeError': true})
    );
  } catch (e) {
    print('   Error was properly caught and logged.');
  }

  print('\n🎊 CodeUChain Dart examples completed successfully!');
}
