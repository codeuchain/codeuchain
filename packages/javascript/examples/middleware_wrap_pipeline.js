/**
 * Middleware Wrap Example
 *
 * Demonstrates the Middleware Wrap pattern from ASCII_PIPELINES.txt:
 * ```
 * [Ctx] -> [Before MW] -> (Link) -> [After MW] -> [Ctx']
 *               | error
 *               v
 *           [OnError MW]
 * ```
 *
 * This example shows how to wrap links with middleware for
 * cross-cutting concerns like logging, timing, and error handling.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class TimingMiddleware {
  async execute(link, ctx, next) {
    const startTime = Date.now();
    const linkName = link.constructor.name;

    console.log(`⏱️ [${linkName}] Starting execution...`);

    try {
      const result = await next();
      const endTime = Date.now();
      const duration = endTime - startTime;

      console.log(`✅ [${linkName}] Completed in ${duration}ms`);
      return result.insert('executionTime', duration);

    } catch (error) {
      const endTime = Date.now();
      const duration = endTime - startTime;

      console.log(`❌ [${linkName}] Failed after ${duration}ms: ${error.message}`);
      throw error;
    }
  }
}

class ValidationMiddleware {
  async execute(link, ctx, next) {
    const linkName = link.constructor.name;

    // Pre-validation
    console.log(`🔍 [${linkName}] Pre-validation...`);
    const requiredFields = this._getRequiredFields(linkName);

    for (const field of requiredFields) {
      if (!ctx.get(field)) {
        throw new Error(`VALIDATION_ERROR: Missing required field '${field}'`);
      }
    }

    console.log(`✅ [${linkName}] Pre-validation passed`);

    const result = await next();

    // Post-validation
    console.log(`🔍 [${linkName}] Post-validation...`);
    const expectedOutputs = this._getExpectedOutputs(linkName);

    for (const output of expectedOutputs) {
      if (!result.get(output)) {
        throw new Error(`VALIDATION_ERROR: Missing expected output '${output}'`);
      }
    }

    console.log(`✅ [${linkName}] Post-validation passed`);
    return result;
  }

  _getRequiredFields(linkName) {
    const fieldMap = {
      'DataProcessorLink': ['inputData'],
      'ResultFormatterLink': ['processedData'],
      'OutputWriterLink': ['formattedResult']
    };
    return fieldMap[linkName] || [];
  }

  _getExpectedOutputs(linkName) {
    const outputMap = {
      'DataProcessorLink': ['processedData'],
      'ResultFormatterLink': ['formattedResult'],
      'OutputWriterLink': ['outputWritten']
    };
    return outputMap[linkName] || [];
  }
}

class MetricsMiddleware {
  constructor() {
    this.metrics = {
      executions: 0,
      successes: 0,
      failures: 0,
      totalTime: 0
    };
  }

  async execute(link, ctx, next) {
    const linkName = link.constructor.name;
    this.metrics.executions++;

    const startTime = Date.now();

    try {
      const result = await next();
      this.metrics.successes++;
      return result;
    } catch (error) {
      this.metrics.failures++;
      throw error;
    } finally {
      const duration = Date.now() - startTime;
      this.metrics.totalTime += duration;

      console.log(`📊 [${linkName}] Metrics updated - Executions: ${this.metrics.executions}`);
    }
  }

  getMetrics() {
    return {
      ...this.metrics,
      avgTime: this.metrics.executions > 0 ? this.metrics.totalTime / this.metrics.executions : 0,
      successRate: this.metrics.executions > 0 ? (this.metrics.successes / this.metrics.executions) * 100 : 0
    };
  }
}

class DataProcessorLink extends Link {
  async call(ctx) {
    const inputData = ctx.get('inputData');
    console.log(`⚙️ Processing: ${inputData}`);

    // Simulate processing
    await new Promise(resolve => setTimeout(resolve, Math.random() * 200 + 100));

    const processedData = `${inputData}_processed_${Date.now()}`;
    return ctx.insert('processedData', processedData);
  }
}

class ResultFormatterLink extends Link {
  async call(ctx) {
    const processedData = ctx.get('processedData');
    console.log(`🎨 Formatting: ${processedData}`);

    // Simulate formatting
    await new Promise(resolve => setTimeout(resolve, Math.random() * 150 + 50));

    const formattedResult = {
      data: processedData,
      timestamp: new Date().toISOString(),
      format: 'json',
      version: '1.0'
    };

    return ctx.insert('formattedResult', formattedResult);
  }
}

class OutputWriterLink extends Link {
  async call(ctx) {
    const formattedResult = ctx.get('formattedResult');
    console.log(`💾 Writing output...`);

    // Simulate writing
    await new Promise(resolve => setTimeout(resolve, Math.random() * 100 + 50));

    console.log(`📄 Output written: ${JSON.stringify(formattedResult)}`);
    return ctx.insert('outputWritten', true);
  }
}

async function main() {
  console.log('🔧 CodeUChain: Middleware Wrap Example');
  console.log('=' * 42);
  console.log();

  // Create custom middleware instances
  const timingMW = new TimingMiddleware();
  const validationMW = new ValidationMiddleware();
  const metricsMW = new MetricsMiddleware();

  // Create the chain
  const chain = new Chain();

  // Add links
  chain.addLink(new DataProcessorLink());
  chain.addLink(new ResultFormatterLink());
  chain.addLink(new OutputWriterLink());

  // Connect links
  chain.connect('DataProcessorLink', 'ResultFormatterLink');
  chain.connect('ResultFormatterLink', 'OutputWriterLink');

  // Apply middleware to all links
  chain.useMiddleware(timingMW);
  chain.useMiddleware(validationMW);
  chain.useMiddleware(metricsMW);

  // Add error handling middleware
  chain.onError((error, ctx, linkName) => {
    console.error(`🚨 Error in ${linkName}: ${error.message}`);
    console.error(`   Context keys: ${Object.keys(ctx.toObject()).join(', ')}`);

    // Could add error recovery logic here
    return ctx.insert('errorHandled', true);
  });

  // Test data
  const testInputs = [
    { inputData: 'test_data_1' },
    { inputData: 'test_data_2' },
    { inputData: '' }, // This will fail validation
    { inputData: 'test_data_3' }
  ];

  console.log('🧪 Testing Middleware Wrap Pipeline:\n');

  for (let i = 0; i < testInputs.length; i++) {
    const testCase = testInputs[i];
    console.log(`📝 Test Case ${i + 1}: ${JSON.stringify(testCase)}`);
    console.log('─'.repeat(40));

    try {
      const initialCtx = new Context(testCase);
      const resultCtx = await chain.run(initialCtx);

      console.log('✅ Pipeline completed successfully!');
      console.log('📊 Execution times by link:');

      // Show timing information
      const executionTime = resultCtx.get('executionTime');
      if (executionTime) {
        console.log(`   Total execution time: ${executionTime}ms`);
      }

    } catch (error) {
      console.log('❌ Pipeline failed:', error.message);
    }

    console.log('─'.repeat(40));
  }

  // Show final metrics
  console.log('\n📈 Final Middleware Metrics:');
  const finalMetrics = metricsMW.getMetrics();
  console.log(`   Total executions: ${finalMetrics.executions}`);
  console.log(`   Successes: ${finalMetrics.successes}`);
  console.log(`   Failures: ${finalMetrics.failures}`);
  console.log(`   Success rate: ${finalMetrics.successRate.toFixed(1)}%`);
  console.log(`   Average time: ${Math.round(finalMetrics.avgTime)}ms`);

  console.log('\n✨ Middleware Wrap Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Before/After middleware execution');
  console.log('• Error handling middleware');
  console.log('• Cross-cutting concerns (timing, validation, metrics)');
  console.log('• Middleware composition and ordering');
  console.log('• Non-invasive enhancement of link behavior');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };