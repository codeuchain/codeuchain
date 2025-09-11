/**
 * Retry with Backoff Example
 *
 * Demonstrates the Retry with Backoff pattern from ASCII_PIPELINES.txt:
 * ```
 * +---------+  failure   +-----------+
 * | Attempt | ---------> | Backoff n | --+
 * +----+----+            +-----------+   |
 *      ^                                  |
 *      +-------------- success <----------+
 * ```
 *
 * This example shows how to implement retry logic with exponential backoff
 * for handling transient failures in processing pipelines.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class RetryableProcessorLink extends Link {
  constructor(maxRetries = 3, baseDelay = 1000) {
    super();
    this.maxRetries = maxRetries;
    this.baseDelay = baseDelay;
    this.attemptCount = 0;
  }

  async call(ctx) {
    const data = ctx.get('inputData');
    const operation = ctx.get('operation') || 'process';

    console.log(`⚙️ Processing "${data}" with operation: ${operation}`);

    // Reset attempt count for new processing
    this.attemptCount = 0;

    // Try processing with retry logic
    return await this._processWithRetry(ctx, data, operation);
  }

  async _processWithRetry(ctx, data, operation) {
    this.attemptCount++;

    try {
      // Simulate processing that might fail
      const result = await this._attemptProcessing(data, operation);

      console.log(`✅ Processing succeeded on attempt ${this.attemptCount}`);
      return ctx
        .insert('processedData', result)
        .insert('attempts', this.attemptCount)
        .insert('success', true);

    } catch (error) {
      console.log(`❌ Attempt ${this.attemptCount} failed: ${error.message}`);

      if (this.attemptCount < this.maxRetries) {
        // Calculate backoff delay with exponential backoff + jitter
        const backoffDelay = this._calculateBackoffDelay(this.attemptCount);

        console.log(`⏳ Retrying in ${backoffDelay}ms (attempt ${this.attemptCount + 1}/${this.maxRetries})`);

        // Wait for backoff delay
        await new Promise(resolve => setTimeout(resolve, backoffDelay));

        // Retry recursively
        return await this._processWithRetry(ctx, data, operation);
      } else {
        // Max retries exceeded
        console.log(`💥 Max retries (${this.maxRetries}) exceeded`);
        throw new Error(`PROCESSING_FAILED: Failed after ${this.attemptCount} attempts. Last error: ${error.message}`);
      }
    }
  }

  async _attemptProcessing(data, operation) {
    // Simulate different types of failures based on input
    if (data.includes('temporary_error') && Math.random() < 0.7) {
      // 70% chance of temporary failure
      throw new Error('TEMPORARY_ERROR: Network timeout');
    }

    if (data.includes('intermittent_error') && Math.random() < 0.5) {
      // 50% chance of intermittent failure
      throw new Error('TEMPORARY_ERROR: Service unavailable');
    }

    if (data.includes('persistent_error')) {
      // Always fails
      throw new Error('PERSISTENT_ERROR: Invalid configuration');
    }

    // Simulate processing time
    const processingTime = Math.random() * 500 + 200;
    await new Promise(resolve => setTimeout(resolve, processingTime));

    // Return successful result
    return `${data}_${operation}_success_${Date.now()}`;
  }

  _calculateBackoffDelay(attemptNumber) {
    // Exponential backoff: baseDelay * 2^(attempt-1) + jitter
    const exponentialDelay = this.baseDelay * Math.pow(2, attemptNumber - 1);
    const jitter = Math.random() * 0.1 * exponentialDelay; // 10% jitter
    return Math.floor(exponentialDelay + jitter);
  }
}

class ResultAnalyzerLink extends Link {
  async call(ctx) {
    const processedData = ctx.get('processedData');
    const attempts = ctx.get('attempts');
    const success = ctx.get('success');

    console.log(`📊 Analyzing result:`);
    console.log(`   Success: ${success}`);
    console.log(`   Attempts: ${attempts}`);
    console.log(`   Result: ${processedData}`);

    const analysis = {
      success,
      attempts,
      retryRate: attempts > 1 ? ((attempts - 1) / attempts * 100).toFixed(1) + '%' : '0%',
      processingId: `proc_${Date.now()}`,
      timestamp: new Date().toISOString()
    };

    return ctx.insert('analysis', analysis);
  }
}

class BackoffMetricsCollectorLink extends Link {
  constructor() {
    super();
    this.metrics = {
      totalAttempts: 0,
      successfulRetries: 0,
      failedRetries: 0,
      averageAttempts: 0,
      backoffPatterns: []
    };
  }

  async call(ctx) {
    const analysis = ctx.get('analysis');
    const attempts = ctx.get('attempts');

    // Update metrics
    this.metrics.totalAttempts += attempts;
    if (analysis.success && attempts > 1) {
      this.metrics.successfulRetries++;
    } else if (!analysis.success) {
      this.metrics.failedRetries++;
    }

    // Track backoff pattern
    this.metrics.backoffPatterns.push({
      attempts,
      success: analysis.success,
      timestamp: analysis.timestamp
    });

    // Calculate running average
    const totalProcessed = this.metrics.successfulRetries + this.metrics.failedRetries;
    this.metrics.averageAttempts = totalProcessed > 0 ?
      (this.metrics.totalAttempts / totalProcessed).toFixed(2) : 0;

    console.log(`📈 Updated metrics:`);
    console.log(`   Total attempts: ${this.metrics.totalAttempts}`);
    console.log(`   Successful retries: ${this.metrics.successfulRetries}`);
    console.log(`   Failed retries: ${this.metrics.failedRetries}`);
    console.log(`   Average attempts: ${this.metrics.averageAttempts}`);

    return ctx.insert('metrics', { ...this.metrics });
  }

  getMetrics() {
    return { ...this.metrics };
  }
}

async function main() {
  console.log('🔄 CodeUChain: Retry with Backoff Example');
  console.log('=' * 45);
  console.log();

  // Create the retry chain
  const chain = new Chain();

  // Create metrics collector (shared across runs)
  const metricsCollector = new BackoffMetricsCollectorLink();

  // Add links
  chain.addLink(new RetryableProcessorLink(3, 500)); // 3 retries, 500ms base delay
  chain.addLink(new ResultAnalyzerLink());
  chain.addLink(metricsCollector);

  // Connect links
  chain.connect('RetryableProcessorLink', 'ResultAnalyzerLink');
  chain.connect('ResultAnalyzerLink', 'BackoffMetricsCollectorLink');

  // Add middleware
  chain.useMiddleware(new LoggingMiddleware());

  // Test data with different failure scenarios
  const testInputs = [
    { inputData: 'normal_data', operation: 'transform' },
    { inputData: 'data_with_temporary_error', operation: 'validate' },
    { inputData: 'data_with_intermittent_error', operation: 'process' },
    { inputData: 'data_with_persistent_error', operation: 'save' },
    { inputData: 'another_temporary_error', operation: 'analyze' },
    { inputData: 'mixed_failure_scenario', operation: 'convert' }
  ];

  console.log('🧪 Testing Retry with Backoff:');
  console.log();

  for (let i = 0; i < testInputs.length; i++) {
    const testCase = testInputs[i];
    console.log(`📝 Test Case ${i + 1}: ${JSON.stringify(testCase)}`);
    console.log('─'.repeat(50));

    try {
      const initialCtx = new Context(testCase);
      const resultCtx = await chain.run(initialCtx);

      const analysis = resultCtx.get('analysis');
      console.log('✅ Processing completed!');
      console.log(`📊 Result: ${analysis.success ? 'SUCCESS' : 'FAILED'}`);
      console.log(`🔄 Attempts: ${analysis.attempts}`);
      console.log(`📈 Retry Rate: ${analysis.retryRate}`);

    } catch (error) {
      console.log('❌ Processing failed permanently:', error.message);
    }

    console.log('─'.repeat(50));
  }

  // Show final metrics
  console.log('\n📈 FINAL METRICS SUMMARY:');
  const finalMetrics = metricsCollector.getMetrics();
  console.log(`   Total processing attempts: ${finalMetrics.totalAttempts}`);
  console.log(`   Successful retries: ${finalMetrics.successfulRetries}`);
  console.log(`   Failed retries: ${finalMetrics.failedRetries}`);
  console.log(`   Average attempts per operation: ${finalMetrics.averageAttempts}`);
  console.log(`   Total operations processed: ${finalMetrics.backoffPatterns.length}`);

  // Show backoff patterns
  console.log('\n🔄 Backoff Patterns:');
  finalMetrics.backoffPatterns.forEach((pattern, index) => {
    console.log(`   ${index + 1}. Attempts: ${pattern.attempts}, Success: ${pattern.success}`);
  });

  console.log('\n✨ Retry with Backoff Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Exponential backoff with jitter');
  console.log('• Configurable retry limits');
  console.log('• Different failure types (temporary vs persistent)');
  console.log('• Metrics collection and analysis');
  console.log('• Graceful handling of transient failures');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };