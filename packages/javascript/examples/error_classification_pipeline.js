/**
 * Error Classification Side Path Example
 *
 * Demonstrates the Error Classification Side Path pattern from ASCII_PIPELINES.txt:
 * ```
 * (Link) -X-> [Error?]--yes--> (Classify) -> (Retry or Fail)
 *    | no
 *    v
 * Next Link
 * ```
 *
 * This example shows how to handle errors by routing them through
 * classification and recovery paths.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class DataProcessorLink extends Link {
  async call(ctx) {
    const data = ctx.get('inputData');
    const operation = ctx.get('operation') || 'process';

    console.log(`⚙️ Processing "${data}" with operation: ${operation}`);

    // Simulate different types of errors based on input
    if (data.includes('error')) {
      if (data.includes('temporary')) {
        throw new Error('TEMPORARY_ERROR: Network timeout');
      } else if (data.includes('validation')) {
        throw new Error('VALIDATION_ERROR: Invalid format');
      } else if (data.includes('auth')) {
        throw new Error('AUTH_ERROR: Unauthorized access');
      } else {
        throw new Error('UNKNOWN_ERROR: Unexpected failure');
      }
    }

    // Simulate successful processing
    const result = `${data}_${operation}_success`;
    console.log(`✅ Processing successful: ${result}`);

    return ctx.insert('processedData', result);
  }
}

class ErrorClassifierLink extends Link {
  async call(ctx) {
    const error = ctx.get('error');
    const errorMessage = error.message;

    console.log(`🔍 Classifying error: ${errorMessage}`);

    let errorType, retryable, retryDelay;

    if (errorMessage.includes('TEMPORARY_ERROR')) {
      errorType = 'temporary';
      retryable = true;
      retryDelay = 1000; // 1 second
    } else if (errorMessage.includes('VALIDATION_ERROR')) {
      errorType = 'validation';
      retryable = false;
      retryDelay = 0;
    } else if (errorMessage.includes('AUTH_ERROR')) {
      errorType = 'auth';
      retryable = false;
      retryDelay = 0;
    } else {
      errorType = 'unknown';
      retryable = true;
      retryDelay = 2000; // 2 seconds
    }

    console.log(`📋 Classified as: ${errorType} (${retryable ? 'retryable' : 'non-retryable'})`);

    return ctx
      .insert('errorType', errorType)
      .insert('retryable', retryable)
      .insert('retryDelay', retryDelay)
      .insert('classified', true);
  }
}

class RetryHandlerLink extends Link {
  constructor() {
    super();
    this.retryCount = 0;
  }

  async call(ctx) {
    const retryable = ctx.get('retryable');
    const retryDelay = ctx.get('retryDelay');
    const errorType = ctx.get('errorType');

    if (!retryable) {
      console.log(`🚫 Non-retryable error (${errorType}), failing permanently`);
      throw new Error(`PERMANENT_FAILURE: ${errorType} error cannot be retried`);
    }

    this.retryCount++;
    console.log(`🔄 Retry #${this.retryCount} for ${errorType} error`);

    if (this.retryCount >= 3) {
      console.log(`💥 Max retries exceeded, failing permanently`);
      throw new Error(`MAX_RETRIES_EXCEEDED: Failed after ${this.retryCount} attempts`);
    }

    // Simulate retry delay
    await new Promise(resolve => setTimeout(resolve, retryDelay));

    // For demo purposes, assume temporary errors resolve after 2 retries
    if (this.retryCount >= 2 && errorType === 'temporary') {
      console.log(`🎉 Temporary error resolved after retry`);
      return ctx.insert('retrySuccess', true);
    }

    // If still failing, throw original error to trigger another retry
    throw ctx.get('error');
  }
}

class SuccessHandlerLink extends Link {
  async call(ctx) {
    const processedData = ctx.get('processedData');
    console.log(`🎯 Processing completed successfully: ${processedData}`);

    return ctx.insert('finalStatus', 'success');
  }
}

class FailureHandlerLink extends Link {
  async call(ctx) {
    const errorType = ctx.get('errorType');
    const error = ctx.get('error');

    console.log(`❌ Processing failed permanently: ${errorType}`);
    console.log(`   Error: ${error.message}`);

    return ctx.insert('finalStatus', 'failed');
  }
}

async function main() {
  console.log('🚨 CodeUChain: Error Classification Side Path Example');
  console.log('=' * 58);
  console.log();

  // Create the error handling chain
  const chain = new Chain();

  // Add all links
  chain.addLink(new DataProcessorLink());
  chain.addLink(new ErrorClassifierLink());
  chain.addLink(new RetryHandlerLink());
  chain.addLink(new SuccessHandlerLink());
  chain.addLink(new FailureHandlerLink());

  // Connect in error classification pattern
  chain.connect('DataProcessorLink', 'SuccessHandlerLink'); // Success path

  // Error handling setup
  chain.onError(async (error, ctx, linkName) => {
    console.log(`\n⚠️ Error detected in ${linkName}: ${error.message}`);

    // Route to error classification
    const errorCtx = ctx.insert('error', error);
    const classifiedCtx = await chain.runLink('ErrorClassifierLink', errorCtx);

    // Route based on classification
    const retryable = classifiedCtx.get('retryable');
    if (retryable) {
      console.log('🔄 Routing to retry handler...');
      try {
        const retryCtx = await chain.runLink('RetryHandlerLink', classifiedCtx);
        if (retryCtx.get('retrySuccess')) {
          // Retry successful, continue with success path
          console.log('✅ Retry successful, continuing...');
          return await chain.runLink('SuccessHandlerLink', retryCtx);
        }
      } catch (retryError) {
        console.log('❌ Retry failed, routing to failure handler...');
        return await chain.runLink('FailureHandlerLink', classifiedCtx.insert('error', retryError));
      }
    } else {
      console.log('🚫 Non-retryable error, routing to failure handler...');
      return await chain.runLink('FailureHandlerLink', classifiedCtx);
    }
  });

  // Add middleware
  chain.useMiddleware(new LoggingMiddleware());

  // Test data with different error scenarios
  const testInputs = [
    { inputData: 'normal_data', operation: 'transform' },
    { inputData: 'data_with_temporary_error', operation: 'validate' },
    { inputData: 'data_with_validation_error', operation: 'process' },
    { inputData: 'data_with_auth_error', operation: 'save' },
    { inputData: 'data_with_unknown_error', operation: 'analyze' }
  ];

  console.log('🧪 Testing Error Classification Pipeline:\n');

  for (const testCase of testInputs) {
    console.log(`📝 Processing: ${JSON.stringify(testCase)}`);
    console.log('─'.repeat(50));

    try {
      const initialCtx = new Context(testCase);
      const resultCtx = await chain.run(initialCtx);

      const finalStatus = resultCtx.get('finalStatus');
      console.log(`🏁 Final Status: ${finalStatus.toUpperCase()}`);

      if (finalStatus === 'success') {
        console.log(`📊 Result: ${resultCtx.get('processedData')}`);
      }

    } catch (error) {
      console.log('💥 Unhandled error:', error.message);
    }

    console.log('='.repeat(70));
    console.log();
  }

  console.log('✨ Error Classification Side Path Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Error detection and classification');
  console.log('• Conditional routing based on error type');
  console.log('• Retry logic for temporary failures');
  console.log('• Permanent failure handling');
  console.log('• Complex error recovery patterns');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };