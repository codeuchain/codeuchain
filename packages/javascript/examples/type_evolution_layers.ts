/**
 * TypeScript: Type Evolution Layers Example
 *
 * Demonstrates the Type Evolution Layers pattern from ASCII_PIPELINES.txt:
 * ```
 * Context<T0>
 *   add validated -> Context<T1>
 *   add parsed    -> Context<T2>
 *   add enriched  -> Context<T3>
 * ```
 *
 * This example shows clean type evolution through processing layers
 * using TypeScript generics and the insertAs() method.
 */

// Import types and classes (assuming TypeScript definitions exist)
import { Context, Chain, Link, LoggingMiddleware } from '../core';

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

interface RawInput {
  rawData: string;
  source: string;
}

interface ValidatedInput extends RawInput {
  isValid: boolean;
  validationErrors: string[];
}

interface ParsedInput extends ValidatedInput {
  parsedData: any;
  parseTimestamp: string;
}

interface EnrichedInput extends ParsedInput {
  enrichedData: any;
  enrichmentMetadata: {
    confidence: number;
    processingTime: number;
    enrichmentsApplied: string[];
  };
}

interface ProcessedResult extends EnrichedInput {
  result: any;
  processingId: string;
  completedAt: string;
}

// =============================================================================
// INTERFACES
// =============================================================================

/**
 * Interface for data processing chains that handle raw input to processed results
 *
 * This interface defines the contract for any data processing chain that:
 * - Takes raw input data in a Context<RawInput>
 * - Processes it through multiple stages with type evolution
 * - Returns processed results in a Context<ProcessedResult>
 *
 * Benefits of this interface:
 * - Enables dependency injection and testing with mocks
 * - Provides clear contract for different implementations
 * - Supports the Strategy pattern for different processing approaches
 * - Allows for better type safety and IntelliSense
 */
interface IDataProcessingChain {
  /**
   * Process raw input data through the entire pipeline
   * @param initialCtx - The initial context containing raw input data
   * @returns Promise resolving to context with processed results
   */
  processData(initialCtx: Context<RawInput>): Promise<Context<ProcessedResult>>;
}// =============================================================================
// TYPED LINK IMPLEMENTATIONS
// =============================================================================

class InputValidatorLink extends Link {
  async call(ctx: Context<RawInput>): Promise<Context<ValidatedInput>> {
    const rawData = ctx.get('rawData');
    const source = ctx.get('source');

    console.log(`🔍 Validating input from ${source}: ${rawData}`);

    // Validation logic
    const validationErrors: string[] = [];
    let isValid = true;

    if (!rawData || rawData.trim().length === 0) {
      validationErrors.push('Raw data cannot be empty');
      isValid = false;
    }

    if (!source || source.trim().length === 0) {
      validationErrors.push('Source cannot be empty');
      isValid = false;
    }

    if (rawData && rawData.length > 1000) {
      validationErrors.push('Raw data too long (max 1000 characters)');
      isValid = false;
    }

    console.log(`✅ Validation ${isValid ? 'passed' : 'failed'}`);
    if (!isValid) {
      console.log(`   Errors: ${validationErrors.join(', ')}`);
    }

    // Type evolution: RawInput -> ValidatedInput
    return ctx.insertAs('isValid', isValid).insertAs('validationErrors', validationErrors);
  }
}

class DataParserLink extends Link {
  async call(ctx: Context<ValidatedInput>): Promise<Context<ParsedInput>> {
    const rawData = ctx.get('rawData');
    const isValid = ctx.get('isValid');

    if (!isValid) {
      throw new Error('Cannot parse invalid data');
    }

    console.log(`📝 Parsing data: ${rawData}`);

    // Parsing logic (simulate JSON parsing)
    let parsedData: any;
    try {
      // Try to parse as JSON first
      parsedData = JSON.parse(rawData);
      console.log('   Parsed as JSON');
    } catch {
      // Fallback to string processing
      parsedData = {
        type: 'string',
        value: rawData,
        length: rawData.length,
        words: rawData.split(/\s+/).length
      };
      console.log('   Parsed as plain text');
    }

    const parseTimestamp = new Date().toISOString();

    console.log(`✅ Parsing completed at ${parseTimestamp}`);

    // Type evolution: ValidatedInput -> ParsedInput
    return ctx.insertAs('parsedData', parsedData).insertAs('parseTimestamp', parseTimestamp);
  }
}

class DataEnricherLink extends Link {
  async call(ctx: Context<ParsedInput>): Promise<Context<EnrichedInput>> {
    const parsedData = ctx.get('parsedData');
    const source = ctx.get('source');

    console.log(`🎨 Enriching data from ${source}`);

    const startTime = Date.now();

    // Enrichment logic
    const enrichmentsApplied: string[] = [];
    let enrichedData = { ...parsedData };

    // Apply various enrichments based on data type
    if (typeof parsedData === 'object' && parsedData !== null) {
      if (parsedData.type === 'string') {
        // String-specific enrichments
        enrichedData.uppercase = parsedData.value.toUpperCase();
        enrichedData.lowercase = parsedData.value.toLowerCase();
        enrichedData.hash = this._simpleHash(parsedData.value);
        enrichmentsApplied.push('case_conversion', 'hash_generation');
      } else if (Array.isArray(parsedData)) {
        // Array-specific enrichments
        enrichedData.length = parsedData.length;
        enrichedData.uniqueItems = Array.from(new Set(parsedData));
        enrichedData.sorted = [...parsedData].sort();
        enrichmentsApplied.push('length_calculation', 'unique_extraction', 'sorting');
      } else {
        // Object-specific enrichments
        enrichedData.keyCount = Object.keys(parsedData).length;
        enrichedData.hasNested = this._hasNestedObjects(parsedData);
        enrichmentsApplied.push('key_counting', 'nesting_detection');
      }
    }

    const processingTime = Date.now() - startTime;
    const confidence = Math.min(0.95, 0.5 + (enrichmentsApplied.length * 0.1));

    const enrichmentMetadata = {
      confidence,
      processingTime,
      enrichmentsApplied
    };

    console.log(`✅ Enrichment completed:`);
    console.log(`   Applied: ${enrichmentsApplied.join(', ')}`);
    console.log(`   Confidence: ${(confidence * 100).toFixed(1)}%`);
    console.log(`   Time: ${processingTime}ms`);

    // Type evolution: ParsedInput -> EnrichedInput
    return ctx.insertAs('enrichedData', enrichedData).insertAs('enrichmentMetadata', enrichmentMetadata);
  }

  private _simpleHash(str: string): number {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash);
  }

  private _hasNestedObjects(obj: any): boolean {
    for (const value of Object.values(obj)) {
      if (typeof value === 'object' && value !== null) {
        return true;
      }
    }
    return false;
  }
}

class ResultProcessorLink extends Link {
  async call(ctx: Context<EnrichedInput>): Promise<Context<ProcessedResult>> {
    const enrichedData = ctx.get('enrichedData');
    const enrichmentMetadata = ctx.get('enrichmentMetadata');

    console.log(`🎯 Processing final result`);

    // Final processing logic
    const processingId = `proc_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    const completedAt = new Date().toISOString();

    const result = {
      data: enrichedData,
      metadata: enrichmentMetadata,
      processingId,
      completedAt,
      status: 'completed'
    };

    console.log(`✅ Final processing completed:`);
    console.log(`   ID: ${processingId}`);
    console.log(`   Status: ${result.status}`);

    // Type evolution: EnrichedInput -> ProcessedResult
    return ctx.insertAs('result', result).insertAs('processingId', processingId).insertAs('completedAt', completedAt);
  }
}

// =============================================================================
// TYPED CHAIN IMPLEMENTATION
// =============================================================================

/**
 * Concrete implementation of the data processing chain
 * Implements the IDataProcessingChain interface using composition
 * with the underlying Chain class for link management and execution.
 */
class DataProcessingChain implements IDataProcessingChain {
  private chain: Chain;

  constructor() {
    this.chain = new Chain();

    // Add typed links with automatic naming
    this.chain.addLink(new InputValidatorLink());
    this.chain.addLink(new DataParserLink());
    this.chain.addLink(new DataEnricherLink());
    this.chain.addLink(new ResultProcessorLink());

    // Connect links in sequence
    this.chain.connect('InputValidatorLink', 'DataParserLink');
    this.chain.connect('DataParserLink', 'DataEnricherLink');
    this.chain.connect('DataEnricherLink', 'ResultProcessorLink');

    // Add middleware
    this.chain.useMiddleware(new LoggingMiddleware());
  }

  async processData(initialCtx: Context<RawInput>): Promise<Context<ProcessedResult>> {
    return await this.chain.run(initialCtx);
  }
}

// =============================================================================
// DEMONSTRATION FUNCTIONS
// =============================================================================

function demonstrateTypeEvolution(): void {
  console.log('=== TYPE EVOLUTION DEMONSTRATION ===\n');

  // Start with RawInput
  const rawInput: RawInput = {
    rawData: '{"name": "Alice", "age": 30, "city": "New York"}',
    source: 'user_input'
  };

  let ctx = new Context<RawInput>(rawInput);
  console.log('1. Initial Context<RawInput>:');
  console.log('   Type: RawInput');
  console.log('   Data keys:', Object.keys(ctx.toObject()));
  console.log();

  // Evolve to ValidatedInput
  ctx = ctx.insertAs('isValid', true).insertAs('validationErrors', []);
  console.log('2. After validation - Context<ValidatedInput>:');
  console.log('   Type: ValidatedInput');
  console.log('   Data keys:', Object.keys(ctx.toObject()));
  console.log();

  // Evolve to ParsedInput
  ctx = ctx.insertAs('parsedData', JSON.parse(rawInput.rawData)).insertAs('parseTimestamp', new Date().toISOString());
  console.log('3. After parsing - Context<ParsedInput>:');
  console.log('   Type: ParsedInput');
  console.log('   Data keys:', Object.keys(ctx.toObject()));
  console.log();

  // Evolve to EnrichedInput
  const enrichmentMetadata = {
    confidence: 0.85,
    processingTime: 150,
    enrichmentsApplied: ['json_parsing', 'validation']
  };
  ctx = ctx.insertAs('enrichedData', ctx.get('parsedData')).insertAs('enrichmentMetadata', enrichmentMetadata);
  console.log('4. After enrichment - Context<EnrichedInput>:');
  console.log('   Type: EnrichedInput');
  console.log('   Data keys:', Object.keys(ctx.toObject()));
  console.log();
}

async function demonstrateTypedChain(): Promise<void> {
  console.log('=== TYPED CHAIN PROCESSING ===\n');

  const chain = new DataProcessingChain();

  // Test data
  const testInputs: RawInput[] = [
    {
      rawData: '{"product": "laptop", "price": 999, "category": "electronics"}',
      source: 'api'
    },
    {
      rawData: 'This is a simple text input for processing',
      source: 'form'
    },
    {
      rawData: '["apple", "banana", "cherry", "apple", "date"]',
      source: 'batch'
    }
  ];

  for (let i = 0; i < testInputs.length; i++) {
    const testCase = testInputs[i];
    console.log(`\n📝 Processing Test Case ${i + 1}:`);
    console.log(`   Source: ${testCase.source}`);
    console.log(`   Data: ${testCase.rawData.substring(0, 50)}${testCase.rawData.length > 50 ? '...' : ''}`);
    console.log('─'.repeat(50));

    try {
      const initialCtx = new Context<RawInput>(testCase);
      const resultCtx = await chain.processData(initialCtx);

      const finalResult = resultCtx.get('result');
      console.log('✅ Processing completed successfully!');
      console.log('📊 Final Result:');
      console.log(`   Processing ID: ${finalResult.processingId}`);
      console.log(`   Status: ${finalResult.status}`);
      console.log(`   Completed: ${finalResult.completedAt}`);
      console.log(`   Enrichments: ${finalResult.metadata.enrichmentsApplied.join(', ')}`);

    } catch (error) {
      console.log('❌ Processing failed:', error.message);
    }
  }
}

// =============================================================================
// MAIN DEMONSTRATION
// =============================================================================

async function main(): Promise<void> {
  console.log('🎯 CodeUChain TypeScript: Type Evolution Layers Example');
  console.log('='.repeat(58));
  console.log();

  console.log('This example demonstrates clean type evolution through processing layers:');
  console.log('• RawInput -> ValidatedInput -> ParsedInput -> EnrichedInput -> ProcessedResult');
  console.log('• Each step adds typed properties without casting');
  console.log('• Full TypeScript generic support');
  console.log('• Type-safe insertAs() method');
  console.log();

  try {
    demonstrateTypeEvolution();
    await demonstrateTypedChain();

    console.log('\n=== SUMMARY ===');
    console.log();
    console.log('✅ Type evolution layers successfully demonstrated!');
    console.log();
    console.log('Key Benefits:');
    console.log('• Clean type progression through processing pipeline');
    console.log('• No explicit casting required');
    console.log('• Full TypeScript IntelliSense support');
    console.log('• Compile-time type safety');
    console.log('• Clear data transformation boundaries');
    console.log();
    console.log('The type evolution pattern provides excellent developer experience');
    console.log('while maintaining runtime flexibility and performance.');

  } catch (error) {
    console.error('❌ Demonstration failed:', error);
    process.exit(1);
  }
}

// Run the demonstration
if (require.main === module) {
  main().catch(console.error);
}

export { main };