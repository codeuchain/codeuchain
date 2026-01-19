/**
 * CodeUChain Converter Example
 *
 * Demonstrates converting between CodeUChain chains and traditional imperative code.
 * Shows the zero-overhead compilation feature.
 */

const { Context, Chain, Link, ChainConverter } = require('../core');

// Define example links
class ValidationLink extends Link {
  /**
   * Validates input data.
   */
  async call(ctx) {
    const value = ctx.get('value');

    if (value === undefined || value === null) {
      return ctx.insert('valid', false).insert('error', 'Value is required');
    }

    if (typeof value !== 'number') {
      return ctx.insert('valid', false).insert('error', 'Value must be a number');
    }

    if (value < 0) {
      return ctx.insert('valid', false).insert('error', 'Value must be positive');
    }

    return ctx.insert('valid', true);
  }
}

class ProcessingLink extends Link {
  /**
   * Processes validated data.
   */
  async call(ctx) {
    if (!ctx.get('valid')) {
      return ctx; // Skip processing if invalid
    }

    const value = ctx.get('value');
    const squared = value ** 2;

    return ctx.insert('processed', squared).insert('original', value);
  }
}

class FormattingLink extends Link {
  /**
   * Formats the result.
   */
  async call(ctx) {
    if (!ctx.get('valid')) {
      const error = ctx.get('error');
      return ctx.insert('message', `Error: ${error}`);
    }

    const original = ctx.get('original');
    const processed = ctx.get('processed');

    const message = `The square of ${original} is ${processed}`;
    return ctx.insert('message', message);
  }
}

async function main() {
  console.log('='.repeat(70));
  console.log('CodeUChain Converter Example: Zero-Overhead Compilation');
  console.log('='.repeat(70));
  console.log();

  // ===== Step 1: Create a CodeUChain =====
  console.log('Step 1: Creating a CodeUChain...');
  console.log('-'.repeat(70));

  const chain = new Chain();
  chain.addLink(new ValidationLink(), 'validate');
  chain.addLink(new ProcessingLink(), 'process');
  chain.addLink(new FormattingLink(), 'format');

  // Connect links in sequence
  chain.connect('validate', 'process');
  chain.connect('process', 'format');

  console.log('✓ Chain created with 3 links: validate → process → format');
  console.log();

  // ===== Step 2: Test the original chain =====
  console.log('Step 2: Testing the original chain...');
  console.log('-'.repeat(70));

  const testCases = [
    { value: 5 },
    { value: 10 },
    { value: -3 },
    { value: 'invalid' },
  ];

  console.log('\nOriginal Chain Results:');
  for (let i = 0; i < testCases.length; i++) {
    const testData = testCases[i];
    const result = await chain.run(new Context(testData));
    console.log(`  Test ${i + 1}: ${JSON.stringify(testData)} → ${result.get('message')}`);
  }
  console.log();

  // ===== Step 3: Convert chain to traditional code =====
  console.log('Step 3: Converting chain to zero-overhead traditional code...');
  console.log('-'.repeat(70));

  const converter = new ChainConverter();
  const traditionalCode = converter.chainToCode(chain, 'executePipeline');

  console.log('\nGenerated Code Preview (first 50 lines):');
  const lines = traditionalCode.split('\n');
  for (let i = 0; i < Math.min(50, lines.length); i++) {
    console.log(`  ${lines[i]}`);
  }

  if (lines.length > 50) {
    console.log(`  ... (${lines.length - 50} more lines)`);
  }
  console.log();

  // ===== Step 4: Execute the generated code =====
  console.log('Step 4: Executing the generated traditional code...');
  console.log('-'.repeat(70));

  // Create a wrapper to evaluate the code
  const codeWrapper = `
${traditionalCode}

return executePipeline;
  `;

  const executePipeline = new Function(
    'Context',
    'ValidationLink',
    'ProcessingLink',
    'FormattingLink',
    codeWrapper
  )(Context, ValidationLink, ProcessingLink, FormattingLink);

  console.log('\nTraditional Code Results:');
  for (let i = 0; i < testCases.length; i++) {
    const testData = testCases[i];
    const result = await executePipeline(testData);
    console.log(`  Test ${i + 1}: ${JSON.stringify(testData)} → ${result.message}`);
  }
  console.log();

  // ===== Step 5: Validate that both produce identical results =====
  console.log('Step 5: Validating that both versions produce identical results...');
  console.log('-'.repeat(70));

  // Manual validation instead of using converter.validate due to circular dependency
  let allValid = true;
  const validationErrors = [];
  
  for (let i = 0; i < testCases.length; i++) {
    const testData = testCases[i];
    try {
      const chainResult = await chain.run(new Context(testData));
      const chainData = chainResult.toObject();
      const funcResult = await executePipeline(testData);
      
      if (JSON.stringify(chainData) !== JSON.stringify(funcResult)) {
        allValid = false;
        validationErrors.push(`Test ${i + 1}: Results differ`);
      }
    } catch (error) {
      allValid = false;
      validationErrors.push(`Test ${i + 1}: ${error.message}`);
    }
  }

  if (allValid) {
    console.log('✓ VALIDATION PASSED: Both versions produce identical results!');
    console.log('  The traditional code has ZERO overhead compared to the chain.');
  } else {
    console.log('✗ VALIDATION FAILED: Results differ!');
    for (const error of validationErrors) {
      console.log(`  ${error}`);
    }
  }
  console.log();

  // ===== Step 6: Generate optimized class =====
  console.log('Step 6: Generating optimized class for production use...');
  console.log('-'.repeat(70));

  const classCode = converter.generateOptimizedClass(chain, 'OptimizedPipeline');

  console.log('\nGenerated Class Preview (first 40 lines):');
  const classLines = classCode.split('\n');
  for (let i = 0; i < Math.min(40, classLines.length); i++) {
    console.log(`  ${classLines[i]}`);
  }

  if (classLines.length > 40) {
    console.log(`  ... (${classLines.length - 40} more lines)`);
  }
  console.log();

  // Execute and test the class
  const classWrapper = `
${classCode}

return OptimizedPipeline;
  `;

  const OptimizedPipeline = new Function(
    'Context',
    'ValidationLink',
    'ProcessingLink',
    'FormattingLink',
    classWrapper
  )(Context, ValidationLink, ProcessingLink, FormattingLink);

  const pipeline = new OptimizedPipeline();

  console.log('Testing the optimized class:');
  for (let i = 0; i < 2; i++) {
    // Test first 2 cases
    const testData = testCases[i];
    const result = await pipeline.execute(testData);
    console.log(`  Test ${i + 1}: ${JSON.stringify(testData)} → ${result.message}`);
  }
  console.log();

  // ===== Summary =====
  console.log('='.repeat(70));
  console.log('Summary: Benefits of Zero-Overhead Conversion');
  console.log('='.repeat(70));
  console.log();
  console.log('1. Development: Use CodeUChain for modularity and maintainability');
  console.log('2. Production: Convert to traditional code for maximum performance');
  console.log('3. Validation: Ensure both versions produce identical results');
  console.log('4. Flexibility: Convert back and forth as needed');
  console.log();
  console.log('Performance Comparison:');
  console.log('  • Chain execution: includes orchestration overhead');
  console.log('  • Traditional code: direct function calls (zero overhead)');
  console.log('  • Optimized class: reusable instance (minimal overhead)');
  console.log();
  console.log('Use Cases:');
  console.log('  • API endpoints: Use optimized class for consistent performance');
  console.log('  • Batch processing: Use traditional code for maximum throughput');
  console.log('  • Development: Use chain for easy testing and modification');
  console.log();
  console.log('='.repeat(70));
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };
