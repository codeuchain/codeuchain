/**
 * Branch + Merge Pipeline Example
 *
 * Demonstrates the Branch + Merge pattern from ASCII_PIPELINES.txt:
 * ```
 *               +-> (Normalize A) -+
 * [Input] -> (Fan)                (Merge) -> (Aggregate) -> [Output]
 *               +-> (Normalize B) -+
 * ```
 *
 * This example shows how to process data through parallel branches
 * and merge the results back together.
 */

const { State, Chain, Link, LoggingHook } = require('../core');

class DataFanOutLink extends Link {
  async call(ctx) {
    const data = ctx.get('inputData');
    console.log(`🔀 Fan-out: Splitting ${data} into parallel branches`);

    // Create branch states
    const branchA = ctx.insert('branch', 'A').insert('data', data.toUpperCase());
    const branchB = ctx.insert('branch', 'B').insert('data', data.toLowerCase());

    return ctx
      .insert('branchA', branchA)
      .insert('branchB', branchB)
      .insert('fanOutComplete', true);
  }
}

class NormalizeBranchALink extends Link {
  async call(ctx) {
    const branchData = ctx.get('branchA');
    const data = branchData.get('data');

    console.log(`🔧 Branch A: Normalizing "${data}"`);

    // Normalize by removing vowels
    const normalized = data.replace(/[AEIOU]/gi, '');

    return ctx.insert('normalizedA', normalized);
  }
}

class NormalizeBranchBLink extends Link {
  async call(ctx) {
    const branchData = ctx.get('branchB');
    const data = branchData.get('data');

    console.log(`🔧 Branch B: Normalizing "${data}"`);

    // Normalize by reversing string
    const normalized = data.split('').reverse().join('');

    return ctx.insert('normalizedB', normalized);
  }
}

class MergeResultsLink extends Link {
  async call(ctx) {
    const normalizedA = ctx.get('normalizedA');
    const normalizedB = ctx.get('normalizedB');

    console.log(`🔗 Merging results: A="${normalizedA}", B="${normalizedB}"`);

    const merged = `${normalizedA}|${normalizedB}`;

    return ctx.insert('mergedResult', merged);
  }
}

class AggregateResultsLink extends Link {
  async call(ctx) {
    const merged = ctx.get('mergedResult');
    const original = ctx.get('inputData');

    console.log(`📊 Aggregating: Original="${original}", Merged="${merged}"`);

    const result = {
      original,
      merged,
      length: merged.length,
      branches: 2,
      timestamp: new Date().toISOString()
    };

    return ctx.insert('finalResult', result);
  }
}

async function main() {
  console.log('🌟 CodeUChain: Branch + Merge Pipeline Example');
  console.log('=' * 55);
  console.log();

  // Create the branch and merge chain
  const chain = new Chain();

  // Add all links
  chain.addLink(new DataFanOutLink());
  chain.addLink(new NormalizeBranchALink());
  chain.addLink(new NormalizeBranchBLink());
  chain.addLink(new MergeResultsLink());
  chain.addLink(new AggregateResultsLink());

  // Connect in branch + merge pattern
  chain.connect('DataFanOutLink', 'NormalizeBranchALink');
  chain.connect('DataFanOutLink', 'NormalizeBranchBLink');
  chain.connect('NormalizeBranchALink', 'MergeResultsLink');
  chain.connect('NormalizeBranchBLink', 'MergeResultsLink');
  chain.connect('MergeResultsLink', 'AggregateResultsLink');

  // Add hook
  chain.useHook(new LoggingHook());

  // Test data
  const testInputs = [
    'Hello World',
    'JavaScript',
    'CodeUChain',
    'Pipeline Processing'
  ];

  console.log('🧪 Testing Branch + Merge Pipeline:\n');

  for (const input of testInputs) {
    console.log(`📝 Processing: "${input}"`);
    console.log('─'.repeat(40));

    try {
      const initialCtx = new State({ inputData: input });
      const resultCtx = await chain.run(initialCtx);

      const finalResult = resultCtx.get('finalResult');
      console.log('✅ Pipeline completed successfully!');
      console.log('📊 Final Result:', JSON.stringify(finalResult, null, 2));

    } catch (error) {
      console.log('❌ Pipeline failed:', error.message);
    }

    console.log('='.repeat(60));
    console.log();
  }

  console.log('✨ Branch + Merge Pipeline Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Fan-out: Splitting work into parallel branches');
  console.log('• Parallel processing: Independent branch execution');
  console.log('• Merge: Combining results from multiple branches');
  console.log('• Aggregation: Final processing of merged results');
  console.log('• Complex pipeline topologies beyond linear chains');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };