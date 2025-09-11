/**
 * Parallel Fan-Out & Join Example
 *
 * Demonstrates the Parallel Fan-Out & Join pattern from ASCII_PIPELINES.txt:
 * ```
 *           +-> (Link A) --+
 * [Ctx] -> ( Split  )      ( Join ) -> [Ctx']
 *           +-> (Link B) --+
 * ```
 *
 * This example shows how to split work into parallel branches
 * and synchronize them back together.
 */

const { Context, Chain, Link, LoggingMiddleware } = require('../core');

class DataSplitterLink extends Link {
  async call(ctx) {
    const items = ctx.get('items');
    console.log(`🔀 Splitting ${items.length} items into parallel processing`);

    // Split items into two branches
    const midPoint = Math.ceil(items.length / 2);
    const branchAItems = items.slice(0, midPoint);
    const branchBItems = items.slice(midPoint);

    console.log(`📦 Branch A: ${branchAItems.length} items`);
    console.log(`📦 Branch B: ${branchBItems.length} items`);

    return ctx
      .insert('branchAItems', branchAItems)
      .insert('branchBItems', branchBItems)
      .insert('splitComplete', true);
  }
}

class ProcessBranchALink extends Link {
  async call(ctx) {
    const items = ctx.get('branchAItems');
    console.log(`⚙️ Processing Branch A: ${items.length} items`);

    // Simulate parallel processing of items
    const results = await Promise.all(
      items.map(async (item, index) => {
        // Simulate async processing with random delay
        const delay = Math.random() * 500 + 100;
        await new Promise(resolve => setTimeout(resolve, delay));

        return {
          id: item.id,
          original: item.value,
          processed: item.value.toUpperCase(),
          branch: 'A',
          processingTime: delay
        };
      })
    );

    console.log(`✅ Branch A completed: ${results.length} items processed`);
    return ctx.insert('branchAResults', results);
  }
}

class ProcessBranchBLink extends Link {
  async call(ctx) {
    const items = ctx.get('branchBItems');
    console.log(`⚙️ Processing Branch B: ${items.length} items`);

    // Simulate parallel processing of items
    const results = await Promise.all(
      items.map(async (item, index) => {
        // Simulate async processing with random delay
        const delay = Math.random() * 500 + 100;
        await new Promise(resolve => setTimeout(resolve, delay));

        return {
          id: item.id,
          original: item.value,
          processed: item.value.split('').reverse().join(''),
          branch: 'B',
          processingTime: delay
        };
      })
    );

    console.log(`✅ Branch B completed: ${results.length} items processed`);
    return ctx.insert('branchBResults', results);
  }
}

class ResultsJoinerLink extends Link {
  async call(ctx) {
    const branchAResults = ctx.get('branchAResults');
    const branchBResults = ctx.get('branchBResults');

    console.log(`🔗 Joining results: A=${branchAResults.length}, B=${branchBResults.length}`);

    // Combine and sort results by original ID
    const combinedResults = [...branchAResults, ...branchBResults]
      .sort((a, b) => a.id - b.id);

    // Calculate processing statistics
    const totalItems = combinedResults.length;
    const avgProcessingTime = combinedResults.reduce((sum, item) => sum + item.processingTime, 0) / totalItems;
    const maxProcessingTime = Math.max(...combinedResults.map(item => item.processingTime));

    const summary = {
      totalItems,
      branchACount: branchAResults.length,
      branchBCount: branchBResults.length,
      avgProcessingTime: Math.round(avgProcessingTime),
      maxProcessingTime: Math.round(maxProcessingTime),
      timestamp: new Date().toISOString()
    };

    console.log(`📊 Join complete: ${totalItems} items, avg time: ${summary.avgProcessingTime}ms`);

    return ctx
      .insert('combinedResults', combinedResults)
      .insert('processingSummary', summary);
  }
}

class FinalAggregatorLink extends Link {
  async call(ctx) {
    const results = ctx.get('combinedResults');
    const summary = ctx.get('processingSummary');

    console.log(`🎯 Aggregation complete:`);
    console.log(`   Total processed: ${summary.totalItems}`);
    console.log(`   Branch A: ${summary.branchACount}, Branch B: ${summary.branchBCount}`);
    console.log(`   Performance: ${summary.avgProcessingTime}ms avg, ${summary.maxProcessingTime}ms max`);

    // Create final aggregated result
    const finalResult = {
      summary,
      results,
      status: 'completed',
      completedAt: new Date().toISOString()
    };

    return ctx.insert('finalResult', finalResult);
  }
}

async function main() {
  console.log('🔄 CodeUChain: Parallel Fan-Out & Join Example');
  console.log('=' * 52);
  console.log();

  // Create the parallel processing chain
  const chain = new Chain();

  // Add all links
  chain.addLink(new DataSplitterLink());
  chain.addLink(new ProcessBranchALink());
  chain.addLink(new ProcessBranchBLink());
  chain.addLink(new ResultsJoinerLink());
  chain.addLink(new FinalAggregatorLink());

  // Connect in parallel pattern
  chain.connect('DataSplitterLink', 'ProcessBranchALink');
  chain.connect('DataSplitterLink', 'ProcessBranchBLink');
  chain.connect('ProcessBranchALink', 'ResultsJoinerLink');
  chain.connect('ProcessBranchBLink', 'ResultsJoinerLink');
  chain.connect('ResultsJoinerLink', 'FinalAggregatorLink');

  // Add middleware
  chain.useMiddleware(new LoggingMiddleware());

  // Test data
  const testData = [
    { items: [
      { id: 1, value: 'alpha' },
      { id: 2, value: 'beta' },
      { id: 3, value: 'gamma' },
      { id: 4, value: 'delta' },
      { id: 5, value: 'epsilon' },
      { id: 6, value: 'zeta' }
    ]},
    { items: [
      { id: 1, value: 'hello' },
      { id: 2, value: 'world' },
      { id: 3, value: 'codeuchain' },
      { id: 4, value: 'pipeline' }
    ]},
    { items: [
      { id: 1, value: 'single' }
    ]}
  ];

  console.log('🧪 Testing Parallel Fan-Out & Join:\n');

  for (let i = 0; i < testData.length; i++) {
    const testCase = testData[i];
    console.log(`📝 Test Case ${i + 1}: ${testCase.items.length} items`);
    console.log('─'.repeat(45));

    try {
      const startTime = Date.now();
      const initialCtx = new Context(testCase);
      const resultCtx = await chain.run(initialCtx);
      const endTime = Date.now();

      const finalResult = resultCtx.get('finalResult');
      console.log('✅ Parallel processing completed!');
      console.log(`⏱️ Total time: ${endTime - startTime}ms`);
      console.log('📊 Summary:', JSON.stringify(finalResult.summary, null, 2));

      // Show sample results
      console.log('📋 Sample Results:');
      finalResult.results.slice(0, 3).forEach(result => {
        console.log(`   ${result.id}: "${result.original}" -> "${result.processed}" (${result.branch})`);
      });

    } catch (error) {
      console.log('❌ Parallel processing failed:', error.message);
    }

    console.log('='.repeat(70));
    console.log();
  }

  console.log('✨ Parallel Fan-Out & Join Example Complete!');
  console.log();
  console.log('Key Concepts Demonstrated:');
  console.log('• Work splitting into parallel branches');
  console.log('• Concurrent processing of independent tasks');
  console.log('• Synchronization and result joining');
  console.log('• Performance optimization through parallelism');
  console.log('• Load balancing across processing branches');
}

// Run the example
if (require.main === module) {
  main().catch(console.error);
}

module.exports = { main };