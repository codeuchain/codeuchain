/**
 * Test the hybrid JSON-protobuf reader setup
 */

const { HybridConfigReader, readHybridConfig } = require('./hybrid-reader');
const path = require('path');

async function testHybridReader() {
    console.log('🧪 Testing Hybrid JSON-Protobuf Reader Setup\n');

    const reader = new HybridConfigReader('./data');

    // Test 1: Read JSON file
    console.log('📄 Test 1: Reading JSON file...');
    try {
        const jsonData = await reader.readJsonFile('test.json');
        console.log('✅ JSON reading works!');
        console.log('Sample data:', JSON.stringify(jsonData, null, 2).substring(0, 200) + '...\n');
    } catch (error) {
        console.log('❌ JSON reading failed:', error.message);
    }

    // Test 2: Check protobuf detection
    console.log('🔍 Test 2: Checking protobuf file detection...');
    const hasProtobuf = await reader.hasProtobufVersion('test.json');
    console.log(`Protobuf version exists: ${hasProtobuf ? '✅ Yes' : '❌ No (expected)'}\n`);

    // Test 3: Test compilation (placeholder)
    console.log('⚙️  Test 3: Testing compilation placeholder...');
    try {
        const outputPath = await reader.compileToProtobuf('test.json');
        console.log(`✅ Compilation placeholder works! Output: ${outputPath}\n`);
    } catch (error) {
        console.log('❌ Compilation failed:', error.message);
    }

    // Test 4: Get stats
    console.log('📊 Test 4: Reader statistics...');
    const stats = reader.getStats();
    console.log('Stats:', stats);
    console.log('');

    console.log('🎉 Hybrid reader setup test completed!');
}

// Run the test
testHybridReader().catch(console.error);