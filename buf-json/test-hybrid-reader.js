#!/usr/bin/env node

/**
 * Test script for the Integrated Hybrid Reader
 */

const path = require('path');
const { HybridConfigReader, readHybridConfig } = require('./src/hybrid/hybrid-reader');

async function testHybridReader() {
    console.log('🧪 Testing Integrated Hybrid Reader...\n');

    const dataDir = path.join(__dirname, 'data');

    try {
        // Test 1: Create hybrid reader instance
        console.log('🏗️  Creating hybrid reader...');
        const reader = new HybridConfigReader(__dirname, {
            preferProtobuf: false, // Start with JSON preference
            autoCompile: true,
            cacheCompiled: true,
            preserveFieldNames: false
        });

        console.log('✅ Hybrid reader created successfully\n');

        // Test 2: Read JSON file (should use JSON reader)
        console.log('📄 Reading base.json (JSON mode)...');
        const baseDataJson = await reader.readFile('data/base.json');
        console.log('✅ Read base.json successfully');
        console.log('📄 Keys:', Object.keys(baseDataJson));
        console.log('📄 Title:', baseDataJson.title);
        console.log();

        // Test 3: Read with protobuf preference (should auto-compile)
        console.log('🔄 Switching to protobuf preference...');
        reader.options.preferProtobuf = true;

        console.log('📦 Reading base.json (should auto-compile to protobuf)...');
        const baseDataProtobuf = await reader.readFile('data/base.json');
        console.log('✅ Read base.json with protobuf successfully');
        console.log('📄 Keys:', Object.keys(baseDataProtobuf));
        console.log();

        // Test 4: Read again (should use cached protobuf)
        console.log('⚡ Reading base.json again (should use cache)...');
        const baseDataCached = await reader.readFile('data/base.json');
        console.log('✅ Read from cache successfully');
        console.log();

        // Test 5: Force JSON format
        console.log('📄 Force reading as JSON...');
        const baseDataForcedJson = await reader.readFile('data/base.json', { format: 'json' });
        console.log('✅ Forced JSON read successful');
        console.log();

        // Test 6: Force protobuf format
        console.log('📦 Force reading as protobuf...');
        const baseDataForcedProtobuf = await reader.readFile('data/base.json', { format: 'protobuf' });
        console.log('✅ Forced protobuf read successful');
        console.log();

        // Test 7: Test batch operations
        console.log('📦 Testing batch compilation...');
        const compileResults = await reader.compileDirectoryToProtobuf('data');
        console.log(`✅ Compiled ${compileResults.length} files`);
        console.log();

        // Test 8: Test schema generation
        console.log('📋 Testing schema generation...');
        const schemaFiles = await reader.generateSchemasForDirectory('data');
        console.log(`✅ Generated ${schemaFiles.length} schemas`);
        console.log();

        // Test 9: Performance comparison
        console.log('⚡ Performance comparison test...');
        const startJson = Date.now();
        for (let i = 0; i < 5; i++) {
            await reader.readFile('data/base.json', { format: 'json' });
        }
        const jsonTime = Date.now() - startJson;

        const startProtobuf = Date.now();
        for (let i = 0; i < 5; i++) {
            await reader.readFile('data/base.json', { format: 'protobuf' });
        }
        const protobufTime = Date.now() - startProtobuf;

        console.log(`⏱️  JSON (5 reads): ${jsonTime}ms`);
        console.log(`⏱️  Protobuf (5 reads): ${protobufTime}ms`);
        console.log(`📊 Protobuf is ${jsonTime > protobufTime ? 'faster' : 'slower'} by ${Math.abs(jsonTime - protobufTime)}ms`);
        console.log();

        // Test 10: Get comprehensive statistics
        console.log('📊 Reader Statistics:');
        const stats = reader.getStats();
        console.log(JSON.stringify(stats, null, 2));
        console.log();

        // Test 11: Test convenience function
        console.log('🔧 Testing convenience function...');
        const convenienceData = await readHybridConfig('data/test.json', __dirname, {
            preferProtobuf: true
        });
        console.log('✅ Convenience function works');
        console.log('📄 Test data keys:', Object.keys(convenienceData));
        console.log();

        console.log('🎉 All hybrid reader tests passed!');

    } catch (error) {
        console.error('❌ Hybrid reader test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run the test
testHybridReader();