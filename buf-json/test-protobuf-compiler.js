#!/usr/bin/env node

/**
 * Test script for the JSON-to-Protobuf Compiler
 */

const path = require('path');
const { compileJSONToProtobuf, decompileProtobufToJSON, validateProtobufCompilation } = require('./src/hybrid/json-to-protobuf-compiler');

async function testProtobufCompiler() {
    console.log('🧪 Testing JSON-to-Protobuf Compiler...\n');

    const dataDir = path.join(__dirname, 'data');

    try {
        // Test 1: Compile base.json to protobuf
        console.log('📦 Compiling base.json to protobuf...');
        const baseResult = await compileJSONToProtobuf(
            path.join(dataDir, 'base.json'),
            path.join(dataDir, 'protobuf', 'base.proto'),
            path.join(dataDir, 'protobuf', 'base.pb')
        );

        // Test 2: Compile test.json to protobuf
        console.log('📦 Compiling test.json to protobuf...');
        const testResult = await compileJSONToProtobuf(
            path.join(dataDir, 'test.json'),
            path.join(dataDir, 'protobuf', 'test.proto'),
            path.join(dataDir, 'protobuf', 'test.pb')
        );

        // Test 3: Decompile back to JSON
        console.log('📦 Decompiling base.pb back to JSON...');
        const decompiledBase = await decompileProtobufToJSON(
            path.join(dataDir, 'protobuf', 'base.pb'),
            path.join(dataDir, 'protobuf', 'base.proto'),
            path.join(dataDir, 'protobuf', 'base-decompiled.json')
        );

        // Test 4: Validate round-trip compilation
        console.log('🔍 Validating round-trip compilation...');
        const validationResult = await validateProtobufCompilation(
            path.join(dataDir, 'base.json'),
            path.join(dataDir, 'protobuf', 'base.proto')
        );

        if (validationResult.isValid) {
            console.log('✅ Round-trip validation passed!');
        } else {
            console.log('❌ Round-trip validation failed!');
            console.log('Differences found between original and decompiled data');
        }

        // Display compression statistics
        console.log('\n📊 Compilation Statistics:');
        console.log(`   Base file: ${baseResult.metadata ? baseResult.metadata.compressionRatio : 'N/A'} compression ratio`);
        console.log(`   Test file: ${testResult.metadata ? testResult.metadata.compressionRatio : 'N/A'} compression ratio`);

        // List generated files
        const fs = require('fs');
        const generatedFiles = fs.readdirSync(path.join(dataDir, 'protobuf'))
            .filter(file => file.endsWith('.pb') || file.endsWith('.json'))
            .map(file => `   - ${file}`);

        console.log('\n📂 Generated files:');
        generatedFiles.forEach(file => console.log(file));

    } catch (error) {
        console.error('❌ Compilation test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run the test
testProtobufCompiler();