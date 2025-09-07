#!/usr/bin/env node

/**
 * Test script for the Protobuf Runtime Loader
 */

const path = require('path');
const { ProtobufRuntimeLoader, loadProtobufFile, validateProtobufFile } = require('./src/hybrid/protobuf-runtime-loader');

async function testProtobufRuntimeLoader() {
    console.log('🧪 Testing Protobuf Runtime Loader...\n');

    const dataDir = path.join(__dirname, 'data');

    try {
        // Test 1: Load protobuf file
        console.log('📦 Loading base.pb...');
        const baseData = await loadProtobufFile(
            path.join(dataDir, 'protobuf', 'base.pb'),
            path.join(dataDir, 'protobuf', 'base.proto')
        );

        console.log('✅ Loaded base.pb successfully');
        console.log('📄 Data keys:', Object.keys(baseData));
        console.log('📄 Sample data:', JSON.stringify(baseData, null, 2).substring(0, 200) + '...');
        console.log();

        // Test 2: Load with field name preservation
        console.log('📦 Loading base.pb with original field names...');
        const loader = new ProtobufRuntimeLoader({ baseDir: dataDir });
        const baseDataOriginal = await loader.loadFile(
            path.join(dataDir, 'protobuf', 'base.pb'),
            path.join(dataDir, 'protobuf', 'base.proto'),
            { preserveFieldNames: true }
        );

        console.log('✅ Loaded with field preservation');
        console.log('📄 Original field names:', Object.keys(baseDataOriginal));
        console.log();

        // Test 3: Load test.pb
        console.log('📦 Loading test.pb...');
        const testData = await loadProtobufFile(
            path.join(dataDir, 'protobuf', 'test.pb'),
            path.join(dataDir, 'protobuf', 'test.proto')
        );

        console.log('✅ Loaded test.pb successfully');
        console.log('📄 Test data:', JSON.stringify(testData, null, 2));
        console.log();

        // Test 4: Validate files
        console.log('🔍 Validating protobuf files...');
        const baseValidation = await validateProtobufFile(
            path.join(dataDir, 'protobuf', 'base.pb'),
            path.join(dataDir, 'protobuf', 'base.proto')
        );

        const testValidation = await validateProtobufFile(
            path.join(dataDir, 'protobuf', 'test.pb'),
            path.join(dataDir, 'protobuf', 'test.proto')
        );

        console.log(`✅ Base validation: ${baseValidation.valid ? 'PASSED' : 'FAILED'}`);
        console.log(`✅ Test validation: ${testValidation.valid ? 'PASSED' : 'FAILED'}`);
        console.log();

        // Test 5: Test caching
        console.log('⚡ Testing caching performance...');
        const startTime = Date.now();

        // Load the same file multiple times
        for (let i = 0; i < 10; i++) {
            await loader.loadFile(
                path.join(dataDir, 'protobuf', 'base.pb'),
                path.join(dataDir, 'protobuf', 'base.proto')
            );
        }

        const endTime = Date.now();
        console.log(`✅ Loaded 10 times in ${endTime - startTime}ms`);
        console.log();

        // Test 6: Get loader statistics
        console.log('📊 Loader Statistics:');
        const stats = loader.getStats();
        console.log(JSON.stringify(stats, null, 2));
        console.log();

        // Test 7: Test fallback to JSON
        console.log('🔄 Testing JSON fallback...');
        const fallbackLoader = new ProtobufRuntimeLoader({
            baseDir: dataDir,
            fallbackToJson: true
        });

        // Try to load a non-existent protobuf file
        try {
            const fallbackData = await fallbackLoader.loadFile(
                path.join(dataDir, 'protobuf', 'nonexistent.pb')
            );
            console.log('✅ Fallback successful');
        } catch (error) {
            console.log('⚠️  Fallback failed as expected:', error.message);
        }

    } catch (error) {
        console.error('❌ Runtime loader test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run the test
testProtobufRuntimeLoader();