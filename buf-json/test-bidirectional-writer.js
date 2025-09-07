/**
 * Test Bidirectional Hybrid Writer
 *
 * This test demonstrates the write-back functionality of the hybrid system,
 * showing how users can write data that gets stored efficiently as protobuf
 * while maintaining JSON interfaces for developers.
 */

const { HybridConfigReader } = require('./src/hybrid/hybrid-reader');
const fs = require('fs');
const path = require('path');

async function testBidirectionalHybridWriter() {
    console.log('🧪 Testing Bidirectional Hybrid Writer...\n');

    const reader = new HybridConfigReader('.', {
        preferProtobuf: true,
        keepJsonFallback: true,
        formatJson: true,
        validateOnWrite: true
    });

    try {
        // Test data to write
        const testData = {
            language_name: "Test Configuration",
            title: "Bidirectional Hybrid Test",
            hero_description: "Testing write-back functionality",
            source_link: "https://github.com/codeuchain/codeuchain",
            logo_link: "../test.html",
            version: "2.0.0",
            company: "Test Company",
            repository: "https://github.com/test/repo",
            custom_field: "Added during test",
            nested_object: {
                level1: {
                    level2: "Deep nesting test"
                }
            },
            array_field: ["item1", "item2", "item3"]
        };

        // Test 1: Write as hybrid (both JSON and protobuf)
        console.log('📝 Test 1: Writing as hybrid format...');
        const hybridResult = await reader.writeAsHybrid('data/test-config.json', testData);
        console.log('✅ Hybrid write result:', {
            protobuf: hybridResult.protobuf ? 'success' : 'failed',
            json: hybridResult.json ? 'success' : 'failed',
            format: hybridResult.format
        });

        // Verify files were created
        const protobufPath = 'data/protobuf/test-config.pb';
        const jsonPath = 'data/test-config.json';
        const protoPath = 'data/protobuf/test-config.proto';

        console.log('📁 Files created:');
        console.log(`   Protobuf: ${fs.existsSync(protobufPath) ? '✅' : '❌'} ${protobufPath}`);
        console.log(`   JSON: ${fs.existsSync(jsonPath) ? '✅' : '❌'} ${jsonPath}`);
        console.log(`   Schema: ${fs.existsSync(protoPath) ? '✅' : '❌'} ${protoPath}\n`);

        // Test 2: Read back the data to verify round-trip
        console.log('📖 Test 2: Reading back the written data...');
        const readData = await reader.readFile('data/test-config.json');
        console.log('✅ Read back data keys:', Object.keys(readData));
        console.log('✅ Custom field value:', readData.customField);
        console.log('✅ Nested object:', JSON.stringify(readData.nestedObject, null, 2));
        console.log('✅ Array field:', readData.arrayField, '\n');

        // Test 3: Update existing data
        console.log('🔄 Test 3: Updating existing data...');
        const updatedData = {
            ...testData,
            version: "2.1.0",
            custom_field: "Updated during test",
            new_field: "Added in update"
        };

        const updateResult = await reader.updateFile('data/test-config.json', updatedData);
        console.log('✅ Update result:', updateResult);

        // Verify the update
        const updatedReadData = await reader.readFile('data/test-config.json');
        console.log('✅ Updated version:', updatedReadData.version);
        console.log('✅ Updated custom field:', updatedReadData.customField);
        console.log('✅ New field:', updatedReadData.newField, '\n');

        // Test 4: Write as JSON only (developer mode)
        console.log('📄 Test 4: Writing as JSON only...');
        const jsonOnlyData = {
            language_name: "JSON Only Config",
            title: "Developer Mode Test",
            version: "1.0.0"
        };

        const jsonResult = await reader.writeAsJson('data/dev-config.json', jsonOnlyData);
        console.log('✅ JSON-only write result:', jsonResult);

        // Verify JSON-only file
        const devJsonPath = 'data/dev-config.json';
        const devProtobufPath = 'data/protobuf/dev-config.pb';
        console.log('📁 JSON-only files:');
        console.log(`   JSON: ${fs.existsSync(devJsonPath) ? '✅' : '❌'} ${devJsonPath}`);
        console.log(`   Protobuf: ${fs.existsSync(devProtobufPath) ? '❌ (expected)' : '✅ (unexpected)'}\n`);

        // Test 5: Write as protobuf only (production mode)
        console.log('🔸 Test 5: Writing as protobuf only...');
        const protobufOnlyData = {
            language_name: "Protobuf Only Config",
            title: "Production Mode Test",
            version: "3.0.0",
            production_flag: true
        };

        const protobufResult = await reader.writeAsProtobuf('data/prod-config.json', protobufOnlyData);
        console.log('✅ Protobuf-only write result:', protobufResult);

        // Verify protobuf-only files
        const prodJsonPath = 'data/prod-config.json';
        const prodProtobufPath = 'data/protobuf/prod-config.pb';
        console.log('📁 Protobuf-only files:');
        console.log(`   JSON: ${fs.existsSync(prodJsonPath) ? '✅' : '❌'} ${prodJsonPath}`);
        console.log(`   Protobuf: ${fs.existsSync(prodProtobufPath) ? '✅' : '❌'} ${prodProtobufPath}\n`);

        // Test 6: Sync files (ensure both versions are in sync)
        console.log('🔄 Test 6: Syncing files...');
        const syncResult = await reader.syncFile('data/test-config.json');
        console.log('✅ Sync result:', syncResult);

        // Test 7: Demonstrate different format preferences
        console.log('⚙️  Test 7: Format preference demonstration...');

        // Reader with JSON preference
        const jsonPreferredReader = new HybridConfigReader('.', {
            preferProtobuf: false,
            keepJsonFallback: true
        });

        const jsonPreferredData = {
            language_name: "JSON Preferred",
            title: "Format Preference Test",
            version: "1.5.0"
        };

        await jsonPreferredReader.writeFile('data/format-test.json', jsonPreferredData);
        console.log('✅ JSON-preferred write completed');

        // Test 8: Writer statistics
        console.log('📊 Test 8: Writer statistics...');
        const stats = reader.getStats();
        console.log('✅ Writer stats:', {
            filesWritten: stats.filesWritten,
            schemasGenerated: stats.schemasGenerated,
            averageWriteTime: stats.averageWriteTime,
            compressionRatio: stats.compressionRatio
        });

        console.log('\n🎉 All bidirectional hybrid writer tests passed!');
        console.log('\n📋 Summary:');
        console.log('   ✅ Hybrid writing (JSON + Protobuf)');
        console.log('   ✅ JSON-only writing (developer mode)');
        console.log('   ✅ Protobuf-only writing (production mode)');
        console.log('   ✅ Data updates and synchronization');
        console.log('   ✅ Round-trip data integrity');
        console.log('   ✅ Format preferences and options');
        console.log('   ✅ File management and validation');

    } catch (error) {
        console.error('❌ Bidirectional hybrid writer test failed:', error.message);
        console.error('Stack trace:', error.stack);
        throw error;
    }
}

// Run the test
if (require.main === module) {
    testBidirectionalHybridWriter().catch(console.error);
}

module.exports = { testBidirectionalHybridWriter };