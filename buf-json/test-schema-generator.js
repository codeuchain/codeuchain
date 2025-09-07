#!/usr/bin/env node

/**
 * Test script for the Protobuf Schema Generator
 */

const path = require('path');
const { generateProtobufSchema, generateProtobufSchemasFromDirectory } = require('./src/hybrid/schema-generator');

async function testSchemaGenerator() {
    console.log('🧪 Testing Protobuf Schema Generator...\n');

    const dataDir = path.join(__dirname, 'data');

    try {
        // Test 1: Generate schema from base.json
        console.log('📄 Generating schema from base.json...');
        const baseSchemaPath = await generateProtobufSchema(
            path.join(dataDir, 'base.json'),
            path.join(dataDir, 'protobuf', 'base.proto')
        );

        // Test 2: Generate schema from test.json
        console.log('📄 Generating schema from test.json...');
        const testSchemaPath = await generateProtobufSchema(
            path.join(dataDir, 'test.json'),
            path.join(dataDir, 'protobuf', 'test.proto')
        );

        // Test 3: Generate schemas for all JSON files in directory
        console.log('📁 Generating schemas for all JSON files...');
        const generatedFiles = await generateProtobufSchemasFromDirectory(dataDir);

        console.log('\n✅ Schema generation completed successfully!');
        console.log(`📂 Generated files: ${generatedFiles.length}`);
        generatedFiles.forEach(file => console.log(`   - ${path.basename(file)}`));

        // Display one of the generated schemas
        console.log('\n📋 Sample generated schema (base.proto):');
        console.log('=' .repeat(50));
        const fs = require('fs');
        const sampleSchema = fs.readFileSync(baseSchemaPath, 'utf8');
        console.log(sampleSchema);

    } catch (error) {
        console.error('❌ Schema generation failed:', error.message);
        process.exit(1);
    }
}

// Run the test
testSchemaGenerator();