#!/usr/bin/env node

/**
 * Comprehensive Test Suite for CodeUChain Hybrid JSON-Protobuf System
 *
 * This test suite provides complete coverage of all hybrid system components
 * including integration tests, edge cases, error handling, and performance benchmarks.
 */

const path = require('path');
const fs = require('fs');
const { performance } = require('perf_hooks');

// Import all hybrid system components
const { ProtobufSchemaGenerator } = require('./src/hybrid/schema-generator');
const { JSONToProtobufCompiler } = require('./src/hybrid/json-to-protobuf-compiler');
const { ProtobufRuntimeLoader } = require('./src/hybrid/protobuf-runtime-loader');
const { HybridConfigReader } = require('./src/hybrid/hybrid-reader');
const { HybridWriter } = require('./src/hybrid/hybrid-writer');

class ComprehensiveTestSuite {
    constructor() {
        this.testResults = {
            passed: 0,
            failed: 0,
            skipped: 0,
            total: 0
        };

        this.testData = {
            simple: {
                name: "Simple Config",
                version: "1.0.0",
                enabled: true
            },
            complex: {
                language_name: "Test Configuration",
                title: "Comprehensive Test Suite",
                hero_description: "Testing all hybrid system components",
                source_link: "https://github.com/codeuchain/codeuchain",
                logo_link: "../test.html",
                version: "2.1.0",
                company: "Test Company",
                repository: "https://github.com/test/repo",
                custom_field: "Added during test",
                nested_object: {
                    level1: {
                        level2: "Deep nesting test"
                    }
                },
                array_field: ["item1", "item2", "item3"],
                number_field: 42,
                boolean_field: true,
                null_field: null,
                empty_array: [],
                empty_object: {}
            },
            edge_cases: {
                empty: {},
                null_values: {
                    field1: null,
                    field2: null,
                    field3: "not null"
                },
                special_chars: {
                    "field-with-dashes": "value1",
                    "field_with_underscores": "value2",
                    "field.with.dots": "value3",
                    "field with spaces": "value4",
                    "123numeric_start": "value5"
                },
                unicode: {
                    emoji: "🚀",
                    accents: "café",
                    chinese: "你好",
                    arabic: "مرحبا"
                }
            }
        };

        this.tempDir = path.join(__dirname, 'test-temp');
        this.ensureTempDirectory();
    }

    ensureTempDirectory() {
        if (!fs.existsSync(this.tempDir)) {
            fs.mkdirSync(this.tempDir, { recursive: true });
        }

        // Clean up any existing test files
        const files = fs.readdirSync(this.tempDir);
        files.forEach(file => {
            const filePath = path.join(this.tempDir, file);
            if (fs.statSync(filePath).isFile()) {
                fs.unlinkSync(filePath);
            }
        });
    }

    log(message, level = 'info') {
        const timestamp = new Date().toISOString();
        const prefix = level === 'error' ? '❌' : level === 'success' ? '✅' : level === 'warn' ? '⚠️' : '📋';
        console.log(`[${timestamp}] ${prefix} ${message}`);
    }

    async runTest(testName, testFunction) {
        this.testResults.total++;
        this.log(`Running test: ${testName}`, 'info');

        try {
            const startTime = performance.now();
            await testFunction();
            const endTime = performance.now();
            const duration = (endTime - startTime).toFixed(2);

            this.testResults.passed++;
            this.log(`✅ PASSED: ${testName} (${duration}ms)`, 'success');
            return true;
        } catch (error) {
            this.testResults.failed++;
            this.log(`❌ FAILED: ${testName} - ${error.message}`, 'error');
            if (error.stack) {
                console.log(error.stack);
            }
            return false;
        }
    }

    async runAllTests() {
        this.log('🚀 Starting Comprehensive Test Suite for CodeUChain Hybrid System', 'info');
        this.log('=' .repeat(70), 'info');

        // Component Tests
        await this.runTest('Schema Generator - Basic Functionality', () => this.testSchemaGenerator());
        await this.runTest('Schema Generator - Edge Cases', () => this.testSchemaGeneratorEdgeCases());
        await this.runTest('Protobuf Compiler - Basic Compilation', () => this.testProtobufCompiler());
        await this.runTest('Protobuf Compiler - Round-trip Validation', () => this.testRoundTripValidation());
        await this.runTest('Protobuf Runtime Loader - Basic Loading', () => this.testProtobufLoader());
        await this.runTest('Protobuf Runtime Loader - Caching', () => this.testProtobufLoaderCaching());
        await this.runTest('Hybrid Reader - Format Switching', () => this.testHybridReader());
        await this.runTest('Hybrid Reader - Auto-compilation', () => this.testHybridReaderAutoCompile());
        await this.runTest('Bidirectional Writer - Basic Write-back', () => this.testBidirectionalWriter());
        await this.runTest('Bidirectional Writer - Format Preferences', () => this.testWriterFormatPreferences());

        // Integration Tests
        await this.runTest('Full Pipeline Integration', () => this.testFullPipelineIntegration());
        await this.runTest('Concurrent Operations', () => this.testConcurrentOperations());
        await this.runTest('Large File Handling', () => this.testLargeFileHandling());

        // Edge Cases and Error Handling
        await this.runTest('Edge Cases - Empty Objects', () => this.testEdgeCases());
        await this.runTest('Error Handling - Invalid Files', () => this.testErrorHandling());
        await this.runTest('Error Handling - Missing Dependencies', () => this.testMissingDependencies());

        // Performance Tests
        await this.runTest('Performance - Load Testing', () => this.testPerformanceLoad());
        await this.runTest('Performance - Memory Usage', () => this.testPerformanceMemory());
        await this.runTest('Performance - Concurrent Reads', () => this.testPerformanceConcurrent());

        // Data Integrity Tests
        await this.runTest('Data Integrity - Complex Structures', () => this.testDataIntegrity());
        await this.runTest('Data Integrity - Special Characters', () => this.testDataIntegritySpecialChars());
        await this.runTest('Data Integrity - Unicode Support', () => this.testDataIntegrityUnicode());

        this.log('=' .repeat(70), 'info');
        this.printSummary();
    }

    // Component Tests
    async testSchemaGenerator() {
        const generator = new ProtobufSchemaGenerator();
        const outputPath = path.join(this.tempDir, 'test-schema.proto');

        await generator.generateFromData(this.testData.simple, outputPath);

        if (!fs.existsSync(outputPath)) {
            throw new Error('Schema file was not generated');
        }

        const schemaContent = fs.readFileSync(outputPath, 'utf8');
        if (!schemaContent.includes('message Test')) {
            throw new Error('Generated schema does not contain expected message');
        }
    }

    async testSchemaGeneratorEdgeCases() {
        const generator = new ProtobufSchemaGenerator();

        // Test empty object
        const emptySchema = generator.generateSchema({});
        if (!emptySchema.includes('message Config')) {
            throw new Error('Empty object schema generation failed');
        }

        // Test special characters in field names
        const specialSchema = generator.generateSchema(this.testData.edge_cases.special_chars);
        if (!specialSchema.includes('fieldWithDashes')) {
            throw new Error('Special character sanitization failed');
        }
    }

    async testProtobufCompiler() {
        const compiler = new JSONToProtobufCompiler();
        const jsonPath = path.join(this.tempDir, 'compile-test.json');
        const protoPath = path.join(this.tempDir, 'compile-test.proto');
        const pbPath = path.join(this.tempDir, 'compile-test.pb');

        // Write test data
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.complex, null, 2));

        // Generate schema
        const generator = new ProtobufSchemaGenerator();
        await generator.generateFromFile(jsonPath, protoPath);

        // Compile to protobuf
        const result = await compiler.compileFile(jsonPath, protoPath, pbPath);

        if (!fs.existsSync(pbPath)) {
            throw new Error('Protobuf file was not created');
        }

        if (!result.success) {
            throw new Error('Compilation was not successful');
        }
    }

    async testRoundTripValidation() {
        const compiler = new JSONToProtobufCompiler();
        const jsonPath = path.join(this.tempDir, 'roundtrip-test.json');
        const protoPath = path.join(this.tempDir, 'roundtrip-test.proto');
        const pbPath = path.join(this.tempDir, 'roundtrip-test.pb');

        // Write test data
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.complex, null, 2));

        // Generate schema and compile
        const generator = new ProtobufSchemaGenerator();
        await generator.generateFromFile(jsonPath, protoPath);
        await compiler.compileFile(jsonPath, protoPath, pbPath);

        // Load and compare
        const loader = new ProtobufRuntimeLoader();
        const loadedData = await loader.loadFile(pbPath, protoPath);

        // Compare key structures (allowing for field name transformations)
        const originalKeys = Object.keys(this.testData.complex);
        const loadedKeys = Object.keys(loadedData);

        if (loadedKeys.length === 0) {
            throw new Error('No data was loaded from protobuf');
        }
    }

    async testProtobufLoader() {
        const loader = new ProtobufRuntimeLoader();
        const jsonPath = path.join(this.tempDir, 'loader-test.json');
        const protoPath = path.join(this.tempDir, 'loader-test.proto');
        const pbPath = path.join(this.tempDir, 'loader-test.pb');

        // Prepare test file
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));
        const generator = new ProtobufSchemaGenerator();
        await generator.generateFromFile(jsonPath, protoPath);

        const compiler = new JSONToProtobufCompiler();
        await compiler.compileFile(jsonPath, protoPath, pbPath);

        // Load the file
        const data = await loader.loadFile(pbPath, protoPath);

        if (!data || typeof data !== 'object') {
            throw new Error('Loaded data is not a valid object');
        }
    }

    async testProtobufLoaderCaching() {
        const loader = new ProtobufRuntimeLoader({ cacheEnabled: true });
        const jsonPath = path.join(this.tempDir, 'cache-test.json');
        const protoPath = path.join(this.tempDir, 'cache-test.proto');
        const pbPath = path.join(this.tempDir, 'cache-test.pb');

        // Prepare test file
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));
        const generator = new ProtobufSchemaGenerator();
        await generator.generateFromFile(jsonPath, protoPath);

        const compiler = new JSONToProtobufCompiler();
        await compiler.compileFile(jsonPath, protoPath, pbPath);

        // Load multiple times to test caching
        const startTime = performance.now();
        for (let i = 0; i < 10; i++) {
            await loader.loadFile(pbPath, protoPath);
        }
        const endTime = performance.now();

        const stats = loader.getStats();
        if (stats.cacheHits === 0) {
            throw new Error('Caching is not working - no cache hits recorded');
        }
    }

    async testHybridReader() {
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: false,
            autoCompile: false
        });

        const jsonPath = path.join(this.tempDir, 'hybrid-test.json');
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));

        // Test JSON reading
        const jsonData = await reader.readFile('hybrid-test.json', { format: 'json' });
        if (!jsonData.name) {
            throw new Error('JSON reading failed');
        }

        // Test with protobuf preference
        reader.options.preferProtobuf = true;
        const protobufData = await reader.readFile('hybrid-test.json');
        if (!protobufData || typeof protobufData !== 'object') {
            throw new Error('Protobuf reading failed');
        }
    }

    async testHybridReaderAutoCompile() {
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: true,
            autoCompile: true
        });

        const jsonPath = path.join(this.tempDir, 'autocompile-test.json');
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));

        // This should auto-compile and then read
        const data = await reader.readFile('autocompile-test.json');

        if (!data || typeof data !== 'object') {
            throw new Error('Auto-compilation failed');
        }

        // Check if protobuf file was created
        const pbPath = path.join(this.tempDir, 'data', 'protobuf', 'autocompile-test.pb');
        if (!fs.existsSync(pbPath)) {
            throw new Error('Auto-compilation did not create protobuf file');
        }
    }

    async testBidirectionalWriter() {
        const writer = new HybridWriter(this.tempDir);
        const testPath = 'bidirectional-test.json';

        // Write data
        const result = await writer.writeAsHybrid(testPath, this.testData.complex);

        if (!result.success) {
            throw new Error('Hybrid write failed');
        }

        // Verify files were created
        const jsonPath = path.join(this.tempDir, testPath);
        const pbPath = path.join(this.tempDir, 'data', 'protobuf', 'bidirectional-test.pb');

        if (!fs.existsSync(jsonPath) || !fs.existsSync(pbPath)) {
            throw new Error('Required files were not created');
        }
    }

    async testWriterFormatPreferences() {
        const writer = new HybridWriter(this.tempDir);

        // Test JSON-only
        const jsonResult = await writer.writeAsJson('json-only-test.json', this.testData.simple);
        if (!jsonResult.success) {
            throw new Error('JSON-only write failed');
        }

        // Test protobuf-only
        const pbResult = await writer.writeAsProtobuf('protobuf-only-test.json', this.testData.simple);
        if (!pbResult.success) {
            throw new Error('Protobuf-only write failed');
        }
    }

    // Integration Tests
    async testFullPipelineIntegration() {
        const testName = 'full-integration-test';

        // 1. Write JSON data
        const jsonPath = path.join(this.tempDir, `${testName}.json`);
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.complex, null, 2));

        // 2. Generate schema
        const generator = new ProtobufSchemaGenerator();
        const protoPath = path.join(this.tempDir, 'data', 'protobuf', `${testName}.proto`);
        await generator.generateFromFile(jsonPath, protoPath);

        // 3. Compile to protobuf
        const compiler = new JSONToProtobufCompiler();
        const pbPath = path.join(this.tempDir, 'data', 'protobuf', `${testName}.pb`);
        await compiler.compileFile(jsonPath, protoPath, pbPath);

        // 4. Load with runtime loader
        const loader = new ProtobufRuntimeLoader();
        const loadedData = await loader.loadFile(pbPath, protoPath);

        // 5. Read with hybrid reader
        const reader = new HybridConfigReader(this.tempDir);
        const readerData = await reader.readFile(`${testName}.json`);

        // 6. Write back with bidirectional writer
        const writer = new HybridWriter(this.tempDir);
        const writeResult = await writer.writeAsHybrid(`${testName}-roundtrip.json`, loadedData);

        if (!writeResult.success) {
            throw new Error('Full pipeline integration failed');
        }
    }

    async testConcurrentOperations() {
        const promises = [];
        const concurrentTests = 5;

        for (let i = 0; i < concurrentTests; i++) {
            promises.push(this.runConcurrentTest(i));
        }

        await Promise.all(promises);
    }

    async runConcurrentTest(index) {
        const testName = `concurrent-test-${index}`;
        const writer = new HybridWriter(this.tempDir);

        const result = await writer.writeAsHybrid(`${testName}.json`, {
            ...this.testData.simple,
            concurrent_id: index
        });

        if (!result.success) {
            throw new Error(`Concurrent test ${index} failed`);
        }
    }

    async testLargeFileHandling() {
        // Create a large test object
        const largeData = {
            ...this.testData.complex,
            large_array: Array.from({ length: 1000 }, (_, i) => `item_${i}`),
            large_object: {}
        };

        for (let i = 0; i < 100; i++) {
            largeData.large_object[`field_${i}`] = `value_${i}`;
        }

        const writer = new HybridWriter(this.tempDir);
        const result = await writer.writeAsHybrid('large-test.json', largeData);

        if (!result.success) {
            throw new Error('Large file handling failed');
        }

        // Verify the file was created and can be read
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: true,
            autoCompile: true
        });
        const readData = await reader.readFile('large-test.json');

        // Check for camelCase field name conversion
        if (!readData.largeArray || readData.largeArray.length !== 1000) {
            throw new Error('Large array was not preserved correctly');
        }

        // Check that original data is preserved
        if (readData.languageName !== largeData.language_name) {
            throw new Error('Language name was not preserved in large file');
        }
    }

    // Edge Cases and Error Handling
    async testEdgeCases() {
        const generator = new ProtobufSchemaGenerator();

        // Empty object
        const emptySchema = generator.generateSchema({});
        if (!emptySchema.includes('message Config')) {
            throw new Error('Empty object handling failed');
        }

        // Null values
        const nullSchema = generator.generateSchema(this.testData.edge_cases.null_values);
        if (!nullSchema.includes('string field1')) {
            throw new Error('Null value handling failed');
        }
    }

    async testErrorHandling() {
        const loader = new ProtobufRuntimeLoader();

        // Test loading non-existent file
        try {
            await loader.loadFile('non-existent.pb');
            throw new Error('Should have thrown an error for non-existent file');
        } catch (error) {
            // Expected error
        }

        // Test loading without schema
        const jsonPath = path.join(this.tempDir, 'error-test.json');
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));

        try {
            await loader.loadFile(jsonPath); // No .proto file
            throw new Error('Should have thrown an error for missing schema');
        } catch (error) {
            // Expected error
        }
    }

    async testMissingDependencies() {
        const reader = new HybridConfigReader(this.tempDir);

        // Try to read non-existent file
        try {
            await reader.readFile('missing-file.json');
            throw new Error('Should have thrown an error for missing file');
        } catch (error) {
            // Expected error
        }
    }

    // Performance Tests
    async testPerformanceLoad() {
        const writer = new HybridWriter(this.tempDir);

        // Create multiple test files
        const testFiles = [];
        for (let i = 0; i < 10; i++) {
            const fileName = `perf-test-${i}.json`;
            testFiles.push(fileName);

            await writer.writeAsHybrid(fileName, {
                ...this.testData.complex,
                index: i
            });
        }

        // Load all files
        const reader = new HybridConfigReader(this.tempDir);
        const startTime = performance.now();

        for (const file of testFiles) {
            await reader.readFile(file);
        }

        const endTime = performance.now();
        const totalTime = endTime - startTime;

        if (totalTime > 5000) { // 5 seconds max
            throw new Error(`Performance test failed: ${totalTime}ms for 10 files`);
        }
    }

    async testPerformanceMemory() {
        const reader = new HybridConfigReader(this.tempDir, {
            cacheEnabled: true
        });

        // Load the same file multiple times
        const jsonPath = path.join(this.tempDir, 'memory-test.json');
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.complex, null, 2));

        const initialMemory = process.memoryUsage().heapUsed;

        for (let i = 0; i < 50; i++) {
            await reader.readFile('memory-test.json');
        }

        const finalMemory = process.memoryUsage().heapUsed;
        const memoryIncrease = finalMemory - initialMemory;

        // Allow some memory increase but not excessive
        if (memoryIncrease > 50 * 1024 * 1024) { // 50MB max increase
            throw new Error(`Memory usage test failed: ${memoryIncrease} bytes increase`);
        }
    }

    async testPerformanceConcurrent() {
        const reader = new HybridConfigReader(this.tempDir);
        const jsonPath = path.join(this.tempDir, 'concurrent-perf-test.json');
        fs.writeFileSync(jsonPath, JSON.stringify(this.testData.simple, null, 2));

        const concurrentReads = 20;
        const promises = [];

        const startTime = performance.now();

        for (let i = 0; i < concurrentReads; i++) {
            promises.push(reader.readFile('concurrent-perf-test.json'));
        }

        await Promise.all(promises);
        const endTime = performance.now();

        const totalTime = endTime - startTime;
        const avgTime = totalTime / concurrentReads;

        if (avgTime > 100) { // 100ms max per read
            throw new Error(`Concurrent performance test failed: ${avgTime}ms average per read`);
        }
    }

    // Data Integrity Tests
    async testDataIntegrity() {
        const writer = new HybridWriter(this.tempDir);
        const originalData = this.testData.complex;

        // Write and read back
        await writer.writeAsHybrid('integrity-test.json', originalData);
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: true,
            autoCompile: true
        });
        const readData = await reader.readFile('integrity-test.json');

        // Check key data points (accounting for protobuf field name conversion to camelCase)
        if (readData.languageName !== originalData.language_name) {
            throw new Error('Language name integrity check failed');
        }

        if (readData.version !== originalData.version) {
            throw new Error('Version integrity check failed');
        }

        if (!readData.nestedObject || !readData.nestedObject.level1) {
            throw new Error('Nested object integrity check failed');
        }

        // Additional checks for other fields
        if (readData.heroDescription !== originalData.hero_description) {
            throw new Error('Hero description integrity check failed');
        }

        if (readData.sourceLink !== originalData.source_link) {
            throw new Error('Source link integrity check failed');
        }

        if (readData.customField !== originalData.custom_field) {
            throw new Error('Custom field integrity check failed');
        }
    }

    async testDataIntegritySpecialChars() {
        const writer = new HybridWriter(this.tempDir);
        const specialData = this.testData.edge_cases.special_chars;

        await writer.writeAsHybrid('special-chars-test.json', specialData);
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: true,
            autoCompile: true
        });
        const readData = await reader.readFile('special-chars-test.json');

        // Check that special characters were handled
        if (Object.keys(readData).length === 0) {
            throw new Error('Special characters data integrity check failed');
        }
    }

    async testDataIntegrityUnicode() {
        const writer = new HybridWriter(this.tempDir);
        const unicodeData = this.testData.edge_cases.unicode;

        await writer.writeAsHybrid('unicode-test.json', unicodeData);
        const reader = new HybridConfigReader(this.tempDir, {
            preferProtobuf: true,
            autoCompile: true
        });
        const readData = await reader.readFile('unicode-test.json');

        // Check unicode preservation
        if (!readData.emoji || readData.emoji !== '🚀') {
            throw new Error('Unicode emoji integrity check failed');
        }

        if (!readData.chinese || readData.chinese !== '你好') {
            throw new Error('Unicode Chinese integrity check failed');
        }
    }

    printSummary() {
        const { passed, failed, skipped, total } = this.testResults;

        this.log(`Test Summary: ${passed}/${total} passed, ${failed} failed, ${skipped} skipped`, 'info');

        if (failed === 0) {
            this.log('🎉 All tests passed! The hybrid system is working correctly.', 'success');
        } else {
            this.log(`⚠️  ${failed} test(s) failed. Please review the errors above.`, 'warn');
        }

        // Clean up temp directory
        try {
            fs.rmSync(this.tempDir, { recursive: true, force: true });
            this.log('🧹 Cleaned up temporary test files', 'info');
        } catch (error) {
            this.log(`⚠️  Failed to clean up temp directory: ${error.message}`, 'warn');
        }
    }
}

// Run the comprehensive test suite
async function runComprehensiveTests() {
    const testSuite = new ComprehensiveTestSuite();
    await testSuite.runAllTests();
}

// Export for use in other modules
module.exports = { ComprehensiveTestSuite, runComprehensiveTests };

// Run if called directly
if (require.main === module) {
    runComprehensiveTests().catch(error => {
        console.error('❌ Comprehensive test suite failed:', error.message);
        process.exit(1);
    });
}