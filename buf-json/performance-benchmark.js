/**
 * Performance Testing Suite for Hybrid JSON-Protobuf System
 *
 * This module provides comprehensive performance benchmarks comparing
 * the hybrid JSON-protobuf system against standard JSON operations.
 * It measures storage efficiency, runtime performance, and memory usage.
 */

const fs = require('fs');
const path = require('path');
const { performance } = require('perf_hooks');

// Import hybrid system components
const { HybridWriter } = require('./src/hybrid/hybrid-writer');
const { HybridConfigReader } = require('./src/hybrid/hybrid-reader');
const { ProtobufSchemaGenerator } = require('./src/hybrid/schema-generator');
const { JSONToProtobufCompiler } = require('./src/hybrid/json-to-protobuf-compiler');

class PerformanceBenchmark {
    constructor(options = {}) {
        this.options = {
            testDataDir: options.testDataDir || './data',
            resultsDir: options.resultsDir || './performance-results',
            iterations: options.iterations || 100,
            largeDataSize: options.largeDataSize || 10000,
            ...options
        };

        this.results = {
            storage: {},
            runtime: {},
            memory: {},
            compression: {}
        };

        // Ensure results directory exists
        if (!fs.existsSync(this.options.resultsDir)) {
            fs.mkdirSync(this.options.resultsDir, { recursive: true });
        }
    }

    /**
     * Generate test data sets of various sizes and complexities
     */
    generateTestDataSets() {
        const datasets = {
            small: this.generateSmallDataset(),
            medium: this.generateMediumDataset(),
            large: this.generateLargeDataset(),
            complex: this.generateComplexDataset()
        };

        // Save datasets to files
        Object.entries(datasets).forEach(([name, data]) => {
            const filePath = path.join(this.options.testDataDir, `${name}-test.json`);
            fs.writeFileSync(filePath, JSON.stringify(data, null, 2));
            console.log(`📄 Generated ${name} dataset: ${filePath}`);
        });

        return datasets;
    }

    generateSmallDataset() {
        return {
            name: "Small Config",
            version: "1.0.0",
            settings: {
                theme: "dark",
                language: "en",
                notifications: true
            },
            api: {
                baseUrl: "https://api.example.com",
                timeout: 5000,
                retries: 3
            }
        };
    }

    generateMediumDataset() {
        const users = [];
        for (let i = 0; i < 100; i++) {
            users.push({
                id: i,
                name: `User ${i}`,
                email: `user${i}@example.com`,
                role: i % 5 === 0 ? 'admin' : 'user',
                preferences: {
                    theme: i % 2 === 0 ? 'dark' : 'light',
                    notifications: i % 3 !== 0,
                    language: ['en', 'es', 'fr', 'de'][i % 4]
                }
            });
        }

        return {
            name: "Medium User Dataset",
            totalUsers: users.length,
            users: users,
            metadata: {
                generated: new Date().toISOString(),
                version: "2.0.0"
            }
        };
    }

    generateLargeDataset() {
        const records = [];
        for (let i = 0; i < this.options.largeDataSize; i++) {
            records.push({
                id: i,
                timestamp: new Date(Date.now() - Math.random() * 365 * 24 * 60 * 60 * 1000).toISOString(),
                value: Math.random() * 1000,
                category: `category_${i % 10}`,
                tags: Array.from({ length: Math.floor(Math.random() * 5) + 1 }, (_, j) => `tag_${j}`),
                metadata: {
                    source: `source_${i % 20}`,
                    priority: Math.floor(Math.random() * 5) + 1,
                    flags: {
                        active: Math.random() > 0.1,
                        verified: Math.random() > 0.2,
                        premium: Math.random() > 0.8
                    }
                }
            });
        }

        return {
            name: "Large Analytics Dataset",
            totalRecords: records.length,
            records: records,
            summary: {
                generated: new Date().toISOString(),
                avgValue: records.reduce((sum, r) => sum + r.value, 0) / records.length,
                categories: [...new Set(records.map(r => r.category))],
                dateRange: {
                    start: records[records.length - 1].timestamp,
                    end: records[0].timestamp
                }
            }
        };
    }

    generateComplexDataset() {
        const nestedStructure = {};
        for (let i = 0; i < 50; i++) {
            nestedStructure[`level_${i}`] = {
                data: Array.from({ length: 20 }, (_, j) => ({
                    id: `${i}_${j}`,
                    value: Math.random(),
                    nested: {
                        subValue: Math.random() * 100,
                        arrayData: Array.from({ length: 5 }, () => Math.random())
                    }
                })),
                metadata: {
                    count: 20,
                    type: 'complex_nested',
                    relations: Array.from({ length: 10 }, (_, j) => `relation_${i}_${j}`)
                }
            };
        }

        return {
            name: "Complex Nested Dataset",
            structure: nestedStructure,
            globalMetadata: {
                totalLevels: 50,
                structureType: 'hierarchical',
                compression: 'recommended'
            }
        };
    }

    /**
     * Benchmark storage efficiency (file sizes)
     */
    async benchmarkStorageEfficiency(datasets) {
        console.log('\n📊 Benchmarking Storage Efficiency...');

        const results = {};

        for (const [name, data] of Object.entries(datasets)) {
            console.log(`\n🔍 Testing ${name} dataset...`);

            const jsonSize = this.measureJSONSize(data);
            const protobufSize = await this.measureProtobufSize(data, name);
            const compressionRatio = ((jsonSize - protobufSize) / jsonSize * 100).toFixed(2);

            results[name] = {
                jsonSize,
                protobufSize,
                compressionRatio: parseFloat(compressionRatio),
                spaceSaved: jsonSize - protobufSize,
                efficiency: protobufSize / jsonSize
            };

            console.log(`  📄 JSON Size: ${this.formatBytes(jsonSize)}`);
            console.log(`  📦 Protobuf Size: ${this.formatBytes(protobufSize)}`);
            console.log(`  📈 Compression Ratio: ${compressionRatio}%`);
            console.log(`  💾 Space Saved: ${this.formatBytes(jsonSize - protobufSize)}`);
        }

        this.results.storage = results;
        return results;
    }

    measureJSONSize(data) {
        return Buffer.byteLength(JSON.stringify(data), 'utf8');
    }

    async measureProtobufSize(data, name) {
        try {
            // Create temporary JSON file
            const tempJsonPath = path.join(this.options.resultsDir, `${name}-temp.json`);
            fs.writeFileSync(tempJsonPath, JSON.stringify(data, null, 2));

            // Generate schema
            const schemaGen = new ProtobufSchemaGenerator();
            const schemaPath = path.join(this.options.resultsDir, `${name}.proto`);
            await schemaGen.generateFromData(data, schemaPath, { messageName: name.replace(/[^a-zA-Z0-9_]/g, '') });

            // Compile to protobuf
            const compiler = new JSONToProtobufCompiler();
            const compiledPath = path.join(this.options.resultsDir, `${name}.pb`);
            await compiler.compileFile(tempJsonPath, schemaPath, compiledPath);

            // Get file size
            const stats = fs.statSync(compiledPath);
            
            // Clean up temp file
            fs.unlinkSync(tempJsonPath);
            
            return stats.size;
        } catch (error) {
            console.warn(`⚠️ Failed to measure protobuf size for ${name}:`, error.message);
            return 0;
        }
    }

    /**
     * Benchmark runtime performance (read/write operations)
     */
    async benchmarkRuntimePerformance(datasets) {
        console.log('\n⚡ Benchmarking Runtime Performance...');

        const results = {};

        for (const [name, data] of Object.entries(datasets)) {
            console.log(`\n🔍 Testing ${name} dataset performance...`);

            // JSON operations
            const jsonWriteTime = this.measureJSONWriteTime(data);
            const jsonReadTime = this.measureJSONReadTime(data);

            // Protobuf operations
            const protobufWriteTime = await this.measureProtobufWriteTime(data, name);
            const protobufReadTime = await this.measureProtobufReadTime(name);

            results[name] = {
                json: {
                    write: jsonWriteTime,
                    read: jsonReadTime,
                    total: jsonWriteTime + jsonReadTime
                },
                protobuf: {
                    write: protobufWriteTime,
                    read: protobufReadTime,
                    total: protobufWriteTime + protobufReadTime
                },
                improvement: {
                    write: ((jsonWriteTime - protobufWriteTime) / jsonWriteTime * 100).toFixed(2),
                    read: ((jsonReadTime - protobufReadTime) / jsonReadTime * 100).toFixed(2),
                    total: ((jsonWriteTime + jsonReadTime - protobufWriteTime - protobufReadTime) / (jsonWriteTime + jsonReadTime) * 100).toFixed(2)
                }
            };

            console.log(`  📄 JSON Write: ${jsonWriteTime.toFixed(2)}ms`);
            console.log(`  📦 Protobuf Write: ${protobufWriteTime.toFixed(2)}ms`);
            console.log(`  📄 JSON Read: ${jsonReadTime.toFixed(2)}ms`);
            console.log(`  📦 Protobuf Read: ${protobufReadTime.toFixed(2)}ms`);
            console.log(`  🚀 Total Performance Improvement: ${results[name].improvement.total}%`);
        }

        this.results.runtime = results;
        return results;
    }

    measureJSONWriteTime(data) {
        const start = performance.now();
        for (let i = 0; i < this.options.iterations; i++) {
            JSON.stringify(data);
        }
        const end = performance.now();
        return (end - start) / this.options.iterations;
    }

    measureJSONReadTime(data) {
        const jsonString = JSON.stringify(data);
        const start = performance.now();
        for (let i = 0; i < this.options.iterations; i++) {
            JSON.parse(jsonString);
        }
        const end = performance.now();
        return (end - start) / this.options.iterations;
    }

    async measureProtobufWriteTime(data, name) {
        try {
            const writer = new HybridWriter(this.options.resultsDir);
            const start = performance.now();
            for (let i = 0; i < this.options.iterations; i++) {
                await writer.writeAsHybrid(`${name}_perf.pb`, data);
            }
            const end = performance.now();
            return (end - start) / this.options.iterations;
        } catch (error) {
            console.warn(`⚠️ Failed to measure protobuf write time for ${name}:`, error.message);
            return 0;
        }
    }

    async measureProtobufReadTime(name) {
        try {
            const reader = new HybridConfigReader(this.options.resultsDir);
            const start = performance.now();
            for (let i = 0; i < this.options.iterations; i++) {
                await reader.readFile(`${name}_perf.pb`);
            }
            const end = performance.now();
            return (end - start) / this.options.iterations;
        } catch (error) {
            console.warn(`⚠️ Failed to measure protobuf read time for ${name}:`, error.message);
            return 0;
        }
    }

    /**
     * Benchmark memory usage
     */
    async benchmarkMemoryUsage(datasets) {
        console.log('\n🧠 Benchmarking Memory Usage...');

        const results = {};

        for (const [name, data] of Object.entries(datasets)) {
            console.log(`\n🔍 Testing ${name} dataset memory usage...`);

            // Force garbage collection if available
            if (global.gc) {
                global.gc();
            }

            const jsonMemory = this.measureJSONMemory(data);
            const protobufMemory = await this.measureProtobufMemory(data, name);

            results[name] = {
                json: jsonMemory,
                protobuf: protobufMemory,
                difference: protobufMemory - jsonMemory,
                efficiency: (jsonMemory - protobufMemory) / jsonMemory * 100
            };

            console.log(`  📄 JSON Memory: ${this.formatBytes(jsonMemory)}`);
            console.log(`  📦 Protobuf Memory: ${this.formatBytes(protobufMemory)}`);
            console.log(`  💾 Memory Efficiency: ${results[name].efficiency.toFixed(2)}%`);
        }

        this.results.memory = results;
        return results;
    }

    measureJSONMemory(data) {
        const startMemory = process.memoryUsage().heapUsed;
        const jsonString = JSON.stringify(data);
        JSON.parse(jsonString); // Simulate usage
        const endMemory = process.memoryUsage().heapUsed;
        return endMemory - startMemory;
    }

    async measureProtobufMemory(data, name) {
        try {
            const startMemory = process.memoryUsage().heapUsed;
            const reader = new HybridConfigReader(this.options.resultsDir);
            const result = await reader.readFile(`${name}_perf.pb`);
            // Simulate usage by accessing properties
            if (result && typeof result === 'object') {
                Object.keys(result).forEach(key => result[key]);
            }
            const endMemory = process.memoryUsage().heapUsed;
            return endMemory - startMemory;
        } catch (error) {
            console.warn(`⚠️ Failed to measure protobuf memory for ${name}:`, error.message);
            return 0;
        }
    }

    /**
     * Run comprehensive benchmark suite
     */
    async runComprehensiveBenchmark() {
        console.log('🚀 Starting Comprehensive Performance Benchmark...\n');

        // Generate test datasets
        const datasets = this.generateTestDataSets();

        // Run all benchmarks
        await this.benchmarkStorageEfficiency(datasets);
        await this.benchmarkRuntimePerformance(datasets);
        await this.benchmarkMemoryUsage(datasets);

        // Generate summary report
        this.generateSummaryReport();

        console.log('\n✅ Benchmark Complete! Results saved to:', this.options.resultsDir);
        return this.results;
    }

    /**
     * Generate comprehensive summary report
     */
    generateSummaryReport() {
        const report = {
            timestamp: new Date().toISOString(),
            system: 'Hybrid JSON-Protobuf Performance Benchmark',
            configuration: this.options,
            summary: this.calculateSummaryStats(),
            detailed: this.results
        };

        const reportPath = path.join(this.options.resultsDir, 'benchmark-report.json');
        fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

        // Generate human-readable summary
        const summaryPath = path.join(this.options.resultsDir, 'benchmark-summary.md');
        const summary = this.generateMarkdownSummary();
        fs.writeFileSync(summaryPath, summary);

        console.log(`📊 Summary report saved: ${summaryPath}`);
    }

    calculateSummaryStats() {
        const storageStats = Object.values(this.results.storage);
        const runtimeStats = Object.values(this.results.runtime);
        const memoryStats = Object.values(this.results.memory);

        return {
            storage: {
                avgCompressionRatio: storageStats.reduce((sum, s) => sum + s.compressionRatio, 0) / storageStats.length,
                totalSpaceSaved: storageStats.reduce((sum, s) => sum + s.spaceSaved, 0),
                avgEfficiency: storageStats.reduce((sum, s) => sum + s.efficiency, 0) / storageStats.length
            },
            runtime: {
                avgWriteImprovement: runtimeStats.reduce((sum, r) => sum + parseFloat(r.improvement.write), 0) / runtimeStats.length,
                avgReadImprovement: runtimeStats.reduce((sum, r) => sum + parseFloat(r.improvement.read), 0) / runtimeStats.length,
                avgTotalImprovement: runtimeStats.reduce((sum, r) => sum + parseFloat(r.improvement.total), 0) / runtimeStats.length
            },
            memory: {
                avgEfficiency: memoryStats.reduce((sum, m) => sum + m.efficiency, 0) / memoryStats.length,
                totalMemorySaved: memoryStats.reduce((sum, m) => sum + m.difference, 0)
            }
        };
    }

    generateMarkdownSummary() {
        const summary = this.calculateSummaryStats();

        return `# Performance Benchmark Summary

**Generated:** ${new Date().toISOString()}

## 📊 Overall Performance Metrics

### Storage Efficiency
- **Average Compression Ratio:** ${summary.storage.avgCompressionRatio.toFixed(2)}%
- **Total Space Saved:** ${this.formatBytes(summary.storage.totalSpaceSaved)}
- **Average Efficiency:** ${(summary.storage.avgEfficiency * 100).toFixed(2)}%

### Runtime Performance
- **Average Write Improvement:** ${summary.runtime.avgWriteImprovement.toFixed(2)}%
- **Average Read Improvement:** ${summary.runtime.avgReadImprovement.toFixed(2)}%
- **Average Total Improvement:** ${summary.runtime.avgTotalImprovement.toFixed(2)}%

### Memory Usage
- **Average Memory Efficiency:** ${summary.memory.avgEfficiency.toFixed(2)}%
- **Total Memory Saved:** ${this.formatBytes(Math.abs(summary.memory.totalMemorySaved))}

## 📈 Detailed Results

### Storage Comparison
${Object.entries(this.results.storage).map(([name, data]) =>
    `#### ${name.charAt(0).toUpperCase() + name.slice(1)} Dataset\n` +
    `- JSON Size: ${this.formatBytes(data.jsonSize)}\n` +
    `- Protobuf Size: ${this.formatBytes(data.protobufSize)}\n` +
    `- Compression Ratio: ${data.compressionRatio}%\n` +
    `- Space Saved: ${this.formatBytes(data.spaceSaved)}\n`
).join('\n')}

### Runtime Performance
${Object.entries(this.results.runtime).map(([name, data]) =>
    `#### ${name.charAt(0).toUpperCase() + name.slice(1)} Dataset\n` +
    `- JSON Write: ${data.json.write.toFixed(2)}ms\n` +
    `- Protobuf Write: ${data.protobuf.write.toFixed(2)}ms\n` +
    `- JSON Read: ${data.json.read.toFixed(2)}ms\n` +
    `- Protobuf Read: ${data.protobuf.read.toFixed(2)}ms\n` +
    `- Total Improvement: ${data.improvement.total}%\n`
).join('\n')}

### Memory Usage
${Object.entries(this.results.memory).map(([name, data]) =>
    `#### ${name.charAt(0).toUpperCase() + name.slice(1)} Dataset\n` +
    `- JSON Memory: ${this.formatBytes(data.json)}\n` +
    `- Protobuf Memory: ${this.formatBytes(data.protobuf)}\n` +
    `- Memory Efficiency: ${data.efficiency.toFixed(2)}%\n`
).join('\n')}

## 🎯 Key Findings

### ✅ Advantages of Hybrid System
- **Storage:** ${summary.storage.avgCompressionRatio.toFixed(1)}% average file size reduction
- **Performance:** ${summary.runtime.avgTotalImprovement.toFixed(1)}% faster read/write operations
- **Memory:** ${summary.memory.avgEfficiency.toFixed(1)}% more memory efficient

### ⚖️ Trade-offs
- **Setup Complexity:** Requires protobuf schema generation and compilation
- **Development Overhead:** Additional tooling for hybrid workflow
- **Compatibility:** Protobuf files need specific readers

### 📋 Recommendations
- Use hybrid system for **large datasets** (>1MB) where compression benefits are significant
- Consider hybrid approach for **high-frequency operations** where performance matters
- Evaluate based on your specific **storage costs vs development complexity** requirements

---
*Benchmark Configuration: ${this.options.iterations} iterations per test*
*Test Environment: Node.js ${process.version}*
*System: ${process.platform} ${process.arch}*
`;
    }

    /**
     * Utility function to format bytes
     */
    formatBytes(bytes) {
        if (bytes === 0) return '0 Bytes';
        const k = 1024;
        const sizes = ['Bytes', 'KB', 'MB', 'GB'];
        const i = Math.floor(Math.log(bytes) / Math.log(k));
        return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    }

    /**
     * Quick benchmark for development/testing
     */
    async runQuickBenchmark() {
        console.log('⚡ Running Quick Performance Benchmark...\n');

        const smallData = this.generateSmallDataset();
        const mediumData = this.generateMediumDataset();

        // Storage test
        console.log('📊 Storage Efficiency:');
        const smallJsonSize = this.measureJSONSize(smallData);
        const smallProtobufSize = await this.measureProtobufSize(smallData, 'quick-small');
        const compression = ((smallJsonSize - smallProtobufSize) / smallJsonSize * 100).toFixed(2);

        console.log(`  Small Dataset: ${this.formatBytes(smallJsonSize)} → ${this.formatBytes(smallProtobufSize)} (${compression}% compression)`);

        const mediumJsonSize = this.measureJSONSize(mediumData);
        const mediumProtobufSize = await this.measureProtobufSize(mediumData, 'quick-medium');
        const compression2 = ((mediumJsonSize - mediumProtobufSize) / mediumJsonSize * 100).toFixed(2);

        console.log(`  Medium Dataset: ${this.formatBytes(mediumJsonSize)} → ${this.formatBytes(mediumProtobufSize)} (${compression2}% compression)`);

        // Runtime test
        console.log('\n⚡ Runtime Performance:');
        const jsonWrite = this.measureJSONWriteTime(smallData);
        const protobufWrite = await this.measureProtobufWriteTime(smallData, 'quick-perf');

        console.log(`  JSON Write: ${jsonWrite.toFixed(2)}ms`);
        console.log(`  Protobuf Write: ${protobufWrite.toFixed(2)}ms`);
        console.log(`  Improvement: ${((jsonWrite - protobufWrite) / jsonWrite * 100).toFixed(2)}%`);

        return {
            storage: {
                small: { json: smallJsonSize, protobuf: smallProtobufSize, compression: parseFloat(compression) },
                medium: { json: mediumJsonSize, protobuf: mediumProtobufSize, compression: parseFloat(compression2) }
            },
            runtime: {
                jsonWrite,
                protobufWrite,
                improvement: ((jsonWrite - protobufWrite) / jsonWrite * 100)
            }
        };
    }
}

/**
 * Convenience functions for running benchmarks
 */
async function runPerformanceBenchmark(options = {}) {
    const benchmark = new PerformanceBenchmark(options);
    return await benchmark.runComprehensiveBenchmark();
}

async function runQuickBenchmark(options = {}) {
    const benchmark = new PerformanceBenchmark(options);
    return await benchmark.runQuickBenchmark();
}

async function generatePerformanceReport(resultsDir = './performance-results') {
    const benchmark = new PerformanceBenchmark({ resultsDir });
    benchmark.generateSummaryReport();
}

// CLI interface
if (require.main === module) {
    const args = process.argv.slice(2);
    const command = args[0] || 'quick';

    switch (command) {
        case 'comprehensive':
            runPerformanceBenchmark().catch(console.error);
            break;
        case 'quick':
            runQuickBenchmark().catch(console.error);
            break;
        case 'report':
            generatePerformanceReport().catch(console.error);
            break;
        default:
            console.log('Usage: node performance-benchmark.js [quick|comprehensive|report]');
            console.log('  quick - Run quick benchmark with small/medium datasets');
            console.log('  comprehensive - Run full benchmark suite with all dataset sizes');
            console.log('  report - Generate performance report from existing results');
    }
}

module.exports = {
    PerformanceBenchmark,
    runPerformanceBenchmark,
    runQuickBenchmark,
    generatePerformanceReport
};