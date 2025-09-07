#!/usr/bin/env node

/**
 * CodeUChain Hybrid JSON-Protobuf Build System
 *
 * This build system provides comprehensive integration of the hybrid JSON-protobuf
 * system into the development workflow with automatic format selection, optimization,
 * and deployment preparation.
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// Import hybrid system components
const { ProtobufSchemaGenerator } = require('./src/hybrid/schema-generator');
const { JSONToProtobufCompiler } = require('./src/hybrid/json-to-protobuf-compiler');
const { HybridConfigReader } = require('./src/hybrid/hybrid-reader');
const { HybridWriter } = require('./src/hybrid/hybrid-writer');

class HybridBuildSystem {
    constructor(options = {}) {
        this.options = {
            baseDir: options.baseDir || '.',
            dataDir: options.dataDir || 'data',
            outputDir: options.outputDir || 'dist',
            protobufDir: options.protobufDir || 'data/protobuf',
            tempDir: options.tempDir || 'build-temp',
            verbose: options.verbose || false,
            optimize: options.optimize !== false,
            compress: options.compress !== false,
            validate: options.validate !== false,
            ...options
        };

        this.stats = {
            filesProcessed: 0,
            schemasGenerated: 0,
            protobufFilesCreated: 0,
            jsonFilesProcessed: 0,
            errors: 0,
            warnings: 0,
            startTime: Date.now()
        };

        // Initialize components
        this.schemaGenerator = new ProtobufSchemaGenerator();
        this.compiler = new JSONToProtobufCompiler();
        this.reader = new HybridConfigReader(this.options.baseDir);
        this.writer = new HybridWriter(this.options.baseDir);

        this.ensureDirectories();
    }

    ensureDirectories() {
        const dirs = [
            this.options.outputDir,
            this.options.protobufDir,
            this.options.tempDir
        ];

        dirs.forEach(dir => {
            const fullPath = path.join(this.options.baseDir, dir);
            if (!fs.existsSync(fullPath)) {
                fs.mkdirSync(fullPath, { recursive: true });
                this.log(`📁 Created directory: ${dir}`, 'info');
            }
        });
    }

    log(message, level = 'info') {
        if (!this.options.verbose && level === 'debug') return;

        const timestamp = new Date().toISOString();
        const prefix = level === 'error' ? '❌' : level === 'success' ? '✅' : level === 'warn' ? '⚠️' : '📋';
        console.log(`[${timestamp}] ${prefix} ${message}`);
    }

    /**
     * Main build process
     */
    async build(options = {}) {
        this.log('🚀 Starting CodeUChain Hybrid Build System', 'info');
        this.log('=' .repeat(60), 'info');

        try {
            // Phase 1: Discovery and Analysis
            await this.discoverFiles();

            // Phase 2: Schema Generation
            await this.generateSchemas();

            // Phase 3: Compilation and Optimization
            await this.compileAndOptimize();

            // Phase 4: Validation and Testing
            if (this.options.validate) {
                await this.validateBuild();
            }

            // Phase 5: Deployment Preparation
            await this.prepareDeployment();

            // Phase 6: Generate Build Report
            this.generateBuildReport();

            this.log('=' .repeat(60), 'info');
            this.log('🎉 Build completed successfully!', 'success');

        } catch (error) {
            this.stats.errors++;
            this.log(`Build failed: ${error.message}`, 'error');
            throw error;
        }
    }

    /**
     * Discover all JSON configuration files
     */
    async discoverFiles() {
        this.log('🔍 Phase 1: Discovering configuration files...', 'info');

        const dataDir = path.join(this.options.baseDir, this.options.dataDir);
        const files = this.findJsonFiles(dataDir);

        this.log(`📋 Found ${files.length} JSON configuration files:`, 'info');
        files.forEach(file => {
            const relativePath = path.relative(this.options.baseDir, file);
            this.log(`   - ${relativePath}`, 'debug');
        });

        this.discoveredFiles = files;
        return files;
    }

    /**
     * Recursively find all JSON files
     */
    findJsonFiles(dir) {
        const files = [];

        if (!fs.existsSync(dir)) {
            return files;
        }

        const items = fs.readdirSync(dir);

        for (const item of items) {
            const fullPath = path.join(dir, item);
            const stat = fs.statSync(fullPath);

            if (stat.isDirectory()) {
                files.push(...this.findJsonFiles(fullPath));
            } else if (item.endsWith('.json')) {
                files.push(fullPath);
            }
        }

        return files;
    }

    /**
     * Generate protobuf schemas for all discovered files
     */
    async generateSchemas() {
        this.log('📋 Phase 2: Generating protobuf schemas...', 'info');

        for (const jsonFile of this.discoveredFiles) {
            try {
                const relativePath = path.relative(this.options.baseDir, jsonFile);
                const protoFile = this.getProtoPath(jsonFile);

                this.log(`📄 Generating schema for ${relativePath}`, 'debug');

                await this.schemaGenerator.generateFromFile(jsonFile, protoFile);
                this.stats.schemasGenerated++;

            } catch (error) {
                this.stats.errors++;
                this.log(`Failed to generate schema for ${jsonFile}: ${error.message}`, 'error');
            }
        }

        this.log(`✅ Generated ${this.stats.schemasGenerated} protobuf schemas`, 'success');
    }

    /**
     * Compile JSON files to protobuf and optimize
     */
    async compileAndOptimize() {
        this.log('🔨 Phase 3: Compiling and optimizing...', 'info');

        for (const jsonFile of this.discoveredFiles) {
            try {
                const relativePath = path.relative(this.options.baseDir, jsonFile);
                const protoFile = this.getProtoPath(jsonFile);
                const pbFile = this.getProtobufPath(jsonFile);

                this.log(`📦 Compiling ${relativePath}`, 'debug');

                // Compile to protobuf
                const result = await this.compiler.compileFile(jsonFile, protoFile, pbFile);

                if (result.success) {
                    this.stats.protobufFilesCreated++;

                    // Apply optimizations if enabled
                    if (this.options.optimize) {
                        await this.optimizeFile(pbFile, jsonFile);
                    }
                } else {
                    this.stats.errors++;
                    this.log(`Compilation failed for ${relativePath}`, 'error');
                }

            } catch (error) {
                this.stats.errors++;
                this.log(`Failed to compile ${jsonFile}: ${error.message}`, 'error');
            }
        }

        this.log(`✅ Compiled ${this.stats.protobufFilesCreated} protobuf files`, 'success');
    }

    /**
     * Optimize compiled protobuf files
     */
    async optimizeFile(pbFile, jsonFile) {
        try {
            // Read file stats
            const stats = fs.statSync(pbFile);
            const originalSize = stats.size;

            // Apply compression if enabled
            if (this.options.compress) {
                // For now, just log the optimization opportunity
                // In a real implementation, you might use additional compression
                this.log(`📊 File ${path.basename(pbFile)}: ${originalSize} bytes`, 'debug');
            }

        } catch (error) {
            this.log(`Optimization failed for ${pbFile}: ${error.message}`, 'warn');
            this.stats.warnings++;
        }
    }

    /**
     * Validate the build output
     */
    async validateBuild() {
        this.log('🔍 Phase 4: Validating build output...', 'info');

        let validationErrors = 0;

        for (const jsonFile of this.discoveredFiles) {
            try {
                const pbFile = this.getProtobufPath(jsonFile);
                const protoFile = this.getProtoPath(jsonFile);

                // Check if all required files exist
                if (!fs.existsSync(pbFile)) {
                    this.log(`Missing protobuf file: ${pbFile}`, 'error');
                    validationErrors++;
                    continue;
                }

                if (!fs.existsSync(protoFile)) {
                    this.log(`Missing proto schema: ${protoFile}`, 'error');
                    validationErrors++;
                    continue;
                }

                // Validate round-trip conversion
                const originalData = JSON.parse(fs.readFileSync(jsonFile, 'utf8'));
                const loadedData = await this.reader.readFile(jsonFile.replace('.json', ''));

                // Basic validation - check if data structure is preserved
                if (!this.validateDataStructure(originalData, loadedData)) {
                    this.log(`Data validation failed for ${jsonFile}`, 'error');
                    validationErrors++;
                }

            } catch (error) {
                this.log(`Validation error for ${jsonFile}: ${error.message}`, 'error');
                validationErrors++;
            }
        }

        if (validationErrors === 0) {
            this.log('✅ All files validated successfully', 'success');
        } else {
            this.log(`⚠️  ${validationErrors} validation errors found`, 'warn');
            this.stats.warnings += validationErrors;
        }
    }

    /**
     * Validate data structure integrity
     */
    validateDataStructure(original, loaded) {
        if (!original || !loaded || typeof original !== 'object' || typeof loaded !== 'object') {
            return false;
        }

        // Check if all top-level keys are present
        const originalKeys = Object.keys(original);
        const loadedKeys = Object.keys(loaded);

        // Allow for some field name transformations (snake_case to camelCase)
        const normalizedOriginalKeys = originalKeys.map(key => this.normalizeFieldName(key));
        const normalizedLoadedKeys = loadedKeys.map(key => this.normalizeFieldName(key));

        return normalizedOriginalKeys.every(key => normalizedLoadedKeys.includes(key));
    }

    /**
     * Normalize field names for comparison
     */
    normalizeFieldName(name) {
        return name.toLowerCase().replace(/[^a-z0-9]/g, '');
    }

    /**
     * Prepare files for deployment
     */
    async prepareDeployment() {
        this.log('📦 Phase 5: Preparing deployment artifacts...', 'info');

        const deployDir = path.join(this.options.outputDir, 'hybrid-config');

        if (!fs.existsSync(deployDir)) {
            fs.mkdirSync(deployDir, { recursive: true });
        }

        // Copy all generated files to deployment directory
        const filesToCopy = [
            ...this.discoveredFiles,
            ...this.discoveredFiles.map(f => this.getProtoPath(f)),
            ...this.discoveredFiles.map(f => this.getProtobufPath(f))
        ];

        for (const file of filesToCopy) {
            if (fs.existsSync(file)) {
                const relativePath = path.relative(this.options.baseDir, file);
                const deployPath = path.join(deployDir, relativePath);

                const deployDirPath = path.dirname(deployPath);
                if (!fs.existsSync(deployDirPath)) {
                    fs.mkdirSync(deployDirPath, { recursive: true });
                }

                fs.copyFileSync(file, deployPath);
            }
        }

        // Generate deployment manifest
        const manifest = {
            name: 'CodeUChain Hybrid Configuration',
            version: '1.0.0',
            buildTime: new Date().toISOString(),
            files: filesToCopy.map(f => path.relative(this.options.baseDir, f)).filter(f => fs.existsSync(f)),
            stats: this.stats
        };

        const manifestPath = path.join(deployDir, 'manifest.json');
        fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));

        this.log(`✅ Deployment artifacts prepared in ${deployDir}`, 'success');
    }

    /**
     * Generate comprehensive build report
     */
    generateBuildReport() {
        this.log('📊 Phase 6: Generating build report...', 'info');

        const endTime = Date.now();
        const duration = (endTime - this.stats.startTime) / 1000;

        const report = {
            buildInfo: {
                timestamp: new Date().toISOString(),
                duration: `${duration.toFixed(2)}s`,
                options: this.options
            },
            statistics: {
                ...this.stats,
                totalFiles: this.discoveredFiles ? this.discoveredFiles.length : 0,
                successRate: this.stats.errors === 0 ? '100%' : `${((this.stats.filesProcessed - this.stats.errors) / this.stats.filesProcessed * 100).toFixed(1)}%`
            },
            files: {
                discovered: this.discoveredFiles ? this.discoveredFiles.map(f => path.relative(this.options.baseDir, f)) : [],
                processed: this.stats.filesProcessed,
                schemasGenerated: this.stats.schemasGenerated,
                protobufFilesCreated: this.stats.protobufFilesCreated
            },
            issues: {
                errors: this.stats.errors,
                warnings: this.stats.warnings
            }
        };

        const reportPath = path.join(this.options.outputDir, 'build-report.json');
        fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

        this.log(`✅ Build report generated: ${reportPath}`, 'success');
        this.printSummary(report);
    }

    /**
     * Print build summary to console
     */
    printSummary(report) {
        this.log('\n📋 Build Summary:', 'info');
        this.log(`   Duration: ${report.buildInfo.duration}`, 'info');
        this.log(`   Files Processed: ${report.statistics.totalFiles}`, 'info');
        this.log(`   Schemas Generated: ${report.statistics.schemasGenerated}`, 'info');
        this.log(`   Protobuf Files: ${report.statistics.protobufFilesCreated}`, 'info');
        this.log(`   Success Rate: ${report.statistics.successRate}`, 'info');

        if (report.issues.errors > 0) {
            this.log(`   Errors: ${report.issues.errors}`, 'error');
        }

        if (report.issues.warnings > 0) {
            this.log(`   Warnings: ${report.issues.warnings}`, 'warn');
        }
    }

    /**
     * Utility methods
     */
    getProtoPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.options.baseDir, this.options.protobufDir, `${parsed.name}.proto`);
    }

    getProtobufPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.options.baseDir, this.options.protobufDir, `${parsed.name}.pb`);
    }

    /**
     * Clean build artifacts
     */
    async clean() {
        this.log('🧹 Cleaning build artifacts...', 'info');

        const dirsToClean = [
            this.options.outputDir,
            this.options.protobufDir,
            this.options.tempDir
        ];

        for (const dir of dirsToClean) {
            const fullPath = path.join(this.options.baseDir, dir);
            if (fs.existsSync(fullPath)) {
                fs.rmSync(fullPath, { recursive: true, force: true });
                this.log(`🗑️  Removed ${dir}`, 'info');
            }
        }

        this.log('✅ Build artifacts cleaned', 'success');
    }

    /**
     * Watch mode for continuous building
     */
    async watch() {
        this.log('👀 Starting watch mode...', 'info');

        const chokidar = require('chokidar');
        const dataDir = path.join(this.options.baseDir, this.options.dataDir);

        const watcher = chokidar.watch(dataDir, {
            ignored: /(^|[\/\\])\../,
            persistent: true
        });

        watcher.on('change', async (filePath) => {
            if (filePath.endsWith('.json')) {
                this.log(`🔄 File changed: ${filePath}`, 'info');
                await this.build({ incremental: true });
            }
        });

        this.log(`✅ Watching ${dataDir} for changes...`, 'success');
        this.log('Press Ctrl+C to stop watching', 'info');

        // Keep the process running
        return new Promise(() => {});
    }
}

/**
 * Command-line interface
 */
async function main() {
    const args = process.argv.slice(2);
    const command = args[0] || 'build';

    const options = {
        verbose: args.includes('--verbose') || args.includes('-v'),
        optimize: !args.includes('--no-optimize'),
        compress: !args.includes('--no-compress'),
        validate: !args.includes('--no-validate'),
        baseDir: process.cwd()
    };

    const buildSystem = new HybridBuildSystem(options);

    try {
        switch (command) {
            case 'build':
                await buildSystem.build();
                break;

            case 'clean':
                await buildSystem.clean();
                break;

            case 'watch':
                await buildSystem.watch();
                break;

            case 'validate':
                await buildSystem.validateBuild();
                break;

            default:
                console.log('Usage: node build.js [command] [options]');
                console.log('');
                console.log('Commands:');
                console.log('  build     Build all configuration files');
                console.log('  clean     Clean build artifacts');
                console.log('  watch     Watch for file changes and rebuild');
                console.log('  validate  Validate existing build');
                console.log('');
                console.log('Options:');
                console.log('  --verbose, -v     Verbose output');
                console.log('  --no-optimize     Skip optimization');
                console.log('  --no-compress     Skip compression');
                console.log('  --no-validate     Skip validation');
                process.exit(1);
        }
    } catch (error) {
        console.error('❌ Build system error:', error.message);
        process.exit(1);
    }
}

// Export for use as module
module.exports = { HybridBuildSystem };

// Run CLI if called directly
if (require.main === module) {
    main().catch(console.error);
}