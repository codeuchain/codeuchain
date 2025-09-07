/**
 * Hybrid JSON-Protobuf Reader
 *
 * This module provides a unified interface for reading configuration files
 * that seamlessly switches between JSON (development) and protobuf (production)
 * formats while maintaining the same API surface.
 */

const fs = require('fs');
const path = require('path');
const { RuntimeJSONReader } = require('../../json-reader');
const { ProtobufSchemaGenerator } = require('./schema-generator');
const { JSONToProtobufCompiler } = require('./json-to-protobuf-compiler');
const { ProtobufRuntimeLoader } = require('./protobuf-runtime-loader');
const { HybridWriter } = require('./hybrid-writer');

class HybridConfigReader {
    constructor(baseDir = '.', options = {}) {
        this.baseDir = baseDir;
        this.options = {
            preferProtobuf: options.preferProtobuf || false,
            fallbackToJson: options.fallbackToJson !== false, // Default true
            autoCompile: options.autoCompile !== false,       // Default true
            cacheCompiled: options.cacheCompiled !== false,   // Default true
            preserveFieldNames: options.preserveFieldNames || false,
            formatJson: options.formatJson !== false,         // Default true - pretty print JSON
            jsonIndent: options.jsonIndent || 2,              // Default 2 spaces indentation
            ...options
        };

        // Initialize components
        this.jsonReader = new RuntimeJSONReader(baseDir);
        this.schemaGenerator = new ProtobufSchemaGenerator(this.options);
        this.compiler = new JSONToProtobufCompiler({
            ...this.options,
            baseDir
        });
        this.protobufLoader = new ProtobufRuntimeLoader({
            ...this.options,
            baseDir
        });
        this.writer = new HybridWriter(baseDir, this.options);

        // Track which files have protobuf versions
        this.protobufFiles = new Set();
        this.compiledFiles = new Map();

        // Performance tracking
        this.stats = {
            jsonReads: 0,
            protobufReads: 0,
            autoCompilations: 0,
            cacheHits: 0,
            totalLoadTime: 0
        };
    }

    /**
     * Format JSON data for output
     */
    formatJsonOutput(data, options = {}) {
        // Check if formatting is disabled in options or globally
        const shouldFormat = (options.formatJson !== false) && (this.options.formatJson !== false);
        if (!shouldFormat && !options.forceFormat) {
            return data;
        }

        const indent = options.indent || this.options.jsonIndent;

        if (typeof data === 'string') {
            try {
                // If it's already a JSON string, parse and reformat
                const parsed = JSON.parse(data);
                return JSON.stringify(parsed, null, indent);
            } catch (error) {
                // If it's not valid JSON, return as-is
                return data;
            }
        }

        // If it's an object, stringify it with formatting
        if (typeof data === 'object' && data !== null) {
            return JSON.stringify(data, null, indent);
        }

        // For primitive types, return as-is
        return data;
    }

    /**
     * Format JSON data with custom replacer function
     */
    formatJsonWithReplacer(data, replacer = null, options = {}) {
        if (!this.options.formatJson && !options.forceFormat) {
            return data;
        }

        const indent = options.indent || this.options.jsonIndent;

        if (typeof data === 'string') {
            try {
                const parsed = JSON.parse(data);
                return JSON.stringify(parsed, replacer, indent);
            } catch (error) {
                return data;
            }
        }

        return JSON.stringify(data, replacer, indent);
    }
    async readFile(filePath, options = {}) {
        const startTime = Date.now();
        const { format } = options;
        const fullPath = path.resolve(this.baseDir, filePath);

        // If format is explicitly specified
        if (format === 'json') {
            this.stats.jsonReads++;
            const result = await this.readJsonFile(filePath, options);
            this.stats.totalLoadTime += (Date.now() - startTime);
            return result;
        } else if (format === 'protobuf') {
            this.stats.protobufReads++;
            const result = await this.readProtobufFile(filePath, options);
            this.stats.totalLoadTime += (Date.now() - startTime);
            return result;
        }

        // Auto-detect format with intelligent decision making
        const decision = await this.decideFormat(filePath);

        if (decision.format === 'protobuf' && decision.hasProtobuf) {
            try {
                this.stats.protobufReads++;
                const result = await this.readProtobufFile(filePath, options);
                this.stats.totalLoadTime += (Date.now() - startTime);
                return result;
            } catch (error) {
                console.warn(`⚠️  Failed to read protobuf version of ${filePath}, falling back to JSON:`, error.message);
                if (this.options.fallbackToJson) {
                    this.stats.jsonReads++;
                    const result = await this.readJsonFile(filePath, options);
                    this.stats.totalLoadTime += (Date.now() - startTime);
                    return result;
                }
                throw error;
            }
        } else {
            // Use JSON (development mode or no protobuf available)
            this.stats.jsonReads++;

            // Auto-compile to protobuf if enabled and no protobuf exists
            if (this.options.autoCompile && !decision.hasProtobuf && decision.canCompile) {
                try {
                    await this.autoCompileToProtobuf(filePath);
                    this.stats.autoCompilations++;
                } catch (error) {
                    console.warn(`⚠️  Auto-compilation failed for ${filePath}:`, error.message);
                }
            }

            const result = await this.readJsonFile(filePath, options);
            this.stats.totalLoadTime += (Date.now() - startTime);
            return result;
        }
    }

    /**
     * Read JSON file using the existing JSON reader
     */
    async readJsonFile(filePath, options = {}) {
        return await this.jsonReader.readFile(filePath, options);
    }

    /**
     * Read protobuf file using the runtime loader
     */
    async readProtobufFile(filePath, options = {}) {
        const protobufPath = this.getProtobufPath(filePath);
        const protoPath = this.getProtoPath(filePath);

        console.log(`🔍 Hybrid reader: Loading protobuf from ${protobufPath}`);
        console.log(`🔍 Hybrid reader: Using proto schema from ${protoPath}`);

        if (!fs.existsSync(protobufPath)) {
            throw new Error(`Protobuf file not found: ${protobufPath}`);
        }

        if (!fs.existsSync(protoPath)) {
            throw new Error(`Proto schema not found: ${protoPath}`);
        }

        // Use the protobuf runtime loader
        const loadOptions = {
            preserveFieldNames: this.options.preserveFieldNames,
            ...options
        };

        return await this.protobufLoader.loadFile(protobufPath, protoPath, loadOptions);
    }

    /**
     * Check if a protobuf version exists for a given file
     */
    async hasProtobufVersion(filePath) {
        const protobufPath = this.getProtobufPath(filePath);
        return fs.existsSync(protobufPath);
    }

    /**
     * Get the protobuf file path for a given JSON file
     */
    getProtobufPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.baseDir, 'data', 'protobuf', `${parsed.name}.pb`);
    }

    /**
     * Get the proto schema file path for a given JSON file
     */
    getProtoPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.baseDir, 'data', 'protobuf', `${parsed.name}.proto`);
    }

    /**
     * Check if a protobuf version exists for a given file
     */
    async hasProtobufVersion(filePath) {
        const protobufPath = this.getProtobufPath(filePath);
        const protoPath = this.getProtoPath(filePath);
        return fs.existsSync(protobufPath) && fs.existsSync(protoPath);
    }

    /**
     * Compile JSON files to protobuf format
     */
    async compileToProtobuf(filePath, outputPath = null) {
        const jsonPath = path.resolve(this.baseDir, filePath);
        const protoPath = this.getProtoPath(filePath);

        // Generate schema if it doesn't exist
        if (!fs.existsSync(protoPath)) {
            await this.schemaGenerator.generateFromFile(jsonPath, protoPath);
        }

        // Compile to protobuf
        const result = await this.compiler.compileFile(jsonPath, protoPath, outputPath || this.getProtobufPath(filePath));
        this.compiledFiles.set(filePath, result);

        return result;
    }

    /**
     * Compile all JSON files in a directory to protobuf
     */
    async compileDirectoryToProtobuf(dirPath = 'data') {
        const fullDirPath = path.resolve(this.baseDir, dirPath);
        return await this.compiler.compileDirectory(fullDirPath);
    }

    /**
     * Generate protobuf schemas for all JSON files
     */
    async generateSchemasForDirectory(dirPath = 'data') {
        const fullDirPath = path.resolve(this.baseDir, dirPath);
        return await this.schemaGenerator.generateFromDirectory(fullDirPath);
    }

    /**
     * Decide which format to use based on availability and preferences
     */
    async decideFormat(filePath) {
        const jsonPath = path.resolve(this.baseDir, filePath);
        const protobufPath = this.getProtobufPath(filePath);
        const protoPath = this.getProtoPath(filePath);

        const hasJson = fs.existsSync(jsonPath);
        const hasProtobuf = fs.existsSync(protobufPath);
        const hasProto = fs.existsSync(protoPath);

        // If protobuf is preferred and available, use it
        if (this.options.preferProtobuf && hasProtobuf && hasProto) {
            return { format: 'protobuf', hasProtobuf: true, canCompile: false };
        }

        // If JSON exists, use it (development mode)
        if (hasJson) {
            return { format: 'json', hasProtobuf, canCompile: hasJson };
        }

        // If only protobuf exists, use it
        if (hasProtobuf && hasProto) {
            return { format: 'protobuf', hasProtobuf: true, canCompile: false };
        }

        throw new Error(`No readable format found for ${filePath}`);
    }

    /**
     * Auto-compile JSON to protobuf in the background
     */
    async autoCompileToProtobuf(filePath) {
        const jsonPath = path.resolve(this.baseDir, filePath);
        const protoPath = this.getProtoPath(filePath);

        // Generate schema if it doesn't exist
        if (!fs.existsSync(protoPath)) {
            await this.schemaGenerator.generateFromFile(jsonPath, protoPath);
        }

        // Compile to protobuf
        const result = await this.compiler.compileFile(jsonPath, protoPath);
        this.compiledFiles.set(filePath, result);

        console.log(`🔄 Auto-compiled ${filePath} for future use`);
        return result;
    }

    /**
     * Read file and return formatted JSON string
     */
    async readFormatted(filePath, options = {}) {
        const data = await this.readFile(filePath, { ...options, formatJson: true });
        return JSON.stringify(data, null, 2);
    }

    /**
     * Read file and return raw data object (no formatting)
     */
    async readRaw(filePath, options = {}) {
        return await this.readFile(filePath, { ...options, formatJson: false });
    }

    /**
     * Read file and return compact JSON string
     */
    async readCompact(filePath, options = {}) {
        const data = await this.readRaw(filePath, options);
        return JSON.stringify(data);
    }

    /**
     * Export data as formatted JSON file
     */
    async exportAsJson(filePath, outputPath, options = {}) {
        const data = await this.readFile(filePath, options);
        const formattedJson = JSON.stringify(data, null, 2);

        const fullOutputPath = path.resolve(this.baseDir, outputPath);
        fs.writeFileSync(fullOutputPath, formattedJson, 'utf8');

        console.log(`💾 Exported formatted JSON to ${fullOutputPath}`);
        return fullOutputPath;
    }
    clearCache() {
        this.jsonReader.clearCache();
        this.compiler.clearCache();
        this.protobufLoader.clearCache();
        this.compiledFiles.clear();
        this.protobufFiles.clear();
    }

    /**
     * Write data back to the hybrid system
     */
    async writeFile(filePath, data, options = {}) {
        return await this.writer.writeFile(filePath, data, options);
    }

    /**
     * Update existing file with new data
     */
    async updateFile(filePath, data, options = {}) {
        return await this.writer.updateFile(filePath, data, options);
    }

    /**
     * Sync JSON and protobuf versions of a file
     */
    async syncFile(filePath, options = {}) {
        return await this.writer.syncFile(filePath, options);
    }

    /**
     * Write data as JSON (developer-friendly format)
     */
    async writeAsJson(filePath, data, options = {}) {
        return await this.writer.writeAsJson(filePath, data, { ...options, format: 'json' });
    }

    /**
     * Write data as protobuf (efficient storage format)
     */
    async writeAsProtobuf(filePath, data, options = {}) {
        return await this.writer.writeAsProtobuf(filePath, data, { ...options, format: 'protobuf' });
    }

    /**
     * Write data as hybrid (both JSON and protobuf)
     */
    async writeAsHybrid(filePath, data, options = {}) {
        return await this.writer.writeAsHybrid(filePath, data, { ...options, format: 'hybrid' });
    }

    /**
     * Get reader statistics
     */
    getStats() {
        const jsonStats = this.jsonReader.cache.size;
        const protobufStats = this.protobufLoader.getStats();
        const compilerStats = this.compiler.getStats();

        return {
            ...this.stats,
            jsonCacheSize: jsonStats,
            protobufLoaderStats: protobufStats,
            compilerStats: compilerStats,
            compiledFilesCount: this.compiledFiles.size,
            options: this.options,
            averageLoadTime: this.stats.totalLoadTime > 0
                ? `${(this.stats.totalLoadTime / (this.stats.jsonReads + this.stats.protobufReads)).toFixed(2)}ms`
                : '0ms'
        };
    }
}

/**
 * Convenience function for hybrid reading
 */
async function readHybridConfig(filePath, baseDir = '.', options = {}) {
    const reader = new HybridConfigReader(baseDir, options);
    return await reader.readFile(filePath);
}

/**
 * Convenience function for hybrid writing
 */
async function writeHybridConfig(filePath, data, baseDir = '.', options = {}) {
    const reader = new HybridConfigReader(baseDir, options);
    return await reader.writeFile(filePath, data, options);
}

/**
 * Convenience function for updating hybrid config
 */
async function updateHybridConfig(filePath, data, baseDir = '.', options = {}) {
    const reader = new HybridConfigReader(baseDir, options);
    return await reader.updateFile(filePath, data, options);
}

module.exports = {
    HybridConfigReader,
    readHybridConfig,
    writeHybridConfig,
    updateHybridConfig
};