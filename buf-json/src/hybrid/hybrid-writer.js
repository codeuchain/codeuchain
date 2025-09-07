/**
 * Bidirectional Hybrid JSON-Protobuf Writer
 *
 * This module provides a unified interface for writing configuration data
 * that intelligently chooses between JSON and protobuf formats for optimal
 * storage efficiency while maintaining developer-friendly JSON interfaces.
 *
 * Key Features:
 * - Write data as JSON, store as protobuf (efficient storage)
 * - Automatic schema generation and updates
 * - Maintain JSON fallbacks for compatibility
 * - Smart format selection based on use case
 * - Bidirectional sync between JSON and protobuf versions
 */

const fs = require('fs');
const path = require('path');
const { ProtobufSchemaGenerator } = require('./schema-generator');
const { JSONToProtobufCompiler } = require('./json-to-protobuf-compiler');
const { ProtobufRuntimeLoader } = require('./protobuf-runtime-loader');

class HybridWriter {
    constructor(baseDir = '.', options = {}) {
        this.baseDir = baseDir;
        this.options = {
            preferProtobuf: options.preferProtobuf !== false, // Default true for efficiency
            keepJsonFallback: options.keepJsonFallback !== false, // Default true for compatibility
            autoGenerateSchema: options.autoGenerateSchema !== false, // Default true
            validateOnWrite: options.validateOnWrite !== false, // Default true
            compressOutput: options.compressOutput !== false, // Default true
            backupOnOverwrite: options.backupOnOverwrite || false,
            formatJson: options.formatJson !== false, // Default true
            jsonIndent: options.jsonIndent || 2,
            ...options
        };

        // Initialize components
        this.schemaGenerator = new ProtobufSchemaGenerator(this.options);
        this.compiler = new JSONToProtobufCompiler({
            ...this.options,
            baseDir
        });
        this.protobufLoader = new ProtobufRuntimeLoader({
            ...this.options,
            baseDir
        });

        // Track written files for sync operations
        this.writtenFiles = new Map();
        this.schemas = new Map();

        // Performance tracking
        this.stats = {
            filesWritten: 0,
            schemasGenerated: 0,
            bytesSaved: 0,
            writeTime: 0,
            validationsPerformed: 0
        };
    }

    /**
     * Write data to a file with intelligent format selection
     */
    async writeFile(filePath, data, options = {}) {
        const startTime = Date.now();
        const { format, forceFormat } = options;
        const fullPath = path.resolve(this.baseDir, filePath);

        // Validate data if requested
        if (this.options.validateOnWrite || options.validate) {
            this.validateData(data);
            this.stats.validationsPerformed++;
        }

        // Determine target format
        const targetFormat = forceFormat || format || this.decideFormat(filePath, data, options);

        try {
            if (targetFormat === 'protobuf') {
                await this.writeAsProtobuf(filePath, data, options);
            } else if (targetFormat === 'json') {
                await this.writeAsJson(filePath, data, options);
            } else if (targetFormat === 'hybrid') {
                await this.writeAsHybrid(filePath, data, options);
            }

            // Track the write operation
            this.writtenFiles.set(filePath, {
                format: targetFormat,
                timestamp: Date.now(),
                size: this.getDataSize(data),
                path: fullPath
            });

            this.stats.filesWritten++;
            this.stats.writeTime += (Date.now() - startTime);

            console.log(`💾 Successfully wrote ${filePath} as ${targetFormat}`);
            return { success: true, format: targetFormat, path: fullPath };

        } catch (error) {
            console.error(`❌ Failed to write ${filePath}:`, error.message);
            throw error;
        }
    }

    /**
     * Write data as protobuf (efficient storage)
     */
    async writeAsProtobuf(filePath, data, options = {}) {
        const protobufPath = this.getProtobufPath(filePath);
        const protoPath = this.getProtoPath(filePath);

        // Check if schema already exists (for round-trip operations)
        let schemaExists = fs.existsSync(protoPath);

        // If schema doesn't exist or regeneration is requested, generate it
        if (!schemaExists || options.regenerateSchema) {
            await this.generateSchemaForData(data, protoPath, filePath);
        }

        // Compile JSON to protobuf
        const compileResult = await this.compiler.compileData(data, protoPath, protobufPath, options);

        // Track compression savings
        const originalSize = this.getDataSize(data);
        const compressedSize = compileResult.size || fs.statSync(protobufPath).size;
        this.stats.bytesSaved += (originalSize - compressedSize);

        // Optionally maintain JSON fallback
        if (this.options.keepJsonFallback || options.keepJsonFallback) {
            await this.writeJsonFallback(filePath, data, options);
        }

        return compileResult;
    }

    /**
     * Write data as JSON (developer-friendly)
     */
    async writeAsJson(filePath, data, options = {}) {
        const fullPath = path.resolve(this.baseDir, filePath);

        // Create backup if requested
        if (this.options.backupOnOverwrite && fs.existsSync(fullPath)) {
            await this.createBackup(fullPath);
        }

        // Format JSON if requested
        const jsonString = this.formatJsonData(data, options);

        // Write the file
        fs.writeFileSync(fullPath, jsonString, 'utf8');

        console.log(`📄 Wrote JSON file: ${fullPath} (${jsonString.length} bytes)`);
        return { path: fullPath, size: jsonString.length, format: 'json' };
    }

    /**
     * Write data as hybrid (both JSON and protobuf)
     */
    async writeAsHybrid(filePath, data, options = {}) {
        // Write protobuf version (primary)
        const protobufResult = await this.writeAsProtobuf(filePath, data, options);

        // Write JSON version (fallback/compatibility)
        const jsonResult = await this.writeAsJson(filePath, data, options);

        return {
            protobuf: protobufResult,
            json: jsonResult,
            format: 'hybrid',
            primaryFormat: 'protobuf'
        };
    }

    /**
     * Update existing file with new data
     */
    async updateFile(filePath, data, options = {}) {
        const fullPath = path.resolve(this.baseDir, filePath);

        // Check if file exists and read current data for comparison
        let existingData = null;
        if (fs.existsSync(fullPath)) {
            try {
                existingData = JSON.parse(fs.readFileSync(fullPath, 'utf8'));
            } catch (error) {
                // File might be in protobuf format, try reading that way
                const protobufPath = this.getProtobufPath(filePath);
                if (fs.existsSync(protobufPath)) {
                    existingData = await this.protobufLoader.loadFile(protobufPath);
                }
            }
        }

        // Perform the write operation
        const result = await this.writeFile(filePath, data, { ...options, update: true });

        // Log the update
        if (existingData) {
            console.log(`🔄 Updated ${filePath} (previously ${this.getDataSize(existingData)} bytes, now ${result.size || 'unknown'} bytes)`);
        }

        return result;
    }

    /**
     * Generate protobuf schema for data
     */
    async generateSchemaForData(data, protoPath, sourceFile = '') {
        const fileName = path.basename(sourceFile, path.extname(sourceFile));
        const expectedMessageName = this.sanitizeMessageName(fileName);

        // Check if there's already a compatible schema in the same directory
        const protoDir = path.dirname(protoPath);
        if (fs.existsSync(protoDir)) {
            const existingSchemas = fs.readdirSync(protoDir)
                .filter(file => file.endsWith('.proto'))
                .map(file => path.join(protoDir, file));

            for (const existingSchemaPath of existingSchemas) {
                try {
                    const existingSchema = fs.readFileSync(existingSchemaPath, 'utf8');
                    const existingMessageName = this.extractMessageNameFromSchema(existingSchema);

                    // Only reuse schema if message names match AND structure is compatible
                    if (existingMessageName === expectedMessageName && this.isSchemaCompatible(data, existingSchema)) {
                        fs.copyFileSync(existingSchemaPath, protoPath);
                        this.schemas.set(protoPath, { messageName: existingMessageName, generated: Date.now() });
                        console.log(`📋 Reused existing schema: ${existingSchemaPath} -> ${protoPath} (message: ${existingMessageName})`);
                        return;
                    }
                } catch (error) {
                    // Continue checking other schemas
                    console.warn(`⚠️  Could not check existing schema ${existingSchemaPath}: ${error.message}`);
                }
            }
        }

        // Generate new schema if no compatible one found
        await this.schemaGenerator.generateFromData(data, protoPath, {
            messageName: expectedMessageName,
            package: 'codeuchain'
        });

        this.schemas.set(protoPath, { messageName: expectedMessageName, generated: Date.now() });
        this.stats.schemasGenerated++;

        console.log(`📋 Generated schema: ${protoPath} (message: ${expectedMessageName})`);
    }

    /**
     * Write JSON fallback file
     */
    async writeJsonFallback(filePath, data, options = {}) {
        const jsonPath = path.resolve(this.baseDir, filePath);
        const jsonString = this.formatJsonData(data, options);

        fs.writeFileSync(jsonPath, jsonString, 'utf8');
        console.log(`📄 Wrote JSON fallback: ${jsonPath}`);
    }

    /**
     * Decide which format to use for writing
     */
    decideFormat(filePath, data, options = {}) {
        // Explicit format override
        if (options.format) {
            return options.format;
        }

        // For configuration/development files, prefer JSON
        if (filePath.includes('config') || filePath.includes('dev') || filePath.includes('test')) {
            return this.options.keepJsonFallback ? 'hybrid' : 'json';
        }

        // For production/data files, prefer protobuf
        if (filePath.includes('data') || filePath.includes('prod') || filePath.includes('cache')) {
            return 'protobuf';
        }

        // Default to hybrid for maximum compatibility
        return 'hybrid';
    }

    /**
     * Format JSON data for writing
     */
    formatJsonData(data, options = {}) {
        const shouldFormat = (options.formatJson !== false) && this.options.formatJson;
        const indent = options.jsonIndent || this.options.jsonIndent;

        if (shouldFormat) {
            return JSON.stringify(data, null, indent);
        } else {
            return JSON.stringify(data);
        }
    }

    /**
     * Validate data before writing
     */
    validateData(data) {
        if (typeof data !== 'object' || data === null) {
            throw new Error('Data must be a valid object');
        }

        // Check for circular references
        const seen = new WeakSet();
        const checkCircular = (obj) => {
            if (typeof obj !== 'object' || obj === null) return;
            if (seen.has(obj)) throw new Error('Circular reference detected');
            seen.add(obj);
            Object.values(obj).forEach(checkCircular);
        };

        checkCircular(data);
    }

    /**
     * Create backup of existing file
     */
    async createBackup(filePath) {
        const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
        const backupPath = `${filePath}.backup.${timestamp}`;

        fs.copyFileSync(filePath, backupPath);
        console.log(`📦 Created backup: ${backupPath}`);
    }

    /**
     * Get protobuf file path for a given JSON file
     */
    getProtobufPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.baseDir, 'data', 'protobuf', `${parsed.name}.pb`);
    }

    /**
     * Get proto schema file path for a given JSON file
     */
    getProtoPath(jsonPath) {
        const parsed = path.parse(jsonPath);
        return path.join(this.baseDir, 'data', 'protobuf', `${parsed.name}.proto`);
    }

    /**
     * Sanitize message names for protobuf
     */
    sanitizeMessageName(name) {
        let cleanName = name.replace(/[^a-zA-Z0-9_]/g, '_');
        cleanName = cleanName.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
        cleanName = cleanName.charAt(0).toUpperCase() + cleanName.slice(1);

        if (/^[0-9]/.test(cleanName)) {
            cleanName = `Msg${cleanName}`;
        }

        return cleanName;
    }

    /**
     * Get data size in bytes
     */
    getDataSize(data) {
        return Buffer.byteLength(JSON.stringify(data), 'utf8');
    }

    /**
     * Sync JSON and protobuf versions of a file
     */
    async syncFile(filePath, options = {}) {
        const protobufPath = this.getProtobufPath(filePath);
        const jsonPath = path.resolve(this.baseDir, filePath);

        // Read from the newer version
        let sourceData = null;
        let sourceFormat = null;

        if (fs.existsSync(protobufPath) && fs.existsSync(jsonPath)) {
            const protobufStat = fs.statSync(protobufPath);
            const jsonStat = fs.statSync(jsonPath);

            if (protobufStat.mtime > jsonStat.mtime) {
                // Protobuf is newer
                sourceData = await this.protobufLoader.loadFile(protobufPath);
                sourceFormat = 'protobuf';
            } else {
                // JSON is newer
                sourceData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
                sourceFormat = 'json';
            }
        } else if (fs.existsSync(protobufPath)) {
            sourceData = await this.protobufLoader.loadFile(protobufPath);
            sourceFormat = 'protobuf';
        } else if (fs.existsSync(jsonPath)) {
            sourceData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
            sourceFormat = 'json';
        } else {
            throw new Error(`No file found to sync: ${filePath}`);
        }

        // Write both versions
        await this.writeAsHybrid(filePath, sourceData, options);

        console.log(`🔄 Synced ${filePath} from ${sourceFormat} to hybrid format`);
        return { synced: true, sourceFormat, data: sourceData };
    }

    /**
     * Get writer statistics
     */
    getStats() {
        const compressionRatio = this.stats.bytesSaved > 0 && this.stats.filesWritten > 0
            ? ((this.stats.bytesSaved / this.stats.filesWritten) / 100).toFixed(1) + '%'
            : '0%';

        return {
            ...this.stats,
            averageWriteTime: this.stats.filesWritten > 0
                ? `${(this.stats.writeTime / this.stats.filesWritten).toFixed(2)}ms`
                : '0ms',
            averageCompression: compressionRatio,
            filesTracked: this.writtenFiles.size,
            schemasTracked: this.schemas.size,
            options: this.options
        };
    }

    /**
     * Extract message name from existing protobuf schema
     */
    extractMessageNameFromSchema(schemaContent) {
        const messageMatch = schemaContent.match(/message\s+(\w+)\s*{/);
        return messageMatch ? messageMatch[1] : null;
    }

    /**
     * Check if existing schema is compatible with data structure
     */
    isSchemaCompatible(data, schemaContent) {
        try {
            // Extract field names from the schema
            const fieldMatches = schemaContent.matchAll(/\s+(\w+)\s*=\s*\d+/g);
            const schemaFields = Array.from(fieldMatches, match => match[1]);

            // Extract field names from data (sanitized)
            const dataFields = Object.keys(data).map(key => this.sanitizeFieldNameForSchema(key));

            // Check if all data fields are present in schema
            // Allow schema to have extra fields, but data shouldn't have fields not in schema
            const dataFieldsCovered = dataFields.every(field => schemaFields.includes(field));

            return dataFieldsCovered;
        } catch (error) {
            console.warn(`⚠️  Schema compatibility check failed: ${error.message}`);
            return false;
        }
    }

    /**
     * Sanitize field names for schema compatibility checking
     */
    sanitizeFieldNameForSchema(name) {
        // Remove import/export markers
        let cleanName = name.replace(/^import:\/\/|^export:\/\//, '');

        // Replace special characters with underscores
        cleanName = cleanName.replace(/[^a-zA-Z0-9_]/g, '_');

        // Convert to camelCase (protobufjs convention)
        cleanName = cleanName.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());

        // Ensure it starts with a letter or underscore
        if (/^[0-9]/.test(cleanName)) {
            cleanName = `_${cleanName}`;
        }

        return cleanName;
    }

    /**
     * Clear all caches and tracked files
     */
    clearCache() {
        this.writtenFiles.clear();
        this.schemas.clear();
        this.compiler.clearCache();
        this.protobufLoader.clearCache();
    }
}

/**
 * Convenience functions
 */
async function writeHybridFile(filePath, data, baseDir = '.', options = {}) {
    const writer = new HybridWriter(baseDir, options);
    return await writer.writeFile(filePath, data, options);
}

async function updateHybridFile(filePath, data, baseDir = '.', options = {}) {
    const writer = new HybridWriter(baseDir, options);
    return await writer.updateFile(filePath, data, options);
}

async function syncHybridFile(filePath, baseDir = '.', options = {}) {
    const writer = new HybridWriter(baseDir, options);
    return await writer.syncFile(filePath, options);
}

module.exports = {
    HybridWriter,
    writeHybridFile,
    updateHybridFile,
    syncHybridFile
};