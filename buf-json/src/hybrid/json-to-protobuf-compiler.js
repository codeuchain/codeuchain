/**
 * JSON-to-Protobuf Compiler
 *
 * This module compiles JSON configuration data to Protocol Buffer binary format
 * for efficient storage while maintaining the ability to convert back to JSON
 * for API compatibility.
 */

const fs = require('fs');
const path = require('path');
const protobuf = require('protobufjs');

class JSONToProtobufCompiler {
    constructor(options = {}) {
        this.options = {
            outputDir: options.outputDir || 'protobuf',
            keepJsonFallback: options.keepJsonFallback !== false,
            compressOutput: options.compressOutput || false,
            includeMetadata: options.includeMetadata !== false,
            ...options
        };

        // Cache for loaded protobuf schemas
        this.schemaCache = new Map();
    }

    /**
     * Compile JSON data object to protobuf binary format
     */
    async compileData(jsonData, protoPath, outputPath, options = {}) {
        // Load and compile protobuf schema
        const root = await this.loadProtoSchema(protoPath);
        const fileName = path.basename(outputPath, '.pb');
        const messageName = this.sanitizeMessageName(fileName);
        const MessageClass = root.lookupType(messageName);

        // Create mapping for JSON field names to protobuf field names
        const mappedData = this.mapJsonToProtobufFields(jsonData);

        // Convert JSON to protobuf message
        const message = MessageClass.fromObject(mappedData);

        // Serialize to binary
        const buffer = MessageClass.encode(message).finish();

        // Ensure output directory exists
        const outputDir = path.dirname(outputPath);
        if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true });
        }

        // Write compiled data
        fs.writeFileSync(outputPath, buffer);

        // Create metadata
        const metadata = this.options.includeMetadata ? {
            jsonSize: JSON.stringify(jsonData).length,
            protobufSize: buffer.length,
            compressionRatio: (buffer.length / JSON.stringify(jsonData).length).toFixed(3)
        } : undefined;

        console.log(`✅ Compiled data to ${outputPath}`);
        if (metadata) {
            console.log(`   📊 Compression ratio: ${metadata.compressionRatio}`);
        }

        return {
            outputPath,
            metadata,
            buffer,
            size: buffer.length
        };
    }
    async compileFile(jsonPath, protoPath = null, outputPath = null) {
        const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
        const fileName = path.basename(jsonPath, '.json');

        // Determine proto path
        if (!protoPath) {
            protoPath = path.join(path.dirname(jsonPath), this.options.outputDir, `${fileName}.proto`);
        }

        // Determine output path
        if (!outputPath) {
            outputPath = path.join(path.dirname(jsonPath), this.options.outputDir, `${fileName}.pb`);
        }

        // Ensure output directory exists
        const outputDir = path.dirname(outputPath);
        if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true });
        }

                // Load and compile protobuf schema
        const root = await this.loadProtoSchema(protoPath);
        const baseFileName = path.basename(jsonPath, '.json');
        const messageName = this.sanitizeMessageName(baseFileName);
        const MessageClass = root.lookupType(messageName); // Use sanitized message name

        // Create mapping for JSON field names to protobuf field names
        const mappedData = this.mapJsonToProtobufFields(jsonData);

        // Convert JSON to protobuf message
        const message = MessageClass.fromObject(mappedData);

        // Serialize to binary
        const buffer = MessageClass.encode(message).finish();

        // Create compiled package with metadata
        const compiledData = {
            version: '1.0.0',
            compiledAt: new Date().toISOString(),
            originalFile: path.basename(jsonPath),
            protobufSchema: path.basename(protoPath),
            data: buffer,
            metadata: this.options.includeMetadata ? {
                jsonSize: JSON.stringify(jsonData).length,
                protobufSize: buffer.length,
                compressionRatio: (buffer.length / JSON.stringify(jsonData).length).toFixed(3)
            } : undefined
        };

        // Write compiled data
        fs.writeFileSync(outputPath, buffer);

        // Optionally keep JSON fallback
        if (this.options.keepJsonFallback) {
            const fallbackPath = `${outputPath}.json`;
            fs.writeFileSync(fallbackPath, JSON.stringify(compiledData, null, 2));
        }

        console.log(`✅ Compiled ${jsonPath} to ${outputPath}`);
        if (compiledData.metadata) {
            console.log(`   📊 Compression ratio: ${compiledData.metadata.compressionRatio}`);
        }

        return {
            outputPath,
            metadata: compiledData.metadata,
            buffer
        };
    }

    /**
     * Load and cache protobuf schema
     */
    async loadProtoSchema(protoPath) {
        if (this.schemaCache.has(protoPath)) {
            return this.schemaCache.get(protoPath);
        }

        const root = await protobuf.load(protoPath);
        this.schemaCache.set(protoPath, root);
        return root;
    }

    /**
     * Compile multiple JSON files to protobuf
     */
    async compileDirectory(jsonDir, options = {}) {
        const {
            protoDir = path.join(jsonDir, this.options.outputDir),
            outputDir = protoDir,
            filePattern = '*.json'
        } = options;

        const files = fs.readdirSync(jsonDir)
            .filter(file => file.endsWith('.json') && !file.endsWith('.pb.json'))
            .map(file => path.join(jsonDir, file));

        const results = [];

        for (const jsonFile of files) {
            const fileName = path.basename(jsonFile, '.json');
            const protoPath = path.join(protoDir, `${fileName}.proto`);
            const outputPath = path.join(outputDir, `${fileName}.pb`);

            if (fs.existsSync(protoPath)) {
                const result = await this.compileFile(jsonFile, protoPath, outputPath);
                results.push(result);
            } else {
                console.warn(`⚠️  Skipping ${jsonFile} - no corresponding .proto file found`);
            }
        }

        console.log(`✅ Compiled ${results.length} files to protobuf format`);
        return results;
    }

    /**
     * Decompile protobuf back to JSON
     */
    async decompileFile(pbPath, protoPath = null, outputPath = null) {
        const fileName = path.basename(pbPath, '.pb');

        // Determine proto path
        if (!protoPath) {
            protoPath = path.join(path.dirname(pbPath), `${fileName}.proto`);
        }

        // Determine output path
        if (!outputPath) {
            outputPath = path.join(path.dirname(pbPath), `${fileName}-decompiled.json`);
        }

        // Load protobuf schema
        const root = await this.loadProtoSchema(protoPath);
        const baseFileName = path.basename(pbPath, '.pb');
        const messageName = this.sanitizeMessageName(baseFileName);
        const MessageClass = root.lookupType(messageName); // Use sanitized message name

        // Read binary data
        const buffer = fs.readFileSync(pbPath);

        // Decode protobuf message
        const message = MessageClass.decode(buffer);

        // Convert to plain object
        const protobufData = MessageClass.toObject(message, {
            longs: String,  // Convert long numbers to strings
            enums: String,  // Convert enums to strings
            bytes: String,  // Convert bytes to base64 strings
        });

        // Map protobuf field names back to original JSON field names
        const jsonData = await this.mapProtobufToJsonFields(protobufData, pbPath);

        // Write JSON output
        fs.writeFileSync(outputPath, JSON.stringify(jsonData, null, 2));

        console.log(`✅ Decompiled ${pbPath} to ${outputPath}`);
        return jsonData;
    }

    /**
     * Validate compilation by round-trip test
     */
    async validateCompilation(jsonPath, protoPath = null) {
        const originalData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));

        // Compile to protobuf
        const compileResult = await this.compileFile(jsonPath, protoPath);

        // Decompile back to JSON
        const decompiledPath = `${compileResult.outputPath}.json`;
        const decompiledData = await this.decompileFile(
            compileResult.outputPath,
            protoPath,
            decompiledPath
        );

        // Compare original vs decompiled (accounting for field name transformations)
        const isValid = this.deepEqualWithFieldMapping(originalData, decompiledData);

        if (isValid) {
            console.log(`✅ Validation passed for ${jsonPath}`);
        } else {
            console.error(`❌ Validation failed for ${jsonPath}`);
            console.log('Original:', JSON.stringify(originalData, null, 2));
            console.log('Decompiled:', JSON.stringify(decompiledData, null, 2));
        }

        return {
            isValid,
            originalData,
            decompiledData,
            compileResult
        };
    }

    /**
     * Deep equality comparison for validation
     */
    deepEqual(a, b) {
        if (a === b) return true;

        if (a == null || b == null) return a === b;

        if (Array.isArray(a) && Array.isArray(b)) {
            if (a.length !== b.length) return false;
            for (let i = 0; i < a.length; i++) {
                if (!this.deepEqual(a[i], b[i])) return false;
            }
            return true;
        }

        if (typeof a === 'object' && typeof b === 'object') {
            const keysA = Object.keys(a);
            const keysB = Object.keys(b);

            if (keysA.length !== keysB.length) return false;

            for (const key of keysA) {
                if (!keysB.includes(key)) return false;
                if (!this.deepEqual(a[key], b[key])) return false;
            }
            return true;
        }

        return false;
    }

    /**
     * Deep equality comparison with field name mapping
     */
    deepEqualWithFieldMapping(originalData, decompiledData) {
        // Create a mapping from original field names to decompiled field names
        const fieldMapping = {};

        for (const originalKey of Object.keys(originalData)) {
            const sanitizedKey = this.sanitizeFieldName(originalKey);
            fieldMapping[sanitizedKey] = originalData[originalKey];
        }

        // Check if all decompiled fields match their original values
        for (const [decompiledKey, decompiledValue] of Object.entries(decompiledData)) {
            const originalValue = fieldMapping[decompiledKey];
            if (originalValue === undefined) {
                console.log(`❌ Extra field in decompiled data: ${decompiledKey}`);
                return false;
            }
            if (!this.deepEqual(originalValue, decompiledValue)) {
                console.log(`❌ Value mismatch for field ${decompiledKey}: expected ${originalValue}, got ${decompiledValue}`);
                return false;
            }
        }

        // Check if all original fields are present in decompiled data
        for (const originalKey of Object.keys(originalData)) {
            const sanitizedKey = this.sanitizeFieldName(originalKey);
            if (!(sanitizedKey in decompiledData)) {
                console.log(`❌ Missing field in decompiled data: ${originalKey} -> ${sanitizedKey}`);
                return false;
            }
        }

        return true;
    }

    /**
     * Clear schema cache
     */
    clearCache() {
        this.schemaCache.clear();
    }

    /**
     * Get compilation statistics
     */
    getStats() {
        return {
            cachedSchemas: this.schemaCache.size,
            options: this.options
        };
    }

    /**
     * Map JSON field names to protobuf field names
     */
    mapJsonToProtobufFields(jsonData) {
        const mapped = {};

        for (const [key, value] of Object.entries(jsonData)) {
            // Sanitize field name to match protobuf schema (camelCase)
            const sanitizedKey = this.sanitizeFieldName(key);
            mapped[sanitizedKey] = value;
        }

        return mapped;
    }

    /**
     * Map protobuf field names back to original JSON field names
     */
    async mapProtobufToJsonFields(protobufData, pbPath) {
        // Try to find the original JSON file
        const jsonPath = pbPath.replace('.pb', '.json');
        let originalFieldNames = [];

        if (fs.existsSync(jsonPath)) {
            try {
                const originalJson = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
                originalFieldNames = Object.keys(originalJson);
            } catch (error) {
                console.warn(`⚠️  Could not read original JSON file for field mapping: ${error.message}`);
            }
        }

        const mapped = {};

        for (const [sanitizedKey, value] of Object.entries(protobufData)) {
            // Try to find the original field name
            let originalKey = sanitizedKey;

            for (const origKey of originalFieldNames) {
                if (this.sanitizeFieldName(origKey) === sanitizedKey) {
                    originalKey = origKey;
                    break;
                }
            }

            mapped[originalKey] = value;
        }

        return mapped;
    }

    /**
     * Sanitize field names for protobuf (convert to camelCase)
     */
    sanitizeFieldName(name) {
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
     * Sanitize message names for protobuf
     */
    sanitizeMessageName(name) {
        let cleanName = this.sanitizeFieldName(name);

        // Convert to PascalCase and remove invalid characters
        cleanName = cleanName.replace(/[^a-zA-Z0-9_]/g, '_');
        cleanName = cleanName.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
        cleanName = cleanName.charAt(0).toUpperCase() + cleanName.slice(1);

        // Ensure it's a valid identifier (no leading numbers)
        if (/^[0-9]/.test(cleanName)) {
            cleanName = `Msg${cleanName}`;
        }

        return cleanName;
    }
}

/**
 * Convenience functions
 */
async function compileJSONToProtobuf(jsonPath, protoPath = null, outputPath = null, options = {}) {
    const compiler = new JSONToProtobufCompiler(options);
    return await compiler.compileFile(jsonPath, protoPath, outputPath);
}

async function compileDirectoryToProtobuf(jsonDir, options = {}) {
    const compiler = new JSONToProtobufCompiler(options);
    return await compiler.compileDirectory(jsonDir, options);
}

async function decompileProtobufToJSON(pbPath, protoPath = null, outputPath = null, options = {}) {
    const compiler = new JSONToProtobufCompiler(options);
    return await compiler.decompileFile(pbPath, protoPath, outputPath);
}

async function validateProtobufCompilation(jsonPath, protoPath = null, options = {}) {
    const compiler = new JSONToProtobufCompiler(options);
    return await compiler.validateCompilation(jsonPath, protoPath);
}

module.exports = {
    JSONToProtobufCompiler,
    compileJSONToProtobuf,
    compileDirectoryToProtobuf,
    decompileProtobufToJSON,
    validateProtobufCompilation
};