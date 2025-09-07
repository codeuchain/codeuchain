/**
 * Protobuf Runtime Loader
 *
 * This module provides efficient r            // Load protobuf schema
            const root = await this.loadSchema(protoPath);

            // Determine message type name
            const fileName = path.basename(pbPath, '.pb');
            const messageName = this.sanitizeMessageName(fileName);
            console.log(`🔍 Looking for message type: ${messageName} in ${protoPath}`);

            // Try lookup with package prefix first
            let MessageClass;
            try {
                MessageClass = root.lookupType(`codeuchain.${messageName}`);
                console.log(`✅ Found message class with package prefix: ${MessageClass?.name}`);
            } catch (e) {
                console.log(`⚠️  Package prefix lookup failed, trying direct lookup`);
                MessageClass = root.lookupType(messageName);
                console.log(`✅ Found message class: ${MessageClass?.name}`);
            } loading of Protocol Buffer files
 * with automatic conversion back to JSON format for API compatibility.
 * It includes caching, lazy loading, and integration with the hybrid reader system.
 */

const fs = require('fs');
const path = require('path');
const protobuf = require('protobufjs');
const { JSONToProtobufCompiler } = require('./json-to-protobuf-compiler');

class ProtobufRuntimeLoader {
    constructor(options = {}) {
        this.options = {
            baseDir: options.baseDir || '.',
            cacheEnabled: options.cacheEnabled !== false,
            preloadSchemas: options.preloadSchemas || false,
            fallbackToJson: options.fallbackToJson !== false,
            ...options
        };

        // Cache for loaded protobuf schemas
        this.schemaCache = new Map();

        // Cache for decoded protobuf messages
        this.messageCache = new Map();

        // Track loading statistics
        this.stats = {
            schemasLoaded: 0,
            messagesDecoded: 0,
            cacheHits: 0,
            cacheMisses: 0,
            loadTime: 0
        };

        // Initialize compiler for fallback operations
        this.compiler = new JSONToProtobufCompiler(this.options);

        // Preload schemas if requested
        if (this.options.preloadSchemas) {
            this.preloadSchemas();
        }
    }

    /**
     * Load a protobuf file and convert to JSON
     */
    async loadFile(pbPath, protoPath = null, options = {}) {
        const startTime = Date.now();
        const cacheKey = `${pbPath}:${protoPath || 'auto'}`;

        // Check cache first
        if (this.options.cacheEnabled && this.messageCache.has(cacheKey)) {
            this.stats.cacheHits++;
            return this.messageCache.get(cacheKey);
        }

        this.stats.cacheMisses++;

        try {
            // Determine proto path
            if (!protoPath) {
                const fileName = path.basename(pbPath, '.pb');
                protoPath = path.join(path.dirname(pbPath), `${fileName}.proto`);
            }

            // Load protobuf schema
            const root = await this.loadSchema(protoPath);

            // Determine message type name
            const fileName = path.basename(pbPath, '.pb');
            const sanitizedName = this.sanitizeMessageName(fileName);
            const packageName = 'codeuchain'; // From the .proto file package declaration
            const fullTypeName = `${packageName}.${sanitizedName}`;

            console.log(`🔍 Looking up message type: ${fullTypeName}`);
            const MessageClass = root.lookupType(fullTypeName);
            console.log(`📋 Message class found:`, !!MessageClass);

            if (!MessageClass) {
                throw new Error(`Message type ${fullTypeName} not found in schema`);
            }

            // Read binary data
            const buffer = fs.readFileSync(pbPath);
            console.log(`📦 Read ${buffer.length} bytes from ${pbPath}`);

            // Decode protobuf message
            const message = MessageClass.decode(buffer);
            console.log(`🔍 Decoded message:`, message);
            console.log(`🔍 Message type:`, typeof message);
            console.log(`🔍 Message constructor:`, message.constructor.name);

            // Convert to JSON
            const jsonData = MessageClass.toObject(message, {
                longs: String,
                enums: String,
                bytes: String,
            });
            console.log(`📋 Converted to object:`, jsonData);
            console.log(`📋 Object type:`, typeof jsonData);
            console.log(`📋 Object keys:`, Object.keys(jsonData));

            console.log(`📋 Final result before return:`, jsonData);
            console.log(`📋 Final result type:`, typeof jsonData);

            // Cache the result if caching is enabled
            if (this.options.cacheEnabled) {
                this.messageCache.set(cacheKey, jsonData);
            }

            this.stats.messagesDecoded++;
            this.stats.loadTime += (Date.now() - startTime);

            return jsonData;

        } catch (error) {
            if (this.options.fallbackToJson) {
                console.warn(`⚠️  Failed to load protobuf ${pbPath}, attempting JSON fallback:`, error.message);
                return await this.loadJsonFallback(pbPath, options);
            }
            throw error;
        }
    }

    /**
     * Load protobuf schema with caching
     */
    async loadSchema(protoPath) {
        if (this.schemaCache.has(protoPath)) {
            console.log(`📋 Using cached schema for ${protoPath}`);
            return this.schemaCache.get(protoPath);
        }

        console.log(`📋 Loading schema from ${protoPath}`);
        const root = await protobuf.load(protoPath);
        console.log(`📋 Schema loaded, available types:`, Object.keys(root.nested?.codeuchain?.nested || {}));
        console.log(`📋 Full schema structure:`, JSON.stringify(root.toJSON(), null, 2));

        this.schemaCache.set(protoPath, root);
        this.stats.schemasLoaded++;

        return root;
    }

    /**
     * Load JSON fallback when protobuf fails
     */
    async loadJsonFallback(pbPath, options = {}) {
        const jsonPath = pbPath.replace('.pb', '.json');

        if (fs.existsSync(jsonPath)) {
            const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
            console.log(`✅ Loaded JSON fallback: ${jsonPath}`);
            return jsonData;
        }

        // Try to find original JSON file
        const originalJsonPath = path.join(this.options.baseDir, path.basename(pbPath, '.pb') + '.json');
        if (fs.existsSync(originalJsonPath)) {
            const jsonData = JSON.parse(fs.readFileSync(originalJsonPath, 'utf8'));
            console.log(`✅ Loaded original JSON: ${originalJsonPath}`);
            return jsonData;
        }

        throw new Error(`No fallback available for ${pbPath}`);
    }

    /**
     * Map protobuf field names back to original JSON field names
     */
    async mapProtobufToOriginalFields(protobufData, pbPath) {
        // Try to find the original JSON file to get field mappings
        const jsonPath = pbPath.replace('.pb', '.json');

        if (fs.existsSync(jsonPath)) {
            try {
                return await this.compiler.mapProtobufToJsonFields(protobufData, pbPath);
            } catch (error) {
                console.warn(`⚠️  Field mapping failed, falling back to camelCase conversion: ${error.message}`);
            }
        }

        // Fallback: Convert camelCase to snake_case
        console.log(`🔄 Using fallback field name mapping for ${pbPath}`);
        const mapped = {};

        for (const [camelKey, value] of Object.entries(protobufData)) {
            // Convert camelCase to snake_case
            const snakeKey = camelKey.replace(/([A-Z])/g, '_$1').toLowerCase();
            mapped[snakeKey] = value;
        }

        return mapped;
    }

    /**
     * Load multiple protobuf files
     */
    async loadFiles(pbPaths, options = {}) {
        const results = {};
        const startTime = Date.now();

        for (const pbPath of pbPaths) {
            const fileName = path.basename(pbPath, '.pb');
            try {
                results[fileName] = await this.loadFile(pbPath, null, options);
            } catch (error) {
                console.error(`❌ Failed to load ${pbPath}:`, error.message);
                if (!options.ignoreErrors) {
                    throw error;
                }
            }
        }

        this.stats.loadTime += (Date.now() - startTime);
        return results;
    }

    /**
     * Preload all available schemas
     */
    async preloadSchemas() {
        const protoDir = path.join(this.options.baseDir, 'protobuf');

        if (!fs.existsSync(protoDir)) {
            return;
        }

        const protoFiles = fs.readdirSync(protoDir)
            .filter(file => file.endsWith('.proto'))
            .map(file => path.join(protoDir, file));

        console.log(`📋 Preloading ${protoFiles.length} protobuf schemas...`);

        for (const protoFile of protoFiles) {
            try {
                await this.loadSchema(protoFile);
            } catch (error) {
                console.warn(`⚠️  Failed to preload schema ${protoFile}:`, error.message);
            }
        }

        console.log(`✅ Preloaded ${this.schemaCache.size} schemas`);
    }

    /**
     * Sanitize message names for protobuf
     */
    sanitizeMessageName(name) {
        let cleanName = name.replace(/[^a-zA-Z0-9_]/g, '_');
        cleanName = cleanName.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
        cleanName = cleanName.charAt(0).toUpperCase() + cleanName.slice(1);

        // Ensure it's a valid identifier (no leading numbers)
        if (/^[0-9]/.test(cleanName)) {
            cleanName = `Msg${cleanName}`;
        }

        return cleanName;
    }

    /**
     * Check if a protobuf file exists and is valid
     */
    async validateFile(pbPath, protoPath = null) {
        try {
            await this.loadFile(pbPath, protoPath, { preserveFieldNames: false });
            return { valid: true, error: null };
        } catch (error) {
            return { valid: false, error: error.message };
        }
    }

    /**
     * Get file metadata without full loading
     */
    getFileMetadata(pbPath) {
        const stats = fs.statSync(pbPath);
        const fileName = path.basename(pbPath, '.pb');

        return {
            path: pbPath,
            name: fileName,
            size: stats.size,
            modified: stats.mtime,
            protoPath: path.join(path.dirname(pbPath), `${fileName}.proto`),
            jsonFallbackPath: path.join(path.dirname(pbPath), `${fileName}.json`)
        };
    }

    /**
     * Clear all caches
     */
    clearCache() {
        this.schemaCache.clear();
        this.messageCache.clear();
        this.stats.cacheHits = 0;
        this.stats.cacheMisses = 0;
    }

    /**
     * Get loader statistics
     */
    getStats() {
        const cacheHitRate = this.stats.cacheHits + this.stats.cacheMisses > 0
            ? (this.stats.cacheHits / (this.stats.cacheHits + this.stats.cacheMisses) * 100).toFixed(1)
            : 0;

        return {
            ...this.stats,
            cacheHitRate: `${cacheHitRate}%`,
            schemasCached: this.schemaCache.size,
            messagesCached: this.messageCache.size,
            averageLoadTime: this.stats.messagesDecoded > 0
                ? `${(this.stats.loadTime / this.stats.messagesDecoded).toFixed(2)}ms`
                : '0ms'
        };
    }

    /**
     * Warm up cache by preloading frequently used files
     */
    async warmupCache(pbPaths) {
        console.log(`🔥 Warming up cache with ${pbPaths.length} files...`);

        for (const pbPath of pbPaths) {
            try {
                await this.loadFile(pbPath);
            } catch (error) {
                console.warn(`⚠️  Failed to warmup ${pbPath}:`, error.message);
            }
        }

        console.log(`✅ Cache warmup complete. ${this.messageCache.size} files cached.`);
    }
}

/**
 * Convenience functions
 */
async function loadProtobufFile(pbPath, protoPath = null, options = {}) {
    const loader = new ProtobufRuntimeLoader(options);
    return await loader.loadFile(pbPath, protoPath, options);
}

async function loadProtobufFiles(pbPaths, options = {}) {
    const loader = new ProtobufRuntimeLoader(options);
    return await loader.loadFiles(pbPaths, options);
}

async function validateProtobufFile(pbPath, protoPath = null, options = {}) {
    const loader = new ProtobufRuntimeLoader(options);
    return await loader.validateFile(pbPath, protoPath);
}

module.exports = {
    ProtobufRuntimeLoader,
    loadProtobufFile,
    loadProtobufFiles,
    validateProtobufFile
};