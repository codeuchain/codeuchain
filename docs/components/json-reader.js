const fs = require('fs');
const path = require('path');

/**
 * Runtime JSON Reader with Value Sharing Support
 *
 * This module allows reading JSON files with special URI-style sharing markers:
 * - "export://key": Export a value for sharing.
 * - "import://key": Import a value from any file in the directory.
 * - "import://filename.json:key": Import a value from a specific file.
 *
 * Normal JSON parsers can still read these files; the markers are just treated as regular keys.
 * This reader resolves the sharing at runtime for enhanced functionality.
 */
class RuntimeJSONReader {
    constructor(baseDir = '.') {
        this.baseDir = baseDir;
        this.exports = new Map();
        this.processedFiles = new Set();
        this.cache = new Map();
        this.exportsCache = new Map(); // Cache for all exported values
        this.exportsCacheInitialized = false;
    }

    /**
     * Pre-populate exports cache by scanning all JSON files.
     */
    async initializeExportsCache(baseDir) {
        if (this.exportsCacheInitialized) {
            return;
        }

        console.log('🔄 Initializing exports cache...');
        const files = fs.readdirSync(baseDir).filter(file => file.endsWith('.json'));

        for (const file of files) {
            const filePath = path.resolve(baseDir, file);
            try {
                const content = fs.readFileSync(filePath, 'utf8');
                const data = JSON.parse(content);
                await this.extractExportsFromData(data, baseDir, filePath);
            } catch (error) {
                console.warn(`⚠️  Skipping ${file} during cache initialization: ${error.message}`);
            }
        }

        this.exportsCacheInitialized = true;
        console.log(`✅ Exports cache initialized with ${this.exportsCache.size} values`);
    }

    /**
     * Extract exports from data recursively.
     */
    async extractExportsFromData(data, baseDir, currentFile) {
        for (const [key, value] of Object.entries(data)) {
            if (this.isExportKey(key)) {
                const exportKey = this.extractExportKey(key);
                const exportValue = typeof value === 'object' && value !== null
                    ? await this.extractExportsFromData(value, baseDir, currentFile)
                    : value;

                this.exportsCache.set(exportKey, {
                    value: exportValue,
                    sourceFile: path.basename(currentFile),
                    sourcePath: currentFile
                });
            } else if (typeof value === 'object' && value !== null) {
                await this.extractExportsFromData(value, baseDir, currentFile);
            }
        }
        return data;
    }

    async readFile(filePath, options = {}) {
        const { resolveSharing = true, cache = true } = options;
        const absolutePath = path.resolve(this.baseDir, filePath);

        if (cache && this.cache.has(absolutePath)) {
            return this.cache.get(absolutePath);
        }

        if (resolveSharing && !this.exportsCacheInitialized) {
            await this.initializeExportsCache(path.dirname(absolutePath));
        }

        try {
            const content = fs.readFileSync(absolutePath, 'utf8');
            const data = JSON.parse(content);

            const result = resolveSharing
                ? await this.resolveSharing(data, path.dirname(absolutePath), absolutePath)
                : data;

            if (cache) {
                this.cache.set(absolutePath, result);
            }

            return result;
        } catch (error) {
            throw new Error(`Error reading ${filePath}: ${error.message}`);
        }
    }

    /**
     * Resolve sharing markers in data.
     */
    async resolveSharing(data, baseDir, currentFile) {
        const result = {};

        for (const [key, value] of Object.entries(data)) {
            if (this.isExportKey(key)) {
                // Exports are pre-cached and don't need to be in the final output.
                continue;
            } else if (this.isImportKey(key)) {
                const importInfo = this.parseImportKey(key);
                if (!importInfo) {
                    throw new Error(`Invalid import syntax: ${key}`);
                }

                const { file, key: importKey } = importInfo;
                const importedValue = file
                    ? await this.resolveFileImport(file, importKey, baseDir)
                    : await this.resolveImport(importKey);

                result[importKey] = importedValue;
            } else {
                if (typeof value === 'object' && value !== null) {
                    result[key] = await this.resolveSharing(value, baseDir, currentFile);
                } else {
                    result[key] = value;
                }
            }
        }

        return result;
    }

    /**
     * Check if a key is an export marker.
     */
    isExportKey(key) {
        return key.startsWith('export://');
    }

    /**
     * Check if a key is an import marker.
     */
    isImportKey(key) {
        return key.startsWith('import://');
    }

    /**
     * Extract the export key from a marked key.
     */
    extractExportKey(key) {
        return key.substring('export://'.length);
    }

    /**
     * Parse an import URI into its file and key components.
     */
    parseImportKey(key) {
        const importPath = key.substring('import://'.length);
        const parts = importPath.split(':');
        if (parts.length === 2) {
            // Handles "import://file.json:key"
            return { file: parts[0], key: parts[1] };
        } else if (parts.length === 1) {
            // Handles "import://key"
            return { file: null, key: parts[0] };
        }
        return null; // Invalid format
    }

    /**
     * Resolve an import by finding the exported value in the cache.
     */
    async resolveImport(importKey) {
        if (this.exportsCache.has(importKey)) {
            const cachedExport = this.exportsCache.get(importKey);
            return cachedExport.value;
        }
        throw new Error(`Import key "${importKey}" not found in any JSON file.`);
    }

    /**
     * Resolve a file-specific import from the cache.
     */
    async resolveFileImport(fileName, importKey, baseDir) {
        if (this.exportsCache.has(importKey)) {
            const cachedExport = this.exportsCache.get(importKey);
            if (cachedExport.sourceFile === fileName) {
                return cachedExport.value;
            }
        }

        // This fallback should ideally not be hit if the cache is complete.
        const filePath = path.resolve(baseDir, fileName);
        if (!fs.existsSync(filePath)) {
            throw new Error(`Import file "${fileName}" not found.`);
        }
        // Force a re-read of the specific file if not found in cache, just in case.
        const reader = new RuntimeJSONReader(baseDir);
        const data = await reader.readFile(fileName, { resolveSharing: false });
        for(const [key, value] of Object.entries(data)) {
            if(this.isExportKey(key) && this.extractExportKey(key) === importKey) {
                return value;
            }
        }

        throw new Error(`Export key "${importKey}" not found in ${fileName}.`);
    }

    /**
     * Clear all caches.
     */
    clearCache() {
        this.cache.clear();
        this.exports.clear();
        this.processedFiles.clear();
        this.exportsCache.clear();
        this.exportsCacheInitialized = false;
    }

    /**
     * Read file without resolving sharing (normal JSON parsing).
     */
    readFileRaw(filePath) {
        return this.readFile(filePath, { resolveSharing: false, cache: false });
    }
}

/**
 * Convenience function to read a JSON file with sharing support.
 */
async function readJSONWithSharing(filePath, baseDir = '.', options = {}) {
    const reader = new RuntimeJSONReader(baseDir);
    return await reader.readFile(filePath, options);
}

/**
 * Convenience function to read a JSON file without sharing (raw).
 */
function readJSONRaw(filePath, baseDir = '.') {
    const reader = new RuntimeJSONReader(baseDir);
    return reader.readFileRaw(filePath);
}

module.exports = {
    RuntimeJSONReader,
    readJSONWithSharing,
    readJSONRaw
};

// CLI usage
if (require.main === module) {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        console.log(`
🔗 Runtime JSON Reader with Sharing Support

Usage:
  node json-reader.js <file> [options]

Options:
  --raw          Read without resolving sharing markers
  --no-cache     Disable caching
  --base-dir <dir>  Base directory for relative paths

Examples:
  node json-reader.js data/test.json
  node json-reader.js data/test.json --raw
  node json-reader.js data/test.json --base-dir /path/to/dir
        `);
        process.exit(1);
    }

    const [filePath, ...flags] = args;
    const options = {};

    flags.forEach(flag => {
        if (flag === '--raw') {
            options.resolveSharing = false;
        } else if (flag === '--no-cache') {
            options.cache = false;
        } else if (flag.startsWith('--base-dir=')) {
            options.baseDir = flag.split('=')[1];
        }
    });

    const baseDir = options.baseDir || path.dirname(filePath);
    delete options.baseDir;

    readJSONWithSharing(path.basename(filePath), baseDir, options)
        .then(result => {
            console.log(JSON.stringify(result, null, 2));
        })
        .catch(error => {
            console.error(`❌ Error: ${error.message}`);
            process.exit(1);
        });
}