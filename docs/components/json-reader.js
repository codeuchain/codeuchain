const fs = require('fs');
const path = require('path');

class RuntimeJSONReader {
    constructor() {
        this.cache = new Map();
        this.baseDir = '';
    }

    /**
     * Read and process a JSON file with import/export resolution
     * @param {string} filename - The JSON file to read
     * @param {object} options - Options for reading
     * @returns {object} The processed JSON data
     */
    readFile(filename, options = {}) {
        const { resolveSharing = true, cache = true } = options;

        // Create cache key
        const cacheKey = path.resolve(this.baseDir, filename);

        // Return cached result if available
        if (cache && this.cache.has(cacheKey)) {
            return this.cache.get(cacheKey);
        }

        // Read the JSON file
        const filePath = path.join(this.baseDir, filename);
        if (!fs.existsSync(filePath)) {
            throw new Error(`JSON file not found: ${filePath}`);
        }

        const content = fs.readFileSync(filePath, 'utf8');
        let data;

        try {
            data = JSON.parse(content);
        } catch (error) {
            throw new Error(`Invalid JSON in ${filePath}: ${error.message}`);
        }

        // Process imports if resolveSharing is enabled
        if (resolveSharing) {
            data = this.resolveImports(data, path.dirname(filePath));
        }

        // Cache the result
        if (cache) {
            this.cache.set(cacheKey, data);
        }

        return data;
    }

    /**
     * Resolve import statements in JSON data
     * @param {object} data - The JSON data to process
     * @param {string} currentDir - The directory containing the current file
     * @returns {object} The data with imports resolved
     */
    resolveImports(data, currentDir) {
        const result = { ...data };

        // Process each property in the data
        for (const [key, value] of Object.entries(result)) {
            if (typeof value === 'string' && value.startsWith('import://')) {
                // Handle import syntax: "import://filename.json:property"
                const importMatch = value.match(/^import:\/\/([^:]+):(.+)$/);
                if (importMatch) {
                    const [_, importFile, importProperty] = importMatch;

                    try {
                        // Read the imported file
                        const importedData = this.readFile(importFile, {
                            resolveSharing: true,
                            cache: true
                        });

                        // Extract the exported property
                        const exportKey = `export://${importProperty}`;
                        if (importedData.hasOwnProperty(exportKey)) {
                            result[key] = importedData[exportKey];
                        } else if (importedData.hasOwnProperty(importProperty)) {
                            // Fallback to direct property access
                            result[key] = importedData[importProperty];
                        } else {
                            console.warn(`Export not found: ${exportKey} in ${importFile}`);
                            result[key] = null;
                        }
                    } catch (error) {
                        console.warn(`Failed to import ${value}: ${error.message}`);
                        result[key] = null;
                    }
                }
            } else if (typeof value === 'object' && value !== null) {
                // Recursively process nested objects
                result[key] = this.resolveImports(value, currentDir);
            }
        }

        return result;
    }

    /**
     * Clear the cache
     */
    clearCache() {
        this.cache.clear();
    }
}

module.exports = { RuntimeJSONReader };