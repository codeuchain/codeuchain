#!/usr/bin/env node

/**
 * JSON Value Sharer - A simple script for sharing values between JSON files
 * Supports special markers for import/export without affecting normal JSON parsing
 *
 * Usage:
 *   node json-sharer.js <input-file> [output-file]
 *
 * Special Markers:
 *   "<<key>>": Export value for sharing with other files
 *   ">>key<<": Import value from another file's export
 *   "<<file:key>>": Import value from specific file's export
 */

const fs = require('fs');
const path = require('path');

class JSONValueSharer {
    constructor() {
        this.exports = new Map();
        this.processedFiles = new Set();
    }

    /**
     * Process a JSON file with sharing capabilities
     */
    async processFile(inputPath, outputPath = null) {
        const absoluteInputPath = path.resolve(inputPath);
        const inputDir = path.dirname(absoluteInputPath);

        console.log(`🔄 Processing: ${inputPath}`);

        // Read and parse the input JSON
        const rawContent = fs.readFileSync(absoluteInputPath, 'utf8');
        let data;

        try {
            data = JSON.parse(rawContent);
        } catch (error) {
            throw new Error(`Invalid JSON in ${inputPath}: ${error.message}`);
        }

        // Process exports first
        const processedData = await this.processData(data, inputDir, absoluteInputPath);

        // Write the processed JSON
        const outputFile = outputPath || inputPath;
        const outputContent = JSON.stringify(processedData, null, 2);

        fs.writeFileSync(outputFile, outputContent);
        console.log(`✅ Processed: ${outputFile}`);
    }

    /**
     * Process the data object, handling imports and exports
     */
    async processData(data, baseDir, currentFile) {
        const result = {};

        for (const [key, value] of Object.entries(data)) {
            if (this.isExportKey(key)) {
                // Handle export
                const exportKey = this.extractExportKey(key);
                this.exports.set(exportKey, value);
                console.log(`📤 Exported: ${exportKey} = ${JSON.stringify(value)}`);

                // Remove export marker from final output
                result[this.cleanKey(key)] = value;

            } else if (this.isImportKey(key)) {
                // Handle import
                const importSpec = this.extractImportKey(key);
                const importedValue = await this.resolveImport(importSpec, baseDir, currentFile);

                console.log(`📥 Imported: ${key} -> ${JSON.stringify(importedValue)}`);
                result[this.cleanKey(key)] = importedValue;

            } else if (this.isFileImportKey(key)) {
                // Handle file-specific import
                const fileImport = this.extractFileImportKey(key);
                if (!fileImport) {
                    throw new Error(`Invalid file import syntax: ${key}`);
                }
                const { file, key: importKey } = fileImport;
                const importedValue = await this.resolveFileImport(file, importKey, baseDir, currentFile);

                console.log(`📁 File imported: ${file}:${importKey} -> ${JSON.stringify(importedValue)}`);
                result[this.cleanKey(key)] = importedValue;

            } else {
                // Regular key-value pair
                if (typeof value === 'object' && value !== null) {
                    result[key] = await this.processData(value, baseDir, currentFile);
                } else {
                    result[key] = value;
                }
            }
        }

        return result;
    }

    /**
     * Check if a key is an export marker
     */
    isExportKey(key) {
        return /^<<[^>]+>>/.test(key) && !key.includes(':');
    }

    /**
     * Check if a key is an import marker
     */
    isImportKey(key) {
        return /^>>[^<]+<</.test(key);
    }

    /**
     * Check if a key is a file-specific import marker
     */
    isFileImportKey(key) {
        return /^<<[^:]+:[^>]+>>/.test(key);
    }

    /**
     * Extract the export key from a marked key
     */
    extractExportKey(key) {
        const match = key.match(/^<<([^>]+)>>/);
        return match ? match[1] : null;
    }

    /**
     * Extract the import key from a marked key
     */
    extractImportKey(key) {
        const match = key.match(/^>>([^<]+)<</);
        return match ? match[1] : null;
    }

    /**
     * Extract file and key from file-specific import
     */
    extractFileImportKey(key) {
        const match = key.match(/^<<([^:]+):([^>]+)>>/);
        if (match) {
            return { file: match[1], key: match[2] };
        }
        return null;
    }

    /**
     * Clean the key by removing markers
     */
    cleanKey(key) {
        // Handle file-specific imports first (most specific)
        if (key.startsWith('<<') && key.includes(':')) {
            const match = key.match(/^<<[^:]+:([^>]+)>>$/);
            return match ? match[1] : key;
        }
        // Handle regular exports
        if (key.startsWith('<<')) {
            const match = key.match(/^<<([^>]+)>>$/);
            return match ? match[1] : key;
        }
        // Handle imports
        if (key.startsWith('>>')) {
            const match = key.match(/^>>([^<]+)<<$/);
            return match ? match[1] : key;
        }
        return key;
    }

    /**
     * Resolve an import by finding the exported value
     */
    async resolveImport(importKey, baseDir, currentFile) {
        // First check if we already have it in our exports
        if (this.exports.has(importKey)) {
            return this.exports.get(importKey);
        }

        // Look for the value in other JSON files in the same directory
        const files = fs.readdirSync(baseDir).filter(file =>
            file.endsWith('.json') && path.resolve(baseDir, file) !== currentFile
        );

        for (const file of files) {
            const filePath = path.resolve(baseDir, file);
            if (this.processedFiles.has(filePath)) continue;

            try {
                const fileContent = fs.readFileSync(filePath, 'utf8');
                const fileData = JSON.parse(fileContent);

                // Process the file to extract exports
                await this.processData(fileData, baseDir, filePath);

                // Check if our import key is now available
                if (this.exports.has(importKey)) {
                    return this.exports.get(importKey);
                }
            } catch (error) {
                console.warn(`⚠️  Skipping ${file}: ${error.message}`);
            }
        }

        throw new Error(`Import key "${importKey}" not found in any JSON file`);
    }

    /**
     * Resolve a file-specific import
     */
    async resolveFileImport(fileName, importKey, baseDir, currentFile) {
        const filePath = path.resolve(baseDir, fileName);
        if (!fs.existsSync(filePath)) {
            throw new Error(`Import file "${fileName}" not found`);
        }

        try {
            const fileContent = fs.readFileSync(filePath, 'utf8');
            const fileData = JSON.parse(fileContent);

            // Look for the export in this specific file
            for (const [key, value] of Object.entries(fileData)) {
                if (this.isExportKey(key)) {
                    const exportKey = this.extractExportKey(key);
                    if (exportKey === importKey) {
                        return value;
                    }
                }
            }

            throw new Error(`Export key "${importKey}" not found in ${fileName}`);
        } catch (error) {
            if (error.message.includes('not found in')) throw error;
            throw new Error(`Error reading ${fileName}: ${error.message}`);
        }
    }
}

/**
 * Main CLI function
 */
async function main() {
    const args = process.argv.slice(2);

    if (args.length === 0) {
        console.log(`
🔗 JSON Value Sharer

Usage:
  node json-sharer.js <input-file> [output-file]

Special Markers:
  "<<key>>": Export value for sharing
  ">>key<<": Import value from any file's export
  "<<filename:key>>": Import value from specific file's export

Examples:
  node json-sharer.js data.json
  node json-sharer.js input.json output.json
        `);
        process.exit(1);
    }

    const [inputFile, outputFile] = args;
    const sharer = new JSONValueSharer();

    try {
        await sharer.processFile(inputFile, outputFile);
        console.log('\n🎉 JSON sharing complete!');
    } catch (error) {
        console.error(`❌ Error: ${error.message}`);
        process.exit(1);
    }
}

// Run if called directly
if (require.main === module) {
    main();
}

module.exports = JSONValueSharer;