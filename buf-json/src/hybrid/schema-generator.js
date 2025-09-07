/**
 * Protobuf Schema Generator
 *
 * This module analyzes JSON configuration files and automatically generates
 * Protocol Buffer (.proto) schema files that can be used for efficient
 * binary serialization while maintaining API compatibility.
 */

const fs = require('fs');
const path = require('path');

class ProtobufSchemaGenerator {
    constructor(options = {}) {
        this.options = {
            packageName: options.packageName || 'codeuchain',
            syntax: options.syntax || 'proto3',
            generateTimestamps: options.generateTimestamps !== false,
            includeComments: options.includeComments !== false,
            ...options
        };

        // Track generated message types to avoid duplicates
        this.generatedMessages = new Set();
        this.messageCounter = 1;

        // Type mappings from JavaScript to Protobuf
        this.typeMappings = {
            string: 'string',
            number: 'double', // Use double for flexibility
            boolean: 'bool',
            object: 'message', // Will generate nested message
            array: 'repeated' // Will determine element type
        };
    }

    /**
     * Generate protobuf schema from JSON data object
     */
    async generateFromData(data, outputPath, options = {}) {
        const messageName = options.messageName || 'Config';
        const packageName = options.package || this.options.packageName;

        // Ensure output directory exists
        const outputDir = path.dirname(outputPath);
        if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true });
        }

        // Temporarily update package name if specified
        const originalPackage = this.options.packageName;
        if (packageName !== originalPackage) {
            this.options.packageName = packageName;
        }

        const schema = this.generateSchema(data, messageName);
        fs.writeFileSync(outputPath, schema);

        // Restore original package name
        if (packageName !== originalPackage) {
            this.options.packageName = originalPackage;
        }

        console.log(`✅ Generated protobuf schema from data: ${outputPath}`);
        return outputPath;
    }
    async generateFromFile(jsonPath, outputPath = null) {
        const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
        const fileName = path.basename(jsonPath, '.json');
        const messageName = this.sanitizeMessageName(fileName);

        if (!outputPath) {
            outputPath = path.join(path.dirname(jsonPath), `${fileName}.proto`);
        }

        // Ensure output directory exists
        const outputDir = path.dirname(outputPath);
        if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true });
        }

        const schema = this.generateSchema(jsonData, messageName);
        fs.writeFileSync(outputPath, schema);

        console.log(`✅ Generated protobuf schema: ${outputPath}`);
        return outputPath;
    }

    /**
     * Generate protobuf schema from JSON data
     */
    generateSchema(jsonData, rootMessageName = 'Config') {
        this.generatedMessages.clear();
        this.messageCounter = 1;

        const messages = [];
        const rootMessage = this.generateMessage(jsonData, rootMessageName, messages);

        // Build the complete proto file
        let protoContent = `// Auto-generated Protocol Buffer schema\n`;
        if (this.options.generateTimestamps) {
            protoContent += `// Generated at: ${new Date().toISOString()}\n`;
        }
        protoContent += `// Source: JSON configuration structure\n\n`;

        protoContent += `syntax = "${this.options.syntax}";\n\n`;
        protoContent += `package ${this.options.packageName};\n\n`;

        // Add all messages
        protoContent += messages.join('\n\n');

        return protoContent;
    }

    /**
     * Generate a protobuf message from JSON object
     */
    generateMessage(obj, messageName, messages) {
        if (this.generatedMessages.has(messageName)) {
            return messageName; // Already generated
        }

        this.generatedMessages.add(messageName);

        let message = `message ${messageName} {\n`;
        let fieldNumber = 1;

        for (const [key, value] of Object.entries(obj)) {
            const fieldType = this.getFieldType(value, key, messages);
            const fieldName = this.sanitizeFieldName(key);

            if (this.options.includeComments && typeof value === 'string' && value.length < 50) {
                message += `  // ${value}\n`;
            }

            message += `  ${fieldType} ${fieldName} = ${fieldNumber};\n`;
            fieldNumber++;
        }

        message += `}`;

        messages.push(message);
        return messageName;
    }

    /**
     * Determine the protobuf field type for a value
     */
    getFieldType(value, key, messages) {
        const jsType = typeof value;

        if (value === null) {
            return 'string'; // Nullable fields as optional strings
        }

        switch (jsType) {
            case 'string':
                // Check for import/export markers
                if (value.startsWith('import://') || value.startsWith('export://')) {
                    return 'string'; // Keep as string for now
                }
                return 'string';

            case 'number':
                return Number.isInteger(value) ? 'int64' : 'double';

            case 'boolean':
                return 'bool';

            case 'object':
                if (Array.isArray(value)) {
                    if (value.length === 0) {
                        return 'repeated string'; // Default for empty arrays
                    }
                    const elementType = this.getFieldType(value[0], `${key}_item`, messages);
                    return `repeated ${elementType}`;
                } else {
                    // Nested object - generate a new message
                    const nestedMessageName = this.generateNestedMessageName(key);
                    this.generateMessage(value, nestedMessageName, messages);
                    return nestedMessageName;
                }

            default:
                return 'string'; // Fallback
        }
    }

    /**
     * Generate a unique name for nested messages
     */
    generateNestedMessageName(baseName) {
        const sanitized = this.sanitizeMessageName(baseName);
        let candidate = sanitized;

        while (this.generatedMessages.has(candidate)) {
            candidate = `${sanitized}${this.messageCounter}`;
            this.messageCounter++;
        }

        return candidate;
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

    /**
     * Generate schema for multiple files
     */
    async generateFromDirectory(dirPath, outputDir = null) {
        if (!outputDir) {
            outputDir = path.join(dirPath, 'protobuf');
        }

        if (!fs.existsSync(outputDir)) {
            fs.mkdirSync(outputDir, { recursive: true });
        }

        const files = fs.readdirSync(dirPath)
            .filter(file => file.endsWith('.json'))
            .map(file => path.join(dirPath, file));

        const generatedFiles = [];

        for (const file of files) {
            const outputPath = path.join(outputDir, `${path.basename(file, '.json')}.proto`);
            await this.generateFromFile(file, outputPath);
            generatedFiles.push(outputPath);
        }

        console.log(`✅ Generated ${generatedFiles.length} protobuf schemas in ${outputDir}`);
        return generatedFiles;
    }

    /**
     * Generate a unified schema for multiple JSON files
     */
    async generateUnifiedSchema(jsonFiles, outputPath, rootMessageName = 'UnifiedConfig') {
        const allData = {};

        // Load all JSON files
        for (const file of jsonFiles) {
            const data = JSON.parse(fs.readFileSync(file, 'utf8'));
            const fileKey = path.basename(file, '.json');
            allData[fileKey] = data;
        }

        // Generate unified schema
        const schema = this.generateSchema(allData, rootMessageName);
        fs.writeFileSync(outputPath, schema);

        console.log(`✅ Generated unified protobuf schema: ${outputPath}`);
        return outputPath;
    }
}

/**
 * Convenience functions
 */
async function generateProtobufSchema(jsonPath, outputPath = null, options = {}) {
    const generator = new ProtobufSchemaGenerator(options);
    return await generator.generateFromFile(jsonPath, outputPath);
}

async function generateProtobufSchemasFromDirectory(dirPath, outputDir = null, options = {}) {
    const generator = new ProtobufSchemaGenerator(options);
    return await generator.generateFromDirectory(dirPath, outputDir);
}

async function generateUnifiedProtobufSchema(jsonFiles, outputPath, options = {}) {
    const generator = new ProtobufSchemaGenerator(options);
    return await generator.generateUnifiedSchema(jsonFiles, outputPath, options.rootMessageName);
}

module.exports = {
    ProtobufSchemaGenerator,
    generateProtobufSchema,
    generateProtobufSchemasFromDirectory,
    generateUnifiedProtobufSchema
};