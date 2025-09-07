#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { RuntimeJSONReader } = require('./json-reader');

// Create a shared reader instance for the build process
const sharedReader = new RuntimeJSONReader();

/**
 * Simple template engine for CodeUChain documentation
 * Populates HTML templates with language-specific data
 */

function loadTemplate(templatePath) {
    return fs.readFileSync(templatePath, 'utf8');
}

async function loadData(dataPath) {
    // Use the shared reader instance to resolve sharing markers
    const dataDir = path.dirname(dataPath);
    const dataFile = path.basename(dataPath);
    // Set the base directory for the shared reader
    sharedReader.baseDir = dataDir;
    return await sharedReader.readFile(dataFile, { resolveSharing: true, cache: true });
}

function loadComponent(componentPath) {
    return fs.readFileSync(componentPath, 'utf8');
}

function populateTemplate(template, data) {
    let result = template;

    // Replace simple variables first
    Object.keys(data).forEach(key => {
        // Handle both direct keys and import-resolved keys
        let templateKey = key.toUpperCase();

        // If it's an import key like "import://base.json:logo_link", extract just "logo_link"
        if (key.startsWith('import://')) {
            const importMatch = key.match(/^import:\/\/[^:]+:(.+)$/);
            if (importMatch) {
                templateKey = importMatch[1].toUpperCase();
            }
        }

        const regex = new RegExp(`{{${templateKey}}}`, 'g');
        result = result.replace(regex, data[key]);
    });

    // Also handle direct export keys for backward compatibility
    Object.keys(data).forEach(key => {
        if (key.startsWith('export://')) {
            const exportKey = key.replace('export://', '').toUpperCase();
            const regex = new RegExp(`{{${exportKey}}}`, 'g');
            result = result.replace(regex, data[key]);
        }
    });

    // Handle conditional blocks with else support
    result = result.replace(/{{#if \(eq ([^ ]+) ([^)]+)\)}}([\s\S]*?)(?:{{else}}([\s\S]*?))?{{\/if}}/g, (match, varName, value, ifContent, elseContent) => {
        const actualValue = data[varName.toLowerCase()];
        // Handle both quoted strings and booleans
        const expectedValue = value.replace(/['"]/g, ''); // Remove quotes if present
        const isBoolean = expectedValue === 'true' || expectedValue === 'false';
        const compareValue = isBoolean ? (expectedValue === 'true') : expectedValue;

        return actualValue === compareValue ? ifContent : (elseContent || '');
    });

    // Load and replace component placeholders
    const componentPlaceholders = [
        'HEAD',
        'NAVIGATION',
        'HERO',
        'CORE_CONCEPTS',
        'DEVELOPER_BENEFITS',
        'AI_LOVE_LETTER',
        'QUICK_START',
        'LANGUAGE_NAVIGATION',
        'FLOATING_NAVIGATION',
        'FOOTER',
        'SCRIPTS',
        'LOGO'
    ];

    componentPlaceholders.forEach(placeholder => {
        // Convert placeholder to filename (e.g., CORE_CONCEPTS -> core-concepts.html, LOGO -> logo.html)
        const filename = placeholder.toLowerCase().replace(/_/g, '-') + '.html';
        const componentPath = path.join(__dirname, filename);
        if (fs.existsSync(componentPath)) {
            let componentContent = loadComponent(componentPath);

            // Replace variables in the component content
            Object.keys(data).forEach(key => {
                // Handle both direct keys and import-resolved keys
                let templateKey = key.toUpperCase();

                // If it's an import key like "import://base.json:logo_link", extract just "logo_link"
                if (key.startsWith('import://')) {
                    const importMatch = key.match(/^import:\/\/[^:]+:(.+)$/);
                    if (importMatch) {
                        templateKey = importMatch[1].toUpperCase();
                    }
                }

                const regex = new RegExp(`{{${templateKey}}}`, 'g');
                componentContent = componentContent.replace(regex, data[key]);
            });

            // Also handle direct export keys for backward compatibility
            Object.keys(data).forEach(key => {
                if (key.startsWith('export://')) {
                    const exportKey = key.replace('export://', '').toUpperCase();
                    const regex = new RegExp(`{{${exportKey}}}`, 'g');
                    componentContent = componentContent.replace(regex, data[key]);
                }
            });

            // Handle conditional blocks in component content
            componentContent = componentContent.replace(/{{#if \(eq ([^ ]+) ([^)]+)\)}}([\s\S]*?)(?:{{else}}([\s\S]*?))?{{\/if}}/g, (match, varName, value, ifContent, elseContent) => {
                const actualValue = data[varName.toLowerCase()];
                // Handle both quoted strings and booleans
                const expectedValue = value.replace(/['"]/g, ''); // Remove quotes if present
                const isBoolean = expectedValue === 'true' || expectedValue === 'false';
                const compareValue = isBoolean ? (expectedValue === 'true') : expectedValue;

                return actualValue === compareValue ? ifContent : (elseContent || '');
            });

            const regex = new RegExp(`{{${placeholder}}}`, 'g');
            result = result.replace(regex, componentContent);
        }
    });

    return result;
}

async function buildIndexPage() {
    const templatePath = path.join(__dirname, 'template.html');
    const dataPath = path.join(__dirname, 'data', 'index.json');
    const outputPath = path.join(__dirname, '..', 'index.html');

    console.log('Building main index page...');

    const template = loadTemplate(templatePath);
    const data = await loadData(dataPath);
    const result = populateTemplate(template, data);

    fs.writeFileSync(outputPath, result);
    console.log('✅ main index page built successfully');
}

async function buildLanguagePage(language) {
    const templatePath = path.join(__dirname, 'template.html');
    const dataPath = path.join(__dirname, 'data', `${language}.json`);
    const outputPath = path.join(__dirname, '..', language, 'index.html');

    console.log(`Building ${language} page...`);

    const template = loadTemplate(templatePath);
    const data = await loadData(dataPath);
    const result = populateTemplate(template, data);

    // Ensure output directory exists
    const outputDir = path.dirname(outputPath);
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }

    fs.writeFileSync(outputPath, result);
    console.log(`✅ ${language} page built successfully`);
}

async function main() {
    const languages = ['pseudo', 'python', 'go', 'javascript', 'csharp', 'rust', 'cpp', 'java', 'cobol'];

    console.log('🚀 Building CodeUChain documentation pages...\n');

    // Build main index page first
    try {
        await buildIndexPage();
    } catch (error) {
        console.error('❌ Error building main index page:', error.message);
    }

    // Build language-specific pages
    for (const language of languages) {
        try {
            await buildLanguagePage(language);
        } catch (error) {
            console.error(`❌ Error building ${language} page:`, error.message);
        }
    }

    console.log('\n🎉 All pages built successfully!');
}

if (require.main === module) {
    main();
}

module.exports = { buildLanguagePage, buildIndexPage, populateTemplate, main };