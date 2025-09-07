const { readJSONWithSharing, readJSONRaw } = require('./json-reader');

/**
 * Example usage of the Runtime JSON Reader with Sharing Support
 *
 * This demonstrates how to use the sharing system in a real application.
 */

async function demonstrateSharing() {
    console.log('🚀 CodeUChain Documentation System Demo\n');

    try {
        // Read the base configuration
        console.log('📖 Reading base configuration...');
        const baseConfig = await readJSONWithSharing('data/base.json');
        console.log('Base config:', JSON.stringify(baseConfig, null, 2));
        console.log();

        // Read index page data with sharing
        console.log('📖 Reading index page with sharing...');
        const indexData = await readJSONWithSharing('data/index.json');
        console.log('Index data:', JSON.stringify(indexData, null, 2));
        console.log();

        // Read Python page data with sharing
        console.log('📖 Reading Python page with sharing...');
        const pythonData = await readJSONWithSharing('data/python.json');
        console.log('Python data:', JSON.stringify(pythonData, null, 2));
        console.log();

        // Show raw data for comparison
        console.log('📖 Reading Python page raw (without sharing)...');
        const pythonRaw = await readJSONRaw('data/python.json');
        console.log('Python raw:', JSON.stringify(pythonRaw, null, 2));
        console.log();

        // Demonstrate that raw files are still valid JSON
        console.log('✅ Raw files are still valid JSON that any parser can read!');
        console.log('✅ Sharing is resolved at runtime for enhanced functionality!');

    } catch (error) {
        console.error('❌ Error:', error.message);
    }
}

// Example of how you might use this in a web application
async function loadPageData(pageName) {
    try {
        const pageData = await readJSONWithSharing(`data/${pageName}.json`);

        // Now you have fully resolved data with all shared values
        return {
            ...pageData,
            // Add any additional processing here
            loadedAt: new Date().toISOString()
        };
    } catch (error) {
        console.error(`Failed to load page data for ${pageName}:`, error.message);
        return null;
    }
}

// Run the demonstration
if (require.main === module) {
    demonstrateSharing();
}

module.exports = {
    loadPageData,
    demonstrateSharing
};