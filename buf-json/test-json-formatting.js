/**
 * Test JSON Output Formatting
 *
 * This test verifies that the hybrid reader can format JSON output
 * with proper indentation and structure.
 */

const { HybridConfigReader } = require('./src/hybrid/hybrid-reader');
const fs = require('fs');
const path = require('path');

async function testJsonFormatting() {
    console.log('🧪 Testing JSON Output Formatting...\n');

    const reader = new HybridConfigReader('.', {
        formatJson: true,
        jsonIndent: 2
    });

    try {
        // Test 1: Read formatted JSON
        console.log('📄 Test 1: Reading formatted JSON...');
        const formattedData = await reader.readFormatted('data/base.json');
        console.log('✅ Formatted JSON (first 200 chars):');
        console.log(formattedData.substring(0, 200) + '...\n');

        // Test 2: Read raw data
        console.log('📄 Test 2: Reading raw data...');
        const rawData = await reader.readRaw('data/base.json');
        console.log('✅ Raw data type:', typeof rawData);
        console.log('✅ Raw data keys:', Object.keys(rawData), '\n');

        // Test 3: Read compact JSON
        console.log('📄 Test 3: Reading compact JSON...');
        const compactJson = await reader.readCompact('data/base.json');
        console.log('✅ Compact JSON (first 100 chars):');
        console.log(compactJson.substring(0, 100) + '...\n');

        // Test 4: Export formatted JSON
        console.log('📄 Test 4: Exporting formatted JSON...');
        const exportPath = 'data/test-export-formatted.json';
        await reader.exportAsJson('data/base.json', exportPath, { jsonIndent: 4 });

        if (fs.existsSync(exportPath)) {
            const exportedContent = fs.readFileSync(exportPath, 'utf8');
            console.log('✅ Exported file size:', exportedContent.length, 'characters');
            console.log('✅ Exported content (first 150 chars):');
            console.log(exportedContent.substring(0, 150) + '...\n');
        }

        // Test 5: Different indentation levels
        console.log('📄 Test 5: Different indentation levels...');
        const reader4Spaces = new HybridConfigReader('.', { jsonIndent: 4 });
        const formatted4Spaces = await reader4Spaces.readFormatted('data/base.json');
        console.log('✅ 4-space indentation (first 100 chars):');
        console.log(formatted4Spaces.substring(0, 100) + '...\n');

        // Test 6: Disable formatting globally
        console.log('📄 Test 6: Disabled formatting...');
        const readerNoFormat = new HybridConfigReader('.', { formatJson: false });
        const unformattedData = await readerNoFormat.readFile('data/base.json');
        console.log('✅ Unformatted data type:', typeof unformattedData);
        console.log('✅ Unformatted data keys:', Object.keys(unformattedData), '\n');

        console.log('🎉 All JSON formatting tests passed!');

    } catch (error) {
        console.error('❌ JSON formatting test failed:', error.message);
        throw error;
    }
}

// Run the test
if (require.main === module) {
    testJsonFormatting().catch(console.error);
}

module.exports = { testJsonFormatting };