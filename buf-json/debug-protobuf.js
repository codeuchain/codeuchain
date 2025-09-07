#!/usr/bin/env node

/**
 * Debug script for protobuf compilation issues
 */

const path = require('path');
const protobuf = require('protobufjs');
const fs = require('fs');

async function debugProtobuf() {
    console.log('🔍 Debugging protobuf compilation...\n');

    const dataDir = path.join(__dirname, 'data');
    const protoPath = path.join(dataDir, 'protobuf', 'base.proto');
    const jsonPath = path.join(dataDir, 'base.json');

    try {
        // Load the JSON data
        const jsonData = JSON.parse(fs.readFileSync(jsonPath, 'utf8'));
        console.log('📄 Original JSON data:');
        console.log(JSON.stringify(jsonData, null, 2));
        console.log();

        // Load protobuf schema
        console.log('📋 Loading protobuf schema...');
        const root = await protobuf.load(protoPath);
        const MessageClass = root.lookupType('base');

        console.log('📋 Message fields:');
        MessageClass.fieldsArray.forEach(field => {
            console.log(`   - ${field.name} (${field.type}) = ${field.id}`);
        });
        console.log();

        // Map JSON to protobuf fields
        const mappedData = {};
        for (const [key, value] of Object.entries(jsonData)) {
            const sanitizedKey = key.replace(/^import:\/\/|^export:\/\//, '').replace(/[^a-zA-Z0-9_]/g, '_');
            mappedData[sanitizedKey] = value;
            console.log(`   ${key} -> ${sanitizedKey}: ${value}`);
        }
        console.log();

        console.log('📋 Mapped data for protobuf:');
        console.log(JSON.stringify(mappedData, null, 2));
        console.log();

        // Create protobuf message
        console.log('📦 Creating protobuf message...');
        const message = MessageClass.fromObject(mappedData);
        console.log('✅ Message created successfully');

        // Check message fields
        console.log('📋 Message field values:');
        for (const field of MessageClass.fieldsArray) {
            const value = message[field.name];
            console.log(`   ${field.name}: ${value}`);
        }
        console.log();

        // Encode to binary
        console.log('🔄 Encoding to binary...');
        const buffer = MessageClass.encode(message).finish();
        console.log(`✅ Encoded to ${buffer.length} bytes`);

        // Decode back
        console.log('🔄 Decoding back to message...');
        const decodedMessage = MessageClass.decode(buffer);
        console.log('✅ Decoded successfully');

        // Convert to object
        console.log('🔄 Converting to object...');
        const decodedObject = MessageClass.toObject(decodedMessage, {
            longs: String,
            enums: String,
            bytes: String,
        });

        console.log('📋 Decoded object:');
        console.log(JSON.stringify(decodedObject, null, 2));

    } catch (error) {
        console.error('❌ Debug failed:', error.message);
        console.error(error.stack);
    }
}

debugProtobuf();