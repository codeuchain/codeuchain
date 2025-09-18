# ES5 Support Guide

CodeUChain JavaScript now supports both ES5 Legacy and ES6+ environments through dual builds and conditional exports.

## Overview

This implementation provides three ways to use CodeUChain:

1. **Automatic Detection** (`require('codeuchain')`) - Automatically chooses ES5 or ES6+ based on environment
2. **ES5 Explicit** (`require('codeuchain/es5')`) - Forces ES5-compatible version
3. **ES6+ Explicit** (`require('codeuchain/es6')`) - Forces modern ES6+ version

## Installation

```bash
npm install codeuchain
```

## Usage

### Automatic Detection (Recommended)

```javascript
const { Context, Link, Chain } = require('codeuchain');

// This automatically uses ES5 in legacy environments
// and ES6+ in modern environments
```

### Explicit ES5 Support

```javascript
const { Context, Link, Chain } = require('codeuchain/es5');

// This explicitly uses the ES5-transpiled version
// Compatible with IE11+, older Node.js versions, etc.
```

### Explicit ES6+ Support

```javascript
const { Context, Link, Chain } = require('codeuchain/es6');

// This explicitly uses the modern ES6+ version
// Requires ES6+ support (Node.js 6+, modern browsers)
```

## Feature Compatibility

Both ES5 and ES6+ versions provide identical functionality:

| Feature | ES5 Version | ES6+ Version |
|---------|-------------|--------------|
| Context class | ✅ (transpiled) | ✅ (native) |
| MutableContext class | ✅ (transpiled) | ✅ (native) |
| Link class | ✅ (transpiled) | ✅ (native) |
| Chain class | ✅ (transpiled) | ✅ (native) |
| Async/await support | ✅ (polyfilled) | ✅ (native) |
| Object spread syntax | ✅ (polyfilled) | ✅ (native) |
| For-of loops | ✅ (transpiled) | ✅ (native) |
| Middleware support | ✅ | ✅ |
| TypeScript definitions | ✅ | ✅ |

## Environment Support

### ES5 Version
- Internet Explorer 11+
- Node.js 6.0+
- Chrome 21+
- Firefox 28+
- Safari 7+
- All legacy environments

### ES6+ Version
- Node.js 14.0+ (recommended)
- Chrome 61+
- Firefox 60+
- Safari 10.1+
- Edge 16+

## Performance Considerations

- **ES6+ Version**: Optimal performance, uses native features
- **ES5 Version**: Slightly slower due to polyfills and transpilation
- **Automatic Detection**: No runtime overhead for feature detection

Performance difference is typically negligible for most applications.

## Build Process

The ES5 version is generated using Babel with the following transformations:

1. **Class syntax** → Function constructors with prototypes
2. **Async/await** → Generator functions with regenerator runtime
3. **Object spread** → Object.assign with polyfills
4. **For-of loops** → Traditional for loops
5. **Template literals** → String concatenation
6. **Const/let** → Var declarations

## Migration Guide

### From ES6-only to Dual Support

No changes needed! Existing code continues to work:

```javascript
// This still works and automatically detects environment
const { Context, Link } = require('codeuchain');
```

### Supporting Legacy Environments

If you need to explicitly support older environments:

```javascript
// Use explicit ES5 version
const { Context, Link } = require('codeuchain/es5');
```

### Optimizing for Modern Environments

If you only target modern environments:

```javascript
// Use explicit ES6+ version
const { Context, Link } = require('codeuchain/es6');
```

## Troubleshooting

### "Unexpected token" errors
Use the ES5 version explicitly:
```javascript
const codeuchain = require('codeuchain/es5');
```

### Performance issues
If ES5 polyfills cause performance problems, use the ES6+ version:
```javascript
const codeuchain = require('codeuchain/es6');
```

### Module not found errors
Make sure you're using the correct export path:
- ✅ `require('codeuchain/es5')`
- ❌ `require('codeuchain/dist/es5')`

## Testing

The package includes comprehensive tests for ES5 compatibility:

```bash
npm test -- tests/es5-compatibility.test.js
```

These tests verify:
- Identical functionality between ES5 and ES6+ versions
- Proper transpilation without ES6+ features
- Performance within acceptable bounds
- Polyfill behavior

## Technical Details

### Babel Configuration

The ES5 build uses:
- Target: ES5
- Polyfills: core-js 3
- Runtime: @babel/runtime
- Transforms: async-to-generator, class-properties

### Package Structure

```
codeuchain/
├── core/              # ES6+ source code
├── dist/
│   ├── es5/          # Transpiled ES5 code
│   └── es6/          # Copy of ES6+ code
├── index.auto.js     # Auto-detection entry point
├── index.es5.js      # ES5 entry point
├── index.es6.js      # ES6+ entry point
└── package.json      # Conditional exports
```

### Conditional Exports

Package.json provides multiple entry points:

```json
{
  "main": "index.auto.js",
  "exports": {
    ".": {
      "es5": "./index.es5.js",
      "es6": "./index.es6.js", 
      "default": "./index.auto.js"
    },
    "./es5": "./index.es5.js",
    "./es6": "./index.es6.js"
  }
}
```

This enables environment-specific imports while maintaining backward compatibility.