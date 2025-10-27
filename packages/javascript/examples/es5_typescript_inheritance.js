/**
 * ES5 TypeScript Inheritance Example
 * 
 * This example demonstrates how CodeUChain Link and Chain classes can be properly
 * extended when using TypeScript with ES5 target compilation.
 * 
 * TypeScript Configuration:
 * {
 *   "compilerOptions": {
 *     "target": "ES5",
 *     "module": "commonjs"
 *   }
 * }
 * 
 * This example shows the TypeScript source code and what it compiles to in ES5.
 */

// ============================================================================
// TypeScript Source Code (what developers write)
// ============================================================================

/*
import { Link, Chain, Context } from 'codeuchain';

interface InputData {
  value: number;
}

interface ValidatedData extends InputData {
  isValid: boolean;
}

interface ProcessedData extends ValidatedData {
  result: number;
}

class ValidateLink extends Link<InputData, ValidatedData> {
  async call(ctx: Context<InputData>): Promise<Context<ValidatedData>> {
    this.validateContext(ctx, ['value']);
    const value = ctx.get('value');
    const isValid = typeof value === 'number' && value > 0;
    return ctx.insertAs<ValidatedData>('isValid', isValid);
  }
}

class ProcessLink extends Link<ValidatedData, ProcessedData> {
  async call(ctx: Context<ValidatedData>): Promise<Context<ProcessedData>> {
    const value = ctx.get('value');
    const isValid = ctx.get('isValid');
    
    if (!isValid) {
      throw new Error('Cannot process invalid data');
    }
    
    const result = value * 2;
    return ctx.insertAs<ProcessedData>('result', result);
  }
}

async function main() {
  const chain = new Chain<InputData, ProcessedData>();
  
  chain.addLink(new ValidateLink(), 'validate');
  chain.addLink(new ProcessLink(), 'process');
  chain.connect('validate', 'process', (ctx) => ctx.get('isValid'));
  
  const inputCtx = new Context<InputData>({ value: 42 });
  const resultCtx = await chain.run(inputCtx);
  
  console.log('Result:', resultCtx.get('result')); // Output: 84
}

main();
*/

// ============================================================================
// Compiled ES5 Output (what TypeScript generates with target: ES5)
// ============================================================================

const { Link, Chain, Context } = require('../core');

var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();

var ValidateLink = /** @class */ (function (_super) {
    __extends(ValidateLink, _super);
    function ValidateLink() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ValidateLink.prototype.call = function (ctx) {
        this.validateContext(ctx, ['value']);
        var value = ctx.get('value');
        var isValid = typeof value === 'number' && value > 0;
        return Promise.resolve(ctx.insertAs('isValid', isValid));
    };
    return ValidateLink;
}(Link));

var ProcessLink = /** @class */ (function (_super) {
    __extends(ProcessLink, _super);
    function ProcessLink() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    ProcessLink.prototype.call = function (ctx) {
        var value = ctx.get('value');
        var isValid = ctx.get('isValid');
        
        if (!isValid) {
            throw new Error('Cannot process invalid data');
        }
        
        var result = value * 2;
        return Promise.resolve(ctx.insertAs('result', result));
    };
    return ProcessLink;
}(Link));

async function main() {
    var chain = new Chain();
    
    chain.addLink(new ValidateLink(), 'validate');
    chain.addLink(new ProcessLink(), 'process');
    chain.connect('validate', 'process', function(ctx) { 
        return ctx.get('isValid'); 
    });
    
    var inputCtx = new Context({ value: 42 });
    var resultCtx = await chain.run(inputCtx);
    
    console.log('✓ ES5 inheritance working correctly!');
    console.log('Input value:', inputCtx.get('value'));
    console.log('Is valid:', resultCtx.get('isValid'));
    console.log('Result (value * 2):', resultCtx.get('result'));
    
    // Test with invalid data
    try {
        var invalidCtx = new Context({ value: -5 });
        await chain.run(invalidCtx);
    } catch (error) {
        console.log('✓ Error handling works:', error.message);
    }
}

// Run the example
if (require.main === module) {
    main().then(function() {
        console.log('\n✓ ES5 TypeScript inheritance example completed successfully!');
    }).catch(function(error) {
        console.error('Error:', error);
        process.exit(1);
    });
}

module.exports = { ValidateLink, ProcessLink, main };
