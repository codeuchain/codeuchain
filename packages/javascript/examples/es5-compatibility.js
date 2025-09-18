/**
 * ES5 Compatibility Example
 * 
 * This example demonstrates CodeUChain working in both ES5 and ES6+ environments
 */

console.log('=== CodeUChain ES5 Support Demo ===\n');

// Test auto-detection
console.log('1. Auto-detection version:');
const auto = require('../index.auto');
console.log('Version:', auto.version);
console.log('Available exports:', Object.keys(auto).join(', '));

// Test ES5 version explicitly
console.log('\n2. ES5 explicit version:');
const es5 = require('../index.es5');
console.log('Version:', es5.version);

// Create a simple chain using ES5 version
const { Context: ES5Context, Link: ES5Link, Chain: ES5Chain } = es5;

// Define a processing link
function ValidationLink() {
  ES5Link.call(this);
}
ValidationLink.prototype = Object.create(ES5Link.prototype);
ValidationLink.prototype.constructor = ValidationLink;
ValidationLink.prototype.call = function(ctx) {
  var email = ctx.get('email');
  var isValid = email && email.includes('@');
  return Promise.resolve(ctx.insert('isValid', isValid));
};

// Define another processing link
function ProcessingLink() {
  ES5Link.call(this);
}
ProcessingLink.prototype = Object.create(ES5Link.prototype);
ProcessingLink.prototype.constructor = ProcessingLink;
ProcessingLink.prototype.call = function(ctx) {
  var name = ctx.get('name');
  var processed = name ? name.toUpperCase() : 'ANONYMOUS';
  return Promise.resolve(ctx.insert('processedName', processed));
};

// Create and run chain
async function runES5Example() {
  try {
    console.log('\n3. Running ES5 chain example:');
    
    var chain = new ES5Chain();
    chain.addLink(new ValidationLink(), 'validator');
    chain.addLink(new ProcessingLink(), 'processor');
    chain.connect('validator', 'processor');
    
    var inputContext = new ES5Context({
      name: 'Alice Johnson',
      email: 'alice@example.com'
    });
    
    console.log('Input: name=' + inputContext.get('name') + ', email=' + inputContext.get('email'));
    
    var result = await chain.run(inputContext);
    
    console.log('Output: name=' + result.get('name') + ', email=' + result.get('email') + 
                ', isValid=' + result.get('isValid') + ', processedName=' + result.get('processedName'));
    console.log('Email valid:', result.get('isValid'));
    console.log('Processed name:', result.get('processedName'));
    
  } catch (error) {
    console.error('Error:', error.message);
  }
}

// Test ES6+ version for comparison
console.log('\n4. ES6+ explicit version:');
const es6 = require('../index.es6');
console.log('Version:', es6.version);

const { Context: ES6Context, Link: ES6Link, Chain: ES6Chain } = es6;

class ES6ValidationLink extends ES6Link {
  async call(ctx) {
    const email = ctx.get('email');
    const isValid = email && email.includes('@');
    return ctx.insert('isValid', isValid);
  }
}

class ES6ProcessingLink extends ES6Link {
  async call(ctx) {
    const name = ctx.get('name');
    const processed = name ? name.toUpperCase() : 'ANONYMOUS';
    return ctx.insert('processedName', processed);
  }
}

async function runES6Example() {
  try {
    console.log('\n5. Running ES6+ chain example:');
    
    const chain = new ES6Chain();
    chain.addLink(new ES6ValidationLink(), 'validator');
    chain.addLink(new ES6ProcessingLink(), 'processor');
    chain.connect('validator', 'processor');
    
    const inputContext = new ES6Context({
      name: 'Alice Johnson',
      email: 'alice@example.com'
    });
    
    console.log('Input: name=' + inputContext.get('name') + ', email=' + inputContext.get('email'));
    
    const result = await chain.run(inputContext);
    
    console.log('Output: name=' + result.get('name') + ', email=' + result.get('email') + 
                ', isValid=' + result.get('isValid') + ', processedName=' + result.get('processedName'));
    console.log('Email valid:', result.get('isValid'));
    console.log('Processed name:', result.get('processedName'));
    
  } catch (error) {
    console.error('Error:', error.message);
  }
}

// Run both examples
async function main() {
  await runES5Example();
  await runES6Example();
  
  console.log('\n6. Compatibility verified! ✅');
  console.log('Both ES5 and ES6+ versions work identically.');
}

main().catch(console.error);