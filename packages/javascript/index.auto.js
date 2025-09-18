/**
 * CodeUChain Auto-Detection Entry Point
 * 
 * Automatically chooses the appropriate version based on the environment.
 * Falls back to ES5 for maximum compatibility.
 */

// Simple feature detection for ES6 support
function supportsES6() {
  try {
    // Test for arrow functions, const/let, and classes
    new Function('const x = () => class {}')();
    return true;
  } catch (e) {
    return false;
  }
}

// Export appropriate version
if (supportsES6()) {
  module.exports = require('./index.es6');
} else {
  module.exports = require('./index.es5');
}