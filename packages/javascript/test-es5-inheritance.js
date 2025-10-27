// Test to reproduce ES5 inheritance issue
// This is what TypeScript generates when targeting ES5

const { Link, Chain, Context } = require('./core');

// ES5 inheritance pattern (what TypeScript generates for ES5 target)
function MyLink() {
  // TypeScript ES5 output tries to call the parent constructor
  // like this, which fails with ES6 class syntax
  return Link.call(this) || this;
}

// ES5 prototype chain setup
MyLink.prototype = Object.create(Link.prototype);
MyLink.prototype.constructor = MyLink;
MyLink.prototype.call = async function(ctx) {
  return ctx.insert('processed', true);
};

// Test instantiation
try {
  const link = new MyLink();
  console.log('ES5 inheritance test passed!');
  console.log('Link instance:', link instanceof Link);
} catch (error) {
  console.error('ES5 inheritance test FAILED:', error.message);
}
