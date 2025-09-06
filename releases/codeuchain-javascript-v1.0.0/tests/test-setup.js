// Test setup for CodeUChain JavaScript tests
// This file runs before each test suite

// Global test utilities
global.testUtils = {
  // Create a simple test context
  createTestContext: (data = {}) => {
    const { Context } = require('../core');
    return new Context(data);
  },

  // Create a simple test link
  createTestLink: (name = 'test', processor = async (ctx) => ctx) => {
    const { Link } = require('../core');

    class TestLink extends Link {
      async call(ctx) {
        return await processor(ctx);
      }
    }

    return new TestLink();
  },

  // Create a simple test chain
  createTestChain: () => {
    const { Chain } = require('../core');
    return new Chain();
  }
};

// Set up console spy for middleware tests
beforeEach(() => {
  jest.spyOn(console, 'log').mockImplementation(() => {});
  jest.spyOn(console, 'error').mockImplementation(() => {});
});

afterEach(() => {
  jest.restoreAllMocks();
});