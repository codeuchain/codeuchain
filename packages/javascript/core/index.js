/**
 * CodeUChain JavaScript Core
 *
 * The loving foundation of CodeUChain for JavaScript ecosystems.
 * The core building blocks for context flow.
 */

const { Context, MutableContext } = require("./context");
const { Link } = require("./link");
const { Chain } = require("./chain");
const {
  Middleware,
  LoggingMiddleware,
  TimingMiddleware,
  ValidationMiddleware
} = require("./middleware");

module.exports = {
  // Core classes
  Context,
  MutableContext,
  Link,
  Chain,
  Middleware,

  // Common middleware implementations
  LoggingMiddleware,
  TimingMiddleware,
  ValidationMiddleware,

  // Version info
  version: "0.1.0"
};