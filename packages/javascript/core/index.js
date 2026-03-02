/**
 * CodeUChain JavaScript Core
 *
 * The loving foundation of CodeUChain for JavaScript ecosystems.
 * The core building blocks for state flow.
 */

const { State, MutableState } = require('./state');
const { Link } = require('./link');
const { Chain } = require('./chain');
const {
  Hook,
  LoggingHook,
  TimingHook,
  ValidationHook
} = require('./hook');

module.exports = {
  // Core classes
  State,
  MutableState,
  Link,
  Chain,
  Hook,

  // Common hook implementations
  LoggingHook,
  TimingHook,
  ValidationHook,

  // Version info
  version: '0.1.0'
};