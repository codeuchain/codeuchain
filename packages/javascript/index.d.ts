/**
 * CodeUChain TypeScript Entry Point
 * 
 * Main entry point for TypeScript consumers of the CodeUChain package.
 * Re-exports all types and runtime values from the types definition file.
 * 
 * This file provides:
 * - All type definitions from types.d.ts
 * - Default export for CommonJS compatibility
 * - Named exports for ES module usage
 * - Full TypeScript IntelliSense support
 * 
 * @fileoverview TypeScript entry point for CodeUChain
 * @version 1.0.1
 * @since 1.0.0
 * 
 * @example
 * ```typescript
 * // Named imports (recommended)
 * import { Context, Chain, Link, LoggingMiddleware } from 'codeuchain';
 * 
 * // Default import
 * import CodeUChain from 'codeuchain';
 * 
 * // Mixed usage
 * import CodeUChain, { Context, Chain } from 'codeuchain';
 * ```
 */

// Re-export all types and values from types.d.ts
export * from './types';

/**
 * Default export for CommonJS and mixed import compatibility.
 * Provides access to all CodeUChain classes through a single import.
 * 
 * @example
 * ```typescript
 * import CodeUChain from 'codeuchain';
 * 
 * const ctx = new CodeUChain.Context({ user: 'Alice' });
 * const chain = new CodeUChain.Chain()
 *   .useMiddleware(new CodeUChain.LoggingMiddleware())
 *   .addLink(new MyProcessingLink());
 * ```
 */
export { default } from './types';
