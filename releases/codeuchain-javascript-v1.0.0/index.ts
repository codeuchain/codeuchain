// TypeScript wrapper for the existing JavaScript implementation.
// This file re-exports the runtime JS modules so TypeScript consumers can import from
// the package while using the JS implementation at runtime.

import * as runtime from './core/index';
import type {
  Context as ContextType,
  MutableContext as MutableContextType,
  Link as LinkType,
  Chain as ChainType,
  Middleware as MiddlewareType,
  LoggingMiddleware as LoggingMiddlewareType,
  TimingMiddleware as TimingMiddlewareType,
  ValidationMiddleware as ValidationMiddlewareType,
  DefaultExport
} from './types';

// Re-export runtime constructors with proper types (value exports)
export const Context: typeof ContextType = (runtime as any).Context;
export const MutableContext: typeof MutableContextType = (runtime as any).MutableContext;
export const Link: typeof LinkType = (runtime as any).Link;
export const Chain: typeof ChainType = (runtime as any).Chain;
export const Middleware: typeof MiddlewareType = (runtime as any).Middleware;
export const LoggingMiddleware: typeof LoggingMiddlewareType = (runtime as any).LoggingMiddleware;
export const TimingMiddleware: typeof TimingMiddlewareType = (runtime as any).TimingMiddleware;
export const ValidationMiddleware: typeof ValidationMiddlewareType = (runtime as any).ValidationMiddleware;

export const version: string = (runtime as any).version || '';

// Default export for JS consumers that import the package directly
export default (runtime as unknown) as DefaultExport;
