// TypeScript wrapper for the existing JavaScript implementation.
// This file re-exports the runtime JS modules so TypeScript consumers can import from
// the package while using the JS implementation at runtime.

import * as runtime from './core/index';
import type {
  State as StateType,
  MutableState as MutableStateType,
  Link as LinkType,
  Chain as ChainType,
  Hook as HookType,
  LoggingHook as LoggingHookType,
  TimingHook as TimingHookType,
  ValidationHook as ValidationHookType,
  DefaultExport
} from './types';

// Re-export runtime constructors with proper types (value exports)
export const State: typeof StateType = (runtime as any).State;
export const MutableState: typeof MutableStateType = (runtime as any).MutableState;
export const Link: typeof LinkType = (runtime as any).Link;
export const Chain: typeof ChainType = (runtime as any).Chain;
export const Hook: typeof HookType = (runtime as any).Hook;
export const LoggingHook: typeof LoggingHookType = (runtime as any).LoggingHook;
export const TimingHook: typeof TimingHookType = (runtime as any).TimingHook;
export const ValidationHook: typeof ValidationHookType = (runtime as any).ValidationHook;

export const version: string = (runtime as any).version || '';

// Default export for JS consumers that import the package directly
export default (runtime as unknown) as DefaultExport;
