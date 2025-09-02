// Concrete type declarations for the package public API

export declare class Context {
  constructor(data?: Record<string, any>);
  static empty(): Context;
  static from(data: Record<string, any>): Context;
  get(key: string): any;
  insert(key: string, value: any): Context;
  withMutation(): MutableContext;
  merge(other: Context): Context;
  toObject(): Record<string, any>;
  has(key: string): boolean;
  keys(): string[];
}

export declare class MutableContext {
  constructor(data?: Record<string, any>);
  get(key: string): any;
  set(key: string, value: any): void;
  toImmutable(): Context;
  has(key: string): boolean;
  keys(): string[];
}

export declare class Link {
  call(ctx: Context): Promise<Context>;
  getName(): string;
  validateContext(ctx: Context, requiredFields?: string[]): void;
}

export declare class Chain {
  constructor();
  addLink(link: Link, name?: string): Chain;
  connect(source: string, target: string, condition?: (ctx: Context) => boolean): Chain;
  useMiddleware(middleware: Middleware): Chain;
  onError(handler: (err: Error, ctx: Context, linkName: string) => any): Chain;
  run(initialCtx: Context): Promise<Context>;
  static createLinear(...links: Link[]): Chain;
}

export declare class Middleware {
  before?(link: Link, ctx: Context, linkName: string): Promise<void> | void;
  after?(link: Link, ctx: Context, linkName: string): Promise<void> | void;
  onError?(link: Link, error: Error, ctx: Context, linkName: string): Promise<void> | void;
}

export declare class LoggingMiddleware extends Middleware {}
export declare class TimingMiddleware extends Middleware {}
export declare class ValidationMiddleware extends Middleware {}

export declare const version: string;

export type DefaultExport = {
  Context: typeof Context;
  MutableContext: typeof MutableContext;
  Link: typeof Link;
  Chain: typeof Chain;
  Middleware: typeof Middleware;
  LoggingMiddleware: typeof LoggingMiddleware;
  TimingMiddleware: typeof TimingMiddleware;
  ValidationMiddleware: typeof ValidationMiddleware;
  version: string;
};

declare const _default: DefaultExport;
export default _default;
