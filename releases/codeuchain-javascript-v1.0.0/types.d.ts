// Concrete type declarations for the package public API

// Type variables for generic typing
export type TInput = any;
export type TOutput = any;

export declare class Context<T = any> {
  constructor(data?: Record<string, any>);
  static empty(): Context<T>;
  static from<TData = any>(data: TData): Context<TData>;
  get(key: string): any;
  insert(key: string, value: any): Context<T>;
  insertAs<TNew = any>(key: string, value: any): Context<TNew>;
  withMutation(): MutableContext<T>;
  merge(other: Context<T>): Context<T>;
  toObject(): Record<string, any>;
  has(key: string): boolean;
  keys(): string[];
}

export declare class MutableContext<T = any> {
  constructor(data?: Record<string, any>);
  get(key: string): any;
  set(key: string, value: any): void;
  toImmutable(): Context<T>;
  has(key: string): boolean;
  keys(): string[];
}

export declare class Link<TInput = any, TOutput = any> {
  call(ctx: Context<TInput>): Promise<Context<TOutput>>;
  getName(): string;
  validateContext(ctx: Context<TInput>, requiredFields?: string[]): void;
}

export declare class Chain<TInput = any, TOutput = any> {
  constructor();
  addLink(link: Link<TInput, TOutput>, name?: string): Chain<TInput, TOutput>;
  connect(source: string, target: string, condition?: (ctx: Context<TInput>) => boolean): Chain<TInput, TOutput>;
  useMiddleware(middleware: Middleware): Chain<TInput, TOutput>;
  onError(handler: (err: Error, ctx: Context<TInput>, linkName: string) => any): Chain<TInput, TOutput>;
  run(initialCtx: Context<TInput>): Promise<Context<TOutput>>;
  static createLinear<TInput = any, TOutput = any>(...links: Link<any, any>[]): Chain<TInput, TOutput>;
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
