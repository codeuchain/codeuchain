// Concrete type declarations for the package public API

// Type variables for generic typing
export type TInput = any;
export type TOutput = any;

export declare class State<T = any> {
  constructor(data?: Record<string, any>);
  static empty(): State<T>;
  static from<TData = any>(data: TData): State<TData>;
  get(key: string): any;
  insert(key: string, value: any): State<T>;
  insertAs<TNew = any>(key: string, value: any): State<TNew>;
  withMutation(): MutableState<T>;
  merge(other: State<T>): State<T>;
  toObject(): Record<string, any>;
  has(key: string): boolean;
  keys(): string[];
}

export declare class MutableState<T = any> {
  constructor(data?: Record<string, any>);
  get(key: string): any;
  set(key: string, value: any): void;
  toImmutable(): State<T>;
  has(key: string): boolean;
  keys(): string[];
}

export declare class Link<TInput = any, TOutput = any> {
  call(ctx: State<TInput>): Promise<State<TOutput>>;
  getName(): string;
  validateState(ctx: State<TInput>, requiredFields?: string[]): void;
}

export declare class Chain<TInput = any, TOutput = any> {
  constructor();
  addLink(link: Link<TInput, TOutput>, name?: string): Chain<TInput, TOutput>;
  connect(source: string, target: string, condition?: (ctx: State<TInput>) => boolean): Chain<TInput, TOutput>;
  useHook(hook: Hook): Chain<TInput, TOutput>;
  onError(handler: (err: Error, ctx: State<TInput>, linkName: string) => any): Chain<TInput, TOutput>;
  run(initialCtx: State<TInput>): Promise<State<TOutput>>;
  static createLinear<TInput = any, TOutput = any>(...links: Link<any, any>[]): Chain<TInput, TOutput>;
}

export declare class Hook {
  before?(link: Link, ctx: State, linkName: string): Promise<void> | void;
  after?(link: Link, ctx: State, linkName: string): Promise<void> | void;
  onError?(link: Link, error: Error, ctx: State, linkName: string): Promise<void> | void;
}

export declare class LoggingHook extends Hook {}
export declare class TimingHook extends Hook {}
export declare class ValidationHook extends Hook {}

export declare const version: string;

export type DefaultExport = {
  State: typeof State;
  MutableState: typeof MutableState;
  Link: typeof Link;
  Chain: typeof Chain;
  Hook: typeof Hook;
  LoggingHook: typeof LoggingHook;
  TimingHook: typeof TimingHook;
  ValidationHook: typeof ValidationHook;
  version: string;
};

declare const _default: DefaultExport;
export default _default;
