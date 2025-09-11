// Package codeuchain provides a modular framework for chaining processing links
// with middleware support, designed for robust Go applications.
package codeuchain

import (
	"context"
)

// Context holds data carefully, immutable by default for safety, mutable for flexibility.
// It embraces Go's map-based approach with JSON marshaling.
// Enhanced with generic typing for type-safe workflows.
type Context[T any] struct {
	data map[string]interface{}
}

// NewContext creates a new context with initial data
func NewContext[T any](data map[string]interface{}) *Context[T] {
	if data == nil {
		data = make(map[string]interface{})
	}
	return &Context[T]{data: data}
}

// Get returns the value for the given key, forgiving absence with nil
func (c *Context[T]) Get(key string) interface{} {
	return c.data[key]
}

// Insert returns a fresh context with the addition, maintaining immutability
func (c *Context[T]) Insert(key string, value interface{}) *Context[T] {
	newData := make(map[string]interface{})
	for k, v := range c.data {
		newData[k] = v
	}
	newData[key] = value
	return &Context[T]{data: newData}
}

// InsertAs returns a fresh context with type evolution, allowing clean type transformations
func (c *Context[T]) InsertAs(key string, value interface{}) *Context[any] {
	newData := make(map[string]interface{})
	for k, v := range c.data {
		newData[k] = v
	}
	newData[key] = value
	return &Context[any]{data: newData}
}

// Merge combines contexts, favoring the other with compassion
func (c *Context[T]) Merge(other *Context[T]) *Context[T] {
	newData := make(map[string]interface{})
	for k, v := range c.data {
		newData[k] = v
	}
	for k, v := range other.data {
		newData[k] = v
	}
	return &Context[T]{data: newData}
}

// ToMap returns a copy of the internal data
func (c *Context[T]) ToMap() map[string]interface{} {
	result := make(map[string]interface{})
	for k, v := range c.data {
		result[k] = v
	}
	return result
}

// MutableContext provides mutable access for performance-critical sections
type MutableContext struct {
	data map[string]interface{}
}

// NewMutableContext creates a new mutable context
func NewMutableContext() *MutableContext {
	return &MutableContext{data: make(map[string]interface{})}
}

// Get returns the value for the given key
func (mc *MutableContext) Get(key string) interface{} {
	return mc.data[key]
}

// Set changes the value in place
func (mc *MutableContext) Set(key string, value interface{}) {
	mc.data[key] = value
}

// ToImmutable returns a fresh immutable copy
func (mc *MutableContext) ToImmutable() *Context[any] {
	return NewContext[any](mc.data)
}

// Link defines the selfless processor interface
type Link[TInput any, TOutput any] interface {
	// Call processes the context and returns a transformed context
	Call(ctx context.Context, c *Context[TInput]) (*Context[TOutput], error)
}

// Middleware defines optional enhancement hooks for processing links.
// All methods have default no-op implementations - override only what you need.
type Middleware[TInput any, TOutput any] interface {
	// Before is called before link execution (optional - defaults to no-op)
	Before(ctx context.Context, link Link[TInput, TOutput], c *Context[TInput]) error
	// After is called after successful link execution (optional - defaults to no-op)
	After(ctx context.Context, link Link[TInput, TOutput], c *Context[TOutput]) error
	// OnError is called when link execution fails (optional - defaults to no-op)
	OnError(ctx context.Context, link Link[TInput, TOutput], err error, c *Context[TInput]) error
}

// NopMiddleware provides no-op implementations for all middleware methods.
// This is the default middleware that does nothing - perfect for embedding or as a base.
var NopMiddleware = &nopMiddleware{}

type nopMiddleware struct{}

func (n *nopMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	return nil // No-op
}

func (n *nopMiddleware) After(ctx context.Context, link Link[any, any], c *Context[any]) error {
	return nil // No-op
}

func (n *nopMiddleware) OnError(ctx context.Context, link Link[any, any], err error, c *Context[any]) error {
	return nil // No-op
}

// Connection represents a conditional flow between links
type Connection[T any] struct {
	Source    string
	Target    string
	Condition func(*Context[T]) bool
}

// Chain orchestrates link execution with middleware
type Chain struct {
	links       map[string]Link[any, any]
	linkOrder   []string // Maintain insertion order
	connections []Connection[any]
	middlewares []Middleware[any, any]
}

// NewChain creates a new empty chain
func NewChain() *Chain {
	return &Chain{
		links:       make(map[string]Link[any, any]),
		linkOrder:   make([]string, 0),
		connections: make([]Connection[any], 0),
		middlewares: make([]Middleware[any, any], 0),
	}
}

// AddLink stores a link with the given name
func (ch *Chain) AddLink(name string, link Link[any, any]) {
	if _, exists := ch.links[name]; !exists {
		ch.linkOrder = append(ch.linkOrder, name)
	}
	ch.links[name] = link
}

// Connect adds a conditional connection between links
func (ch *Chain) Connect(source, target string, condition func(*Context[any]) bool) {
	ch.connections = append(ch.connections, Connection[any]{
		Source:    source,
		Target:    target,
		Condition: condition,
	})
}

// UseMiddleware attaches middleware to the chain
func (ch *Chain) UseMiddleware(mw Middleware[any, any]) {
	ch.middlewares = append(ch.middlewares, mw)
}

// Run executes the chain with the given context
func (ch *Chain) Run(ctx context.Context, initialCtx *Context[any]) (*Context[any], error) {
	currentCtx := initialCtx

	// Execute before hooks
	for _, mw := range ch.middlewares {
		if err := mw.Before(ctx, nil, currentCtx); err != nil {
			return nil, err
		}
	}

	// Simple linear execution for now
	for _, name := range ch.linkOrder {
		link := ch.links[name]
		// Before each link
		for _, mw := range ch.middlewares {
			if err := mw.Before(ctx, link, currentCtx); err != nil {
				// On error
				for _, mwErr := range ch.middlewares {
					_ = mwErr.OnError(ctx, link, err, currentCtx)
				}
				return nil, err
			}
		}

		// Execute link
		resultCtx, err := link.Call(ctx, currentCtx)
		if err != nil {
			// On error - call all middlewares but don't suppress by default
			for _, mwErr := range ch.middlewares {
				_ = mwErr.OnError(ctx, link, err, currentCtx)
			}
			return nil, err
		}
		currentCtx = resultCtx

		// After each link
		for _, mw := range ch.middlewares {
			if err := mw.After(ctx, link, currentCtx); err != nil {
				return nil, err
			}
		}
	}

	// Final after hooks
	for _, mw := range ch.middlewares {
		if err := mw.After(ctx, nil, currentCtx); err != nil {
			return nil, err
		}
	}

	return currentCtx, nil
}

// ErrorHandlingMixin provides compassionate error routing
type ErrorHandlingMixin struct {
	ErrorConnections []ErrorConnection
}

// ErrorConnection represents an error routing rule
type ErrorConnection struct {
	Source    string
	Handler   string
	Condition func(error) bool
}

// NewErrorHandlingMixin creates a new error handling mixin
func NewErrorHandlingMixin() *ErrorHandlingMixin {
	return &ErrorHandlingMixin{
		ErrorConnections: make([]ErrorConnection, 0),
	}
}

// OnError adds an error routing rule
func (ehm *ErrorHandlingMixin) OnError(source, handler string, condition func(error) bool) {
	ehm.ErrorConnections = append(ehm.ErrorConnections, ErrorConnection{
		Source:    source,
		Handler:   handler,
		Condition: condition,
	})
}

// HandleError finds and calls the appropriate error handler
func (ehm *ErrorHandlingMixin) HandleError(linkName string, err error, ctx *Context[any], links map[string]Link[any, any]) (*Context[any], error) {
	for _, conn := range ehm.ErrorConnections {
		if conn.Source == linkName && conn.Condition(err) {
			if handler, exists := links[conn.Handler]; exists {
				ctxWithError := ctx.Insert("error", err.Error())
				return handler.Call(context.Background(), ctxWithError)
			}
		}
	}
	return nil, nil // No handler found
}

// RetryLink provides forgiveness through retries
type RetryLink struct {
	Inner      Link[any, any]
	MaxRetries int
}

// NewRetryLink creates a new retry link
func NewRetryLink(inner Link[any, any], maxRetries int) *RetryLink {
	return &RetryLink{
		Inner:      inner,
		MaxRetries: maxRetries,
	}
}

// Call implements the Link interface with retry logic
func (rl *RetryLink) Call(ctx context.Context, c *Context[any]) (*Context[any], error) {
	var lastErr error

	for attempt := 0; attempt <= rl.MaxRetries; attempt++ {
		result, err := rl.Inner.Call(ctx, c)
		if err == nil {
			return result, nil
		}
		lastErr = err

		if attempt == rl.MaxRetries {
			return c.Insert("error", lastErr.Error()), lastErr
		}
	}

	// This point is never reached due to the early return above
	// when attempt == rl.MaxRetries, but Go requires a return statement
	return nil, lastErr
}
