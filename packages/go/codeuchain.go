// Package codeuchain provides a modular framework for chaining processing links
// with middleware support, embracing the agape philosophy of selfless design.
package codeuchain

import (
	"context"
)

// Context holds data tenderly, immutable by default for safety, mutable for flexibility.
// With agape compassion, it embraces Go's map-based approach with JSON marshaling.
type Context struct {
	data map[string]interface{}
}

// NewContext creates a new context with initial data
func NewContext(data map[string]interface{}) *Context {
	if data == nil {
		data = make(map[string]interface{})
	}
	return &Context{data: data}
}

// Get returns the value for the given key, forgiving absence with nil
func (c *Context) Get(key string) interface{} {
	return c.data[key]
}

// Insert returns a fresh context with the addition, maintaining immutability
func (c *Context) Insert(key string, value interface{}) *Context {
	newData := make(map[string]interface{})
	for k, v := range c.data {
		newData[k] = v
	}
	newData[key] = value
	return &Context{data: newData}
}

// Merge combines contexts, favoring the other with compassion
func (c *Context) Merge(other *Context) *Context {
	newData := make(map[string]interface{})
	for k, v := range c.data {
		newData[k] = v
	}
	for k, v := range other.data {
		newData[k] = v
	}
	return &Context{data: newData}
}

// ToMap returns a copy of the internal data
func (c *Context) ToMap() map[string]interface{} {
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
func (mc *MutableContext) ToImmutable() *Context {
	return NewContext(mc.data)
}

// Link defines the selfless processor interface
type Link interface {
	// Call processes the context and returns a transformed context
	Call(ctx context.Context, c *Context) (*Context, error)
}

// Middleware provides optional enhancement hooks
type Middleware interface {
	// Before is called before link execution
	Before(ctx context.Context, link Link, c *Context) error
	// After is called after link execution
	After(ctx context.Context, link Link, c *Context) error
	// OnError is called when an error occurs
	OnError(ctx context.Context, link Link, err error, c *Context) error
}

// Chain orchestrates link execution with middleware
type Chain struct {
	links       map[string]Link
	connections []Connection
	middlewares []Middleware
}

// Connection represents a conditional flow between links
type Connection struct {
	Source      string
	Target      string
	Condition   func(*Context) bool
}

// NewChain creates a new empty chain
func NewChain() *Chain {
	return &Chain{
		links:       make(map[string]Link),
		connections: make([]Connection, 0),
		middlewares: make([]Middleware, 0),
	}
}

// AddLink stores a link with the given name
func (ch *Chain) AddLink(name string, link Link) {
	ch.links[name] = link
}

// Connect adds a conditional connection between links
func (ch *Chain) Connect(source, target string, condition func(*Context) bool) {
	ch.connections = append(ch.connections, Connection{
		Source:    source,
		Target:    target,
		Condition: condition,
	})
}

// UseMiddleware attaches middleware to the chain
func (ch *Chain) UseMiddleware(mw Middleware) {
	ch.middlewares = append(ch.middlewares, mw)
}

// Run executes the chain with the given context
func (ch *Chain) Run(ctx context.Context, initialCtx *Context) (*Context, error) {
	currentCtx := initialCtx

	// Execute before hooks
	for _, mw := range ch.middlewares {
		if err := mw.Before(ctx, nil, currentCtx); err != nil {
			return nil, err
		}
	}

	// Simple linear execution for now
	for _, link := range ch.links {
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
			// On error
			for _, mw := range ch.middlewares {
				_ = mw.OnError(ctx, link, err, currentCtx)
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
func (ehm *ErrorHandlingMixin) HandleError(linkName string, err error, ctx *Context, links map[string]Link) (*Context, error) {
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
	Inner     Link
	MaxRetries int
}

// NewRetryLink creates a new retry link
func NewRetryLink(inner Link, maxRetries int) *RetryLink {
	return &RetryLink{
		Inner:      inner,
		MaxRetries: maxRetries,
	}
}

// Call implements the Link interface with retry logic
func (rl *RetryLink) Call(ctx context.Context, c *Context) (*Context, error) {
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

	return c.Insert("error", "Max retries exceeded"), lastErr
}