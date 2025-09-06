package components

import (
	"context"
	"fmt"

	"github.com/joshuawink/codeuchain"
)

// LoggingMiddleware provides logging functionality
type LoggingMiddleware struct{}

// NewLoggingMiddleware creates a new logging middleware
func NewLoggingMiddleware() *LoggingMiddleware {
	return &LoggingMiddleware{}
}

// Before logs before link execution
func (lm *LoggingMiddleware) Before(ctx context.Context, link codeuchain.Link, c *codeuchain.Context) error {
	fmt.Printf("Before link: %v\n", c.ToMap())
	return nil
}

// After logs after link execution
func (lm *LoggingMiddleware) After(ctx context.Context, link codeuchain.Link, c *codeuchain.Context) error {
	fmt.Printf("After link: %v\n", c.ToMap())
	return nil
}

// OnError logs errors
func (lm *LoggingMiddleware) OnError(ctx context.Context, link codeuchain.Link, err error, c *codeuchain.Context) error {
	fmt.Printf("Error in link: %v\n", err)
	return nil
}

// BeforeOnlyMiddleware only implements Before
type BeforeOnlyMiddleware struct{}

// NewBeforeOnlyMiddleware creates a new before-only middleware
func NewBeforeOnlyMiddleware() *BeforeOnlyMiddleware {
	return &BeforeOnlyMiddleware{}
}

// Before logs before execution
func (bom *BeforeOnlyMiddleware) Before(ctx context.Context, link codeuchain.Link, c *codeuchain.Context) error {
	fmt.Printf("🚀 Starting execution with context: %v\n", c.ToMap())
	return nil
}

// After does nothing
func (bom *BeforeOnlyMiddleware) After(ctx context.Context, link codeuchain.Link, c *codeuchain.Context) error {
	return nil
}

// OnError does nothing
func (bom *BeforeOnlyMiddleware) OnError(ctx context.Context, link codeuchain.Link, err error, c *codeuchain.Context) error {
	return nil
}