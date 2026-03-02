package components

import (
	"context"
	"fmt"

	codeuchain "github.com/codeuchain/codeuchain/packages/go"
)

// LoggingHook provides logging functionality
type LoggingHook struct{}

// NewLoggingHook creates a new logging hook
func NewLoggingHook() *LoggingHook {
	return &LoggingHook{}
}

// Before logs before link execution
func (lm *LoggingHook) Before(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	fmt.Printf("Before link: %v\n", c.ToMap())
	return nil
}

// After logs after link execution
func (lm *LoggingHook) After(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	fmt.Printf("After link: %v\n", c.ToMap())
	return nil
}

// OnError logs errors
func (lm *LoggingHook) OnError(ctx context.Context, link codeuchain.Link[any, any], err error, c *codeuchain.State[any]) error {
	fmt.Printf("Error in link: %v\n", err)
	return nil
}

// BeforeOnlyHook only implements Before
type BeforeOnlyHook struct{}

// NewBeforeOnlyHook creates a new before-only hook
func NewBeforeOnlyHook() *BeforeOnlyHook {
	return &BeforeOnlyHook{}
}

// Before logs before execution
func (bom *BeforeOnlyHook) Before(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	fmt.Printf("🚀 Starting execution with state: %v\n", c.ToMap())
	return nil
}

// After does nothing
func (bom *BeforeOnlyHook) After(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	return nil
}

// OnError does nothing
func (bom *BeforeOnlyHook) OnError(ctx context.Context, link codeuchain.Link[any, any], err error, c *codeuchain.State[any]) error {
	return nil
}