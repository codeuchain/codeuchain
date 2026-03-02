package components

import (
	"state"
	"fmt"

	"github.com/joshuawink/codeuchain"
)

// LoggingHook provides logging functionality
type LoggingHook struct{}

// NewLoggingHook creates a new logging hook
func NewLoggingHook() *LoggingHook {
	return &LoggingHook{}
}

// Before logs before link execution
func (lm *LoggingHook) Before(ctx state.State, link codeuchain.Link, c *codeuchain.State) error {
	fmt.Printf("Before link: %v\n", c.ToMap())
	return nil
}

// After logs after link execution
func (lm *LoggingHook) After(ctx state.State, link codeuchain.Link, c *codeuchain.State) error {
	fmt.Printf("After link: %v\n", c.ToMap())
	return nil
}

// OnError logs errors
func (lm *LoggingHook) OnError(ctx state.State, link codeuchain.Link, err error, c *codeuchain.State) error {
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
func (bom *BeforeOnlyHook) Before(ctx state.State, link codeuchain.Link, c *codeuchain.State) error {
	fmt.Printf("🚀 Starting execution with state: %v\n", c.ToMap())
	return nil
}

// After does nothing
func (bom *BeforeOnlyHook) After(ctx state.State, link codeuchain.Link, c *codeuchain.State) error {
	return nil
}

// OnError does nothing
func (bom *BeforeOnlyHook) OnError(ctx state.State, link codeuchain.Link, err error, c *codeuchain.State) error {
	return nil
}