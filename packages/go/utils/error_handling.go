package utils

import (
	"context"
	"fmt"

	codeuchain "github.com/codeuchain/codeuchain/packages/go"
)

// ErrorHandlingMixin provides error routing capabilities
type ErrorHandlingMixin struct {
	ErrorConnections []ErrorConnection
}

// ErrorConnection represents error routing rules
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

// HandleError finds and executes error handler
func (ehm *ErrorHandlingMixin) HandleError(linkName string, err error, ctx *codeuchain.Context[any], links map[string]codeuchain.Link[any, any]) (*codeuchain.Context[any], error) {
	for _, conn := range ehm.ErrorConnections {
		if conn.Source == linkName && conn.Condition(err) {
			if handler, exists := links[conn.Handler]; exists {
				// Insert error info into context
				ctxWithError := ctx.Insert("error", err.Error())
				return handler.Call(context.Background(), ctxWithError)
			}
		}
	}
	return nil, fmt.Errorf("no error handler found: %w", err)
}

// RetryLink wraps a link with retry logic
type RetryLink struct {
	Inner      codeuchain.Link[any, any]
	MaxRetries int
}

// NewRetryLink creates a new retry link
func NewRetryLink(inner codeuchain.Link[any, any], maxRetries int) *RetryLink {
	return &RetryLink{
		Inner:      inner,
		MaxRetries: maxRetries,
	}
}

// Call implements the Link interface with retry logic
func (rl *RetryLink) Call(ctx context.Context, c *codeuchain.Context[any]) (*codeuchain.Context[any], error) {
	var lastErr error

	for attempt := 0; attempt <= rl.MaxRetries; attempt++ {
		result, err := rl.Inner.Call(ctx, c)
		if err == nil {
			return result, nil
		}
		lastErr = err

		if attempt == rl.MaxRetries {
			return c.Insert("error", fmt.Sprintf("Max retries exceeded: %v", err)), lastErr
		}
	}

	return c.Insert("error", fmt.Sprintf("Max retries exceeded: %v", lastErr)), lastErr
}