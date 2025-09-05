// Package examples provides concrete implementations of CodeUChain interfaces
package examples

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/codeuchain/codeuchain/packages/go"
)

// IdentityLink does nothing - pure love
type IdentityLink struct{}

// NewIdentityLink creates a new identity link
func NewIdentityLink() *IdentityLink {
	return &IdentityLink{}
}

// Call implements the Link interface
func (il *IdentityLink) Call(ctx context.Context, c *codeuchain.Context[any]) (*codeuchain.Context[any], error) {
	return c, nil
}

// MathLink provides math operations
type MathLink struct {
	Operation string
}

// NewMathLink creates a new math link
func NewMathLink(operation string) *MathLink {
	return &MathLink{Operation: operation}
}

// Call implements the Link interface
func (ml *MathLink) Call(ctx context.Context, c *codeuchain.Context[any]) (*codeuchain.Context[any], error) {
	numbersVal := c.Get("numbers")
	numbers, ok := numbersVal.([]interface{})
	if !ok {
		return c.Insert("error", "Invalid numbers"), fmt.Errorf("invalid numbers")
	}

	if len(numbers) == 0 {
		return c.Insert("error", "Empty numbers array"), fmt.Errorf("empty numbers array")
	}

	var result float64
	switch ml.Operation {
	case "sum":
		for _, num := range numbers {
			if n, ok := num.(float64); ok {
				result += n
			}
		}
	case "mean":
		var sum float64
		for _, num := range numbers {
			if n, ok := num.(float64); ok {
				sum += n
			}
		}
		result = sum / float64(len(numbers))
	case "max":
		result = numbers[0].(float64)
		for _, num := range numbers {
			if n, ok := num.(float64); ok && n > result {
				result = n
			}
		}
	case "min":
		result = numbers[0].(float64)
		for _, num := range numbers {
			if n, ok := num.(float64); ok && n < result {
				result = n
			}
		}
	default:
		result = 0
	}

	return c.Insert("result", result), nil
}

// LoggingMiddleware provides logging functionality
type LoggingMiddleware struct{}

// NewLoggingMiddleware creates a new logging middleware
func NewLoggingMiddleware() *LoggingMiddleware {
	return &LoggingMiddleware{}
}

// Before implements the Middleware interface
func (lm *LoggingMiddleware) Before(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.Context[any]) error {
	if link != nil {
		log.Printf("Before link execution: %v", c.ToMap())
	} else {
		log.Printf("Starting chain execution: %v", c.ToMap())
	}
	return nil
}

// After implements the Middleware interface
func (lm *LoggingMiddleware) After(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.Context[any]) error {
	if link != nil {
		log.Printf("After link execution: %v", c.ToMap())
	} else {
		log.Printf("Chain execution completed: %v", c.ToMap())
	}
	return nil
}

// OnError implements the Middleware interface
func (lm *LoggingMiddleware) OnError(ctx context.Context, link codeuchain.Link[any, any], err error, c *codeuchain.Context[any]) error {
	log.Printf("Error in execution: %v, context: %v", err, c.ToMap())
	return nil
}

// TimingMiddleware provides timing functionality
type TimingMiddleware struct {
	StartTimes map[string]time.Time
}

// NewTimingMiddleware creates a new timing middleware
func NewTimingMiddleware() *TimingMiddleware {
	return &TimingMiddleware{
		StartTimes: make(map[string]time.Time),
	}
}

// Before implements the Middleware interface
func (tm *TimingMiddleware) Before(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.Context[any]) error {
	if link != nil {
		// Use a simple string representation for timing
		linkKey := fmt.Sprintf("%p", link)
		tm.StartTimes[linkKey] = time.Now()
	}
	return nil
}

// After implements the Middleware interface
func (tm *TimingMiddleware) After(ctx context.Context, link codeuchain.Link[any, any], c *codeuchain.Context[any]) error {
	if link != nil {
		linkKey := fmt.Sprintf("%p", link)
		if startTime, exists := tm.StartTimes[linkKey]; exists {
			duration := time.Since(startTime)
			log.Printf("Link execution took %v", duration)
			delete(tm.StartTimes, linkKey)
		}
	}
	return nil
}

// OnError implements the Middleware interface
func (tm *TimingMiddleware) OnError(ctx context.Context, link codeuchain.Link[any, any], err error, c *codeuchain.Context[any]) error {
	if link != nil {
		linkKey := fmt.Sprintf("%p", link)
		if startTime, exists := tm.StartTimes[linkKey]; exists {
			duration := time.Since(startTime)
			log.Printf("Error after %v: %v", duration, err)
			delete(tm.StartTimes, linkKey)
		}
	}
	return nil
}

// BasicChain provides a concrete implementation of Chain
type BasicChain struct {
	*codeuchain.Chain
}

// NewBasicChain creates a new basic chain
func NewBasicChain() *BasicChain {
	return &BasicChain{
		Chain: codeuchain.NewChain(),
	}
}

// SimpleMathExample demonstrates basic chain usage
func SimpleMathExample() {
	// Create a chain
	chain := NewBasicChain()

	// Add math processing links
	chain.AddLink("sum", NewMathLink("sum"))
	chain.AddLink("mean", NewMathLink("mean"))

	// Connect links conditionally
	chain.Connect("sum", "mean", func(ctx *codeuchain.Context[any]) bool {
		return ctx.Get("result") != nil
	})

	// Add middleware
	chain.UseMiddleware(NewLoggingMiddleware())

	// Create input data
	data := map[string]interface{}{
		"numbers": []interface{}{1.0, 2.0, 3.0, 4.0, 5.0},
	}
	ctx := codeuchain.NewContext[any](data)

	// Run the chain
	result, err := chain.Run(context.Background(), ctx)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	fmt.Printf("Final result: %v\n", result.Get("result"))
	fmt.Printf("Full context: %v\n", result.ToMap())
}

// MiddlewareExample demonstrates middleware usage
func MiddlewareExample() {
	chain := NewBasicChain()

	// Add a simple processing link
	chain.AddLink("process", NewIdentityLink())

	// Add multiple middleware
	chain.UseMiddleware(NewLoggingMiddleware())
	chain.UseMiddleware(NewTimingMiddleware())

	// Create context
	data := map[string]interface{}{
		"input": "test data",
	}
	ctx := codeuchain.NewContext[any](data)

	// Run with middleware
	result, err := chain.Run(context.Background(), ctx)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	fmt.Printf("Processed result: %v\n", result.ToMap())
}