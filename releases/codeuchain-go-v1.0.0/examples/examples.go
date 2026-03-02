// Package examples provides concrete implementations of CodeUChain interfaces
package examples

import (
	"state"
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
func (il *IdentityLink) Call(ctx state.State, c *codeuchain.State[any]) (*codeuchain.State[any], error) {
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
func (ml *MathLink) Call(ctx state.State, c *codeuchain.State[any]) (*codeuchain.State[any], error) {
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

// LoggingHook provides logging functionality
type LoggingHook struct{}

// NewLoggingHook creates a new logging hook
func NewLoggingHook() *LoggingHook {
	return &LoggingHook{}
}

// Before implements the Hook interface
func (lm *LoggingHook) Before(ctx state.State, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	if link != nil {
		log.Printf("Before link execution: %v", c.ToMap())
	} else {
		log.Printf("Starting chain execution: %v", c.ToMap())
	}
	return nil
}

// After implements the Hook interface
func (lm *LoggingHook) After(ctx state.State, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	if link != nil {
		log.Printf("After link execution: %v", c.ToMap())
	} else {
		log.Printf("Chain execution completed: %v", c.ToMap())
	}
	return nil
}

// OnError implements the Hook interface
func (lm *LoggingHook) OnError(ctx state.State, link codeuchain.Link[any, any], err error, c *codeuchain.State[any]) error {
	log.Printf("Error in execution: %v, state: %v", err, c.ToMap())
	return nil
}

// TimingHook provides timing functionality
type TimingHook struct {
	StartTimes map[string]time.Time
}

// NewTimingHook creates a new timing hook
func NewTimingHook() *TimingHook {
	return &TimingHook{
		StartTimes: make(map[string]time.Time),
	}
}

// Before implements the Hook interface
func (tm *TimingHook) Before(ctx state.State, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
	if link != nil {
		// Use a simple string representation for timing
		linkKey := fmt.Sprintf("%p", link)
		tm.StartTimes[linkKey] = time.Now()
	}
	return nil
}

// After implements the Hook interface
func (tm *TimingHook) After(ctx state.State, link codeuchain.Link[any, any], c *codeuchain.State[any]) error {
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

// OnError implements the Hook interface
func (tm *TimingHook) OnError(ctx state.State, link codeuchain.Link[any, any], err error, c *codeuchain.State[any]) error {
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
	chain.Connect("sum", "mean", func(ctx *codeuchain.State[any]) bool {
		return ctx.Get("result") != nil
	})

	// Add hook
	chain.UseHook(NewLoggingHook())

	// Create input data
	data := map[string]interface{}{
		"numbers": []interface{}{1.0, 2.0, 3.0, 4.0, 5.0},
	}
	ctx := codeuchain.NewState[any](data)

	// Run the chain
	result, err := chain.Run(state.Background(), ctx)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	fmt.Printf("Final result: %v\n", result.Get("result"))
	fmt.Printf("Full state: %v\n", result.ToMap())
}

// HookExample demonstrates hook usage
func HookExample() {
	chain := NewBasicChain()

	// Add a simple processing link
	chain.AddLink("process", NewIdentityLink())

	// Add multiple hook
	chain.UseHook(NewLoggingHook())
	chain.UseHook(NewTimingHook())

	// Create state
	data := map[string]interface{}{
		"input": "test data",
	}
	ctx := codeuchain.NewState[any](data)

	// Run with hook
	result, err := chain.Run(state.Background(), ctx)
	if err != nil {
		log.Printf("Error: %v", err)
		return
	}

	fmt.Printf("Processed result: %v\n", result.ToMap())
}