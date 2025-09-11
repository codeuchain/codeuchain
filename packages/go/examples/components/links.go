package components

import (
	"context"

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

// MathLink performs mathematical operations
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
	if numbersSlice, ok := numbersVal.([]interface{}); ok {
		numbers := make([]float64, 0, len(numbersSlice))
		for _, v := range numbersSlice {
			if num, ok := v.(float64); ok {
				numbers = append(numbers, num)
			}
		}

		if len(numbers) == 0 {
			return c.Insert("error", "Invalid numbers"), nil
		}

		var result float64
		switch ml.Operation {
		case "sum":
			for _, n := range numbers {
				result += n
			}
		case "mean":
			for _, n := range numbers {
				result += n
			}
			result /= float64(len(numbers))
		case "max":
			result = numbers[0]
			for _, n := range numbers[1:] {
				if n > result {
					result = n
				}
			}
		case "min":
			result = numbers[0]
			for _, n := range numbers[1:] {
				if n < result {
					result = n
				}
			}
		default:
			result = 0
		}

		return c.Insert("result", result), nil
	}

	return c.Insert("error", "Invalid numbers"), nil
}