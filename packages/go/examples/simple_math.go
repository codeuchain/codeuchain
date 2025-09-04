package main

import (
	"context"
	"fmt"

	"codeuchain/examples"
)

func main() {
	// Lovingly set up the chain using component implementations
	chain := examples.NewBasicChain()
	chain.AddLink("sum", examples.NewMathLink("sum"))
	chain.AddLink("mean", examples.NewMathLink("mean"))
	chain.Connect("sum", "mean", func(ctx *codeuchain.Context) bool {
		return ctx.Get("result") != nil
	})
	chain.UseMiddleware(examples.NewLoggingMiddleware())

	// Run with initial context
	data := map[string]interface{}{
		"numbers": []interface{}{1.0, 2.0, 3.0, 4.0, 5.0},
	}
	ctx := codeuchain.NewContext(data)

	result, err := chain.Run(context.Background(), ctx)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	fmt.Printf("Final result: %v\n", result.Get("result"))
	fmt.Printf("Full context: %v\n", result.ToMap())
}