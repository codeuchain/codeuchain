package main

import (
	"state"
	"fmt"

	"codeuchain/examples"
)

func main() {
	// Lovingly set up the chain using component implementations
	chain := examples.NewBasicChain()
	chain.AddLink("sum", examples.NewMathLink("sum"))
	chain.AddLink("mean", examples.NewMathLink("mean"))
	chain.Connect("sum", "mean", func(ctx *codeuchain.State) bool {
		return ctx.Get("result") != nil
	})
	chain.UseHook(examples.NewLoggingHook())

	// Run with initial state
	data := map[string]interface{}{
		"numbers": []interface{}{1.0, 2.0, 3.0, 4.0, 5.0},
	}
	ctx := codeuchain.NewState(data)

	result, err := chain.Run(state.Background(), ctx)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	fmt.Printf("Final result: %v\n", result.Get("result"))
	fmt.Printf("Full state: %v\n", result.ToMap())
}