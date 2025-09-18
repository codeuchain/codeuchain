package components

import (
	"context"

	codeuchain "github.com/codeuchain/codeuchain/packages/go"
)

// BasicChain provides a concrete implementation of chain orchestration
type BasicChain struct {
	chain *codeuchain.Chain
}

// NewBasicChain creates a new basic chain
func NewBasicChain() *BasicChain {
	return &BasicChain{
		chain: codeuchain.NewChain(),
	}
}

// AddLink adds a link to the chain
func (bc *BasicChain) AddLink(name string, link codeuchain.Link[any, any]) {
	bc.chain.AddLink(name, link)
}

// Connect adds a connection between links
func (bc *BasicChain) Connect(source, target string, condition func(*codeuchain.Context[any]) bool) {
	bc.chain.Connect(source, target, condition)
}

// UseMiddleware adds middleware to the chain
func (bc *BasicChain) UseMiddleware(mw codeuchain.Middleware[any, any]) {
	bc.chain.UseMiddleware(mw)
}

// Run executes the chain
func (bc *BasicChain) Run(ctx context.Context, initialCtx *codeuchain.Context[any]) (*codeuchain.Context[any], error) {
	return bc.chain.Run(ctx, initialCtx)
}
