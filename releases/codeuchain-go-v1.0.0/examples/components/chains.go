package components

import (
	"context"

	"github.com/joshuawink/codeuchain"
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
func (bc *BasicChain) AddLink(name string, link codeuchain.Link) {
	bc.chain.AddLink(name, link)
}

// Connect adds a connection between links
func (bc *BasicChain) Connect(source, target string, condition func(*codeuchain.Context) bool) {
	bc.chain.Connect(source, target, condition)
}

// UseMiddleware adds middleware to the chain
func (bc *BasicChain) UseMiddleware(mw codeuchain.Middleware) {
	bc.chain.UseMiddleware(mw)
}

// Run executes the chain
func (bc *BasicChain) Run(ctx context.Context, initialCtx *codeuchain.Context) (*codeuchain.Context, error) {
	return bc.chain.Run(ctx, initialCtx)
}