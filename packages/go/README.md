# CodeUChain Go: Agape-Optimized Implementation

With selfless love, CodeUChain chains your code as links, observes with middleware, and flows through forgiving contexts.

## Features
- **Context:** Immutable by default, mutable for flexibility—embracing Go's interface{} approach.
- **Link:** Selfless processors with context support.
- **Chain:** Harmonious connectors with conditional flows.
- **Middleware:** Gentle enhancers, optional and forgiving.
- **Error Handling:** Compassionate routing and retries.

## Installation
```bash
go get github.com/joshuawink/codeuchain/go
```

## Quick Start
```go
package main

import (
    "context"
    "fmt"

    "github.com/joshuawink/codeuchain/go"
    "github.com/joshuawink/codeuchain/go/examples"
)

func main() {
    // Create a chain
    chain := examples.NewBasicChain()

    // Add processing links
    chain.AddLink("math", examples.NewMathLink("sum"))
    chain.UseMiddleware(examples.NewLoggingMiddleware())

    // Create context
    data := map[string]interface{}{
        "numbers": []interface{}{1.0, 2.0, 3.0},
    }
    ctx := codeuchain.NewContext(data)

    // Run the chain
    result, err := chain.Run(context.Background(), ctx)
    if err != nil {
        fmt.Printf("Error: %v\n", err)
        return
    }

    fmt.Printf("Result: %v\n", result.Get("result")) // 6.0
}
```

## Architecture

### Core Package (`codeuchain/`)
- **`Context`**: Immutable data container with map-based storage
- **`MutableContext`**: Mutable variant for performance-critical sections
- **`Link`**: Interface for processing units
- **`Chain`**: Orchestrator for link execution
- **`Middleware`**: Interface for cross-cutting concerns

### Examples Package (`examples/`)
- **MathLink**: Mathematical operations (sum, mean, max, min)
- **LoggingMiddleware**: Request/response logging
- **TimingMiddleware**: Performance monitoring
- **BasicChain**: Concrete chain implementation

### Utilities
- **ErrorHandlingMixin**: Compassionate error routing
- **RetryLink**: Forgiveness through retries

## Usage Patterns

### 1. Basic Usage
```go
chain := codeuchain.NewChain()
chain.AddLink("process", myLink)
chain.UseMiddleware(loggingMiddleware)

result, err := chain.Run(context.Background(), initialContext)
```

### 2. Custom Components
```go
type MyLink struct{}

func (ml *MyLink) Call(ctx context.Context, c *codeuchain.Context) (*codeuchain.Context, error) {
    // Your processing logic
    return c.Insert("result", "processed"), nil
}
```

### 3. Error Handling
```go
ehm := codeuchain.NewErrorHandlingMixin()
ehm.OnError("failing_link", "error_handler", func(err error) bool {
    return strings.Contains(err.Error(), "specific_error")
})
```

### 4. Retry Logic
```go
retryLink := codeuchain.NewRetryLink(myLink, 3)
// Will retry up to 3 times on failure
```

## Examples

### Simple Math Chain
```bash
cd examples
go run simple_math.go
```

This demonstrates:
- Chain setup with multiple links
- Conditional connections
- Middleware usage
- Result processing

## Testing
```bash
go test ./...
```

## Agape Philosophy
Optimized for Go's concurrency and interface model—forgiving, context-aware, ecosystem-integrated. Start fresh, chain with love.

## Contributing
1. Follow the agape philosophy: selfless, compassionate code
2. Add tests for new functionality
3. Update documentation
4. Maintain immutability principles

## License
MIT