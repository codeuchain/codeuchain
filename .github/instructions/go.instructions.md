---
applyTo: 'packages/go/**'
---

# Go Development Instructions

## 🔵 Go-Specific Guidelines

CodeUChain's Go implementation is **production-ready** with 97.5% test coverage. It uses Go 1.18+ generics for type safety.

## Project Structure

```
packages/go/
├── core/             # Core framework (Context, Link, Chain)
├── middleware/       # Middleware implementations
├── examples/         # Example implementations
└── go.mod           # Go module definition
```

## Development Setup

### Prerequisites
- Go 1.18 or higher (for generics support)

### Installation
```bash
cd packages/go
go mod download      # Download dependencies
go build ./...       # Build all packages
```

### Running Tests
```bash
cd packages/go
go test ./...                  # Run all tests
go test -v ./...              # Verbose output
go test -cover ./...          # With coverage
go test -coverprofile=coverage.out ./...  # Generate coverage file
go tool cover -html=coverage.out          # View coverage in browser
```

### Running Linters
```bash
cd packages/go
go fmt ./...         # Format code
go vet ./...         # Check for issues
golint ./...         # Run golint (if installed)
staticcheck ./...    # Run staticcheck (if installed)
```

## Code Style

### Package Structure
```go
package core

import (
    "context"
    "fmt"
)

// Public types/functions start with capital letter
type Context[T any] struct {
    data map[string]any
}

// Private types/functions start with lowercase
func newContext[T any]() *Context[T] {
    return &Context[T]{data: make(map[string]any)}
}
```

### Error Handling
```go
// Return errors, don't panic
func (l *MyLink) Call(ctx Context[InputData]) (Context[OutputData], error) {
    value, err := risky()
    if err != nil {
        return Context[OutputData]{}, fmt.Errorf("failed to process: %w", err)
    }
    return ctx.InsertAs[OutputData]("result", value), nil
}
```

### Documentation
```go
// MyLink processes input data and returns a result.
// It implements the Link interface for InputData -> OutputData transformation.
//
// Example:
//   link := &MyLink{}
//   ctx := NewContext[InputData](map[string]any{"numbers": []int{1, 2, 3}})
//   result, err := link.Call(ctx)
type MyLink struct {
    // configuration fields
}
```

## Testing Patterns

### Basic Test Structure
```go
package core

import (
    "testing"
)

func TestMyLink(t *testing.T) {
    // Arrange
    ctx := NewContext[InputData](map[string]any{
        "numbers": []int{1, 2, 3},
    })
    link := &MyLink{}
    
    // Act
    result, err := link.Call(ctx)
    
    // Assert
    if err != nil {
        t.Fatalf("unexpected error: %v", err)
    }
    
    got := result.Get("result")
    want := 6
    if got != want {
        t.Errorf("got %v, want %v", got, want)
    }
}
```

### Table-Driven Tests
```go
func TestSumLink(t *testing.T) {
    tests := []struct {
        name    string
        input   []int
        want    int
        wantErr bool
    }{
        {
            name:  "basic sum",
            input: []int{1, 2, 3},
            want:  6,
        },
        {
            name:  "empty slice",
            input: []int{},
            want:  0,
        },
    }
    
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            ctx := NewContext[InputData](map[string]any{
                "numbers": tt.input,
            })
            link := &SumLink{}
            
            result, err := link.Call(ctx)
            
            if (err != nil) != tt.wantErr {
                t.Errorf("error = %v, wantErr %v", err, tt.wantErr)
                return
            }
            
            got := result.Get("result").(int)
            if got != tt.want {
                t.Errorf("got %v, want %v", got, tt.want)
            }
        })
    }
}
```

### Benchmark Tests
```go
func BenchmarkChain(b *testing.B) {
    chain := NewChain[InputData, OutputData](
        &Link1{},
        &Link2{},
        &Link3{},
    )
    ctx := NewContext[InputData](map[string]any{"data": "test"})
    
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _, err := chain.Execute(ctx)
        if err != nil {
            b.Fatal(err)
        }
    }
}
```

## Common Patterns

### Creating a Simple Link
```go
package core

type MyLink struct {
    // Configuration fields
}

// Call implements the Link interface
func (l *MyLink) Call(ctx Context[any]) (Context[any], error) {
    // Get value from context
    value := ctx.Get("input_key")
    
    // Process value
    result, err := l.process(value)
    if err != nil {
        return Context[any]{}, err
    }
    
    // Return new context with result
    return ctx.Insert("output_key", result), nil
}

func (l *MyLink) process(value any) (any, error) {
    // Your processing logic
    return value, nil
}
```

### Creating a Generic Link
```go
package core

type InputData struct {
    Numbers []int
}

type OutputData struct {
    Result int
}

type SumLink struct{}

// Call implements Link[InputData, OutputData]
func (l *SumLink) Call(ctx Context[InputData]) (Context[OutputData], error) {
    numbers, ok := ctx.Get("numbers").([]int)
    if !ok {
        return Context[OutputData]{}, fmt.Errorf("invalid numbers type")
    }
    
    total := 0
    for _, n := range numbers {
        total += n
    }
    
    return ctx.InsertAs[OutputData]("result", total), nil
}
```

### Creating a Chain
```go
// Simple chain
chain := NewChain[InputData, OutputData](
    &Link1{},
    &Link2{},
    &Link3{},
)

// Execute chain
initialCtx := NewContext[InputData](map[string]any{"input": "data"})
result, err := chain.Execute(initialCtx)
if err != nil {
    log.Fatalf("chain execution failed: %v", err)
}
```

### Using Middleware
```go
import "github.com/codeuchain/codeuchain/middleware"

chain := NewChain[InputData, OutputData](
    &Link1{},
    &Link2{},
    WithMiddleware(&middleware.Logging{}),
)
```

## Type Evolution

### Using Insert() - Preserves Type
```go
ctx := NewContext[InputData](map[string]any{"numbers": []int{1, 2, 3}})
// Type is still Context[InputData]
ctx2 := ctx.Insert("extra", "value")
```

### Using InsertAs() - Changes Type
```go
ctx := NewContext[InputData](map[string]any{"numbers": []int{1, 2, 3}})
// Type is now Context[OutputData]
ctx2 := ctx.InsertAs[OutputData]("result", 6)
```

## Error Handling

### Link-Level Errors
```go
func (l *MyLink) Call(ctx Context[InputData]) (Context[OutputData], error) {
    value, err := l.riskyOperation(ctx)
    if err != nil {
        // Wrap error with context
        return Context[OutputData]{}, fmt.Errorf("MyLink failed: %w", err)
    }
    return ctx.InsertAs[OutputData]("result", value), nil
}
```

### Chain-Level Errors
```go
result, err := chain.Execute(ctx)
if err != nil {
    // Check error type
    var execErr *ChainExecutionError
    if errors.As(err, &execErr) {
        log.Printf("chain failed at link %d: %v", execErr.LinkIndex, execErr.Err)
    }
    return err
}
```

## Best Practices

### Context Management
- **Immutable**: Always create new Context instances
- **Type Safety**: Use generics for compile-time safety
- **Clear Keys**: Use descriptive key names

### Link Design
- **Single Responsibility**: Each link does one thing
- **Error Returns**: Always return errors, don't panic
- **Interface Compliance**: Implement Link interface correctly

### Go Idioms
- **Error Handling**: Check errors immediately
- **Short Variable Names**: Use short, clear names (ctx, err)
- **Package Names**: Use lowercase, no underscores
- **Exported Names**: Start with capital letter

### Performance
- **Avoid Allocations**: Reuse slices/maps when possible
- **Goroutines**: Use for concurrent processing
- **Defer**: Use for cleanup, but be aware of performance
- **Benchmarks**: Write benchmarks for performance-critical code

## Concurrency

### Goroutines in Links
```go
func (l *ParallelLink) Call(ctx Context[InputData]) (Context[OutputData], error) {
    results := make(chan result, 3)
    
    // Launch goroutines
    go l.process1(ctx, results)
    go l.process2(ctx, results)
    go l.process3(ctx, results)
    
    // Collect results
    var finalResult OutputData
    for i := 0; i < 3; i++ {
        r := <-results
        if r.err != nil {
            return Context[OutputData]{}, r.err
        }
        // Combine results
    }
    
    return ctx.InsertAs[OutputData]("result", finalResult), nil
}
```

### Context Cancellation
```go
import "context"

func (l *MyLink) Call(ctx Context[InputData]) (Context[OutputData], error) {
    goctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()
    
    select {
    case result := <-l.processWithContext(goctx, ctx):
        return result, nil
    case <-goctx.Done():
        return Context[OutputData]{}, goctx.Err()
    }
}
```

## Debugging

### Print Context State
```go
ctx := NewContext[InputData](map[string]any{"key": "value"})
fmt.Printf("Context: %+v\n", ctx.ToMap())
```

### Debug Link Execution
```go
type DebugLink struct{}

func (l *DebugLink) Call(ctx Context[any]) (Context[any], error) {
    fmt.Printf("Before: %+v\n", ctx.ToMap())
    result, err := l.process(ctx)
    if err != nil {
        return Context[any]{}, err
    }
    fmt.Printf("After: %+v\n", result.ToMap())
    return result, nil
}
```

### Using Delve Debugger
```bash
# Install delve
go install github.com/go-delve/delve/cmd/dlv@latest

# Debug test
dlv test -- -test.run TestMyLink

# Debug application
dlv debug main.go
```

## Common Issues

### Type Assertions
```go
// Always check type assertions
numbers, ok := ctx.Get("numbers").([]int)
if !ok {
    return Context[OutputData]{}, fmt.Errorf("invalid type for numbers")
}
```

### Generic Constraints
```go
// Use appropriate constraints
type Link[TInput any, TOutput any] interface {
    Call(ctx Context[TInput]) (Context[TOutput], error)
}
```

### Import Cycles
- Avoid circular dependencies
- Use interfaces to break cycles
- Reorganize packages if needed

## Building and Distribution

### Build Binary
```bash
go build -o bin/app ./cmd/app
```

### Cross-Compilation
```bash
GOOS=linux GOARCH=amd64 go build -o bin/app-linux ./cmd/app
GOOS=windows GOARCH=amd64 go build -o bin/app-windows.exe ./cmd/app
```

### Module Publishing
```bash
# Tag version
git tag v1.0.0
git push origin v1.0.0

# Go modules will automatically fetch from git
```

## Reference

- **Core Implementation**: `core/`
- **Test Examples**: `core/*_test.go`
- **Go Documentation**: `packages/go/README.md`
- **Go Generics**: https://go.dev/doc/tutorial/generics
