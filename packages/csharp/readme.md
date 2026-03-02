# CodeUChain C#

A modular framework for chaining processing links with hook support, designed for robust .NET applications.

## 🤖 LLM Support

This package supports the [llm.txt standard](https://codeuchain.github.io/codeuchain/csharp/llm.txt) for easy AI/LLM integration. See [llm-full.txt](https://codeuchain.github.io/codeuchain/csharp/llm-full.txt) for comprehensive documentation.

## Overview

CodeUChain C# provides a clean, async-first architecture for building processing pipelines with:

- **Immutable State**: Thread-safe data passing between processing steps
- **Link Interface**: Pluggable processing units
- **Chain Orchestration**: Sequential execution with error handling
- **Hook Support**: Cross-cutting concerns like logging, authentication, etc.

## Installation

### NuGet Package
```bash
dotnet add package CodeUChain --version 1.0.0
```

### From Source
```bash
# Clone the repository
git clone https://github.com/codeuchain/codeuchain.git
cd codeuchain/packages/csharp

# Build the library
dotnet build CodeUChain.csproj

# Run the example
dotnet run --project examples/MathProcessingExample.csproj
```

## Quick Start

```csharp
using CodeUChain;

// Create a processing chain
var chain = new Chain();

// Add processing links
chain = chain.AddLink("validate", new ValidationLink());
chain = chain.AddLink("process", new ProcessingLink());
chain = chain.AddLink("save", new SaveLink());

// Add hook
chain = chain.UseHook(new LoggingHook());
chain = chain.UseHook(new ErrorHandlingHook());

// Execute the chain
var input = State.Create(new Dictionary<string, object>
{
    ["data"] = "some input"
});

var result = await chain.RunAsync(input);
```

## Core Components

### State

Immutable data container that flows through the processing chain:

```csharp
// Create state
var state = State.Create();
var stateWithData = State.Create(new Dictionary<string, object>
{
    ["key"] = "value"
});

// Manipulate data
var newState = state.Insert("newKey", "newValue");
var removedState = state.Remove("oldKey");

// Access data
var value = state.Get<string>("key");
var hasKey = state.ContainsKey("key");
```

### Link Interface

Processing units that transform the state:

```csharp
public class MyLink : ILink
{
    public async Task<State> CallAsync(State state)
    {
        // Process the state
        var data = state.Get<string>("input");
        var result = ProcessData(data);

        return state.Insert("output", result);
    }
}
```

### Hook Interface

Cross-cutting concerns that intercept execution:

```csharp
public class LoggingHook : IHook
{
    public Task<State> BeforeAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> AfterAsync(ILink? link, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return Task.FromResult(state);
    }

    public Task<State> OnErrorAsync(ILink? link, Exception exception, State state)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return Task.FromResult(state);
    }
}
```

### Chain

Orchestrator that manages link execution and hook:

```csharp
var chain = new Chain()
    .AddLink("step1", new Step1Link())
    .AddLink("step2", new Step2Link())
    .UseHook(new LoggingHook());

var result = await chain.RunAsync(inputState);
```

## Architecture Principles

CodeUChain C# emphasizes:

- **Harmony**: Clean interfaces and predictable behavior
- **Immutability**: Thread-safe data flow
- **Composability**: Easy combination of components
- **Error Resilience**: Comprehensive error handling
- **Observability**: Hook-based monitoring

## Examples

See the `examples/` directory for complete working examples:

- **MathProcessingExample**: Demonstrates basic chain execution with logging hook
- More examples coming soon...

## Testing

```bash
# Run tests (when test project is properly configured)
dotnet test
```

## Contributing

1. Follow the established patterns from other language implementations
2. Maintain immutability and async-first design
3. Add comprehensive tests for new features
4. Update documentation

## License

See the main repository for licensing information.
