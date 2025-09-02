# CodeUChain C#

A modular framework for chaining processing links with middleware support, following agape philosophy.

## Overview

CodeUChain C# provides a clean, async-first architecture for building processing pipelines with:

- **Immutable Context**: Thread-safe data passing between processing steps
- **Link Interface**: Pluggable processing units
- **Chain Orchestration**: Sequential execution with error handling
- **Middleware Support**: Cross-cutting concerns like logging, authentication, etc.

## Installation

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

// Add middleware
chain = chain.UseMiddleware(new LoggingMiddleware());
chain = chain.UseMiddleware(new ErrorHandlingMiddleware());

// Execute the chain
var input = Context.Create(new Dictionary<string, object>
{
    ["data"] = "some input"
});

var result = await chain.RunAsync(input);
```

## Core Components

### Context

Immutable data container that flows through the processing chain:

```csharp
// Create context
var context = Context.Create();
var contextWithData = Context.Create(new Dictionary<string, object>
{
    ["key"] = "value"
});

// Manipulate data
var newContext = context.Insert("newKey", "newValue");
var removedContext = context.Remove("oldKey");

// Access data
var value = context.Get<string>("key");
var hasKey = context.ContainsKey("key");
```

### Link Interface

Processing units that transform the context:

```csharp
public class MyLink : ILink
{
    public async Task<Context> CallAsync(Context context)
    {
        // Process the context
        var data = context.Get<string>("input");
        var result = ProcessData(data);

        return context.Insert("output", result);
    }
}
```

### Middleware Interface

Cross-cutting concerns that intercept execution:

```csharp
public class LoggingMiddleware : IMiddleware
{
    public Task<Context> BeforeAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Executing: {linkName}");
        return Task.FromResult(context);
    }

    public Task<Context> AfterAsync(ILink? link, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Completed: {linkName}");
        return Task.FromResult(context);
    }

    public Task<Context> OnErrorAsync(ILink? link, Exception exception, Context context)
    {
        var linkName = link?.GetType().Name ?? "Chain";
        Console.WriteLine($"Error in {linkName}: {exception.Message}");
        return Task.FromResult(context);
    }
}
```

### Chain

Orchestrator that manages link execution and middleware:

```csharp
var chain = new Chain()
    .AddLink("step1", new Step1Link())
    .AddLink("step2", new Step2Link())
    .UseMiddleware(new LoggingMiddleware());

var result = await chain.RunAsync(inputContext);
```

## Architecture Principles

Following agape philosophy, CodeUChain C# emphasizes:

- **Harmony**: Clean interfaces and predictable behavior
- **Immutability**: Thread-safe data flow
- **Composability**: Easy combination of components
- **Error Resilience**: Comprehensive error handling
- **Observability**: Middleware-based monitoring

## Examples

See the `examples/` directory for complete working examples:

- **MathProcessingExample**: Demonstrates basic chain execution with logging middleware
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
