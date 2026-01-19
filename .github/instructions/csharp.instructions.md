---
applyTo: 'packages/csharp/**'
---

# C# Development Instructions

## 🔷 C#-Specific Guidelines

CodeUChain's C# implementation provides strong static typing with covariance support and seamless .NET integration.

## Project Structure

```
packages/csharp/
├── src/
│   ├── Core/          # Core framework (Context, Link, Chain)
│   ├── Middleware/    # Middleware implementations
│   └── CodeUChain.csproj
├── tests/            # xUnit/NUnit test suite
├── examples/         # Example implementations
└── CodeUChain.sln   # Solution file
```

## Development Setup

### Prerequisites
- .NET 6.0 or higher
- Visual Studio 2022, VS Code, or Rider

### Installation
```bash
cd packages/csharp
dotnet restore       # Restore dependencies
dotnet build         # Build project
```

### Running Tests
```bash
cd packages/csharp
dotnet test                      # Run all tests
dotnet test --verbosity normal   # Verbose output
dotnet test --filter TestName    # Run specific test
dotnet test --collect:"XPlat Code Coverage"  # With coverage
```

### Running Linters
```bash
cd packages/csharp
dotnet format        # Format code
dotnet build /p:TreatWarningsAsErrors=true  # Strict build
```

## Code Style

### Namespace and Using
```csharp
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace CodeUChain.Core
{
    /// <summary>
    /// Public class with XML documentation
    /// </summary>
    public class Context<T>
    {
        private readonly Dictionary<string, object> _data;
        
        // Implementation
    }
}
```

### Nullable Reference Types
```csharp
#nullable enable

public class MyLink : ILink<InputData, OutputData>
{
    public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
    {
        // value could be null
        string? value = context.Get<string>("key");
        
        if (value is null)
        {
            throw new InvalidOperationException("Key not found");
        }
        
        return context.InsertAs<OutputData>("result", value);
    }
}
```

### XML Documentation
```csharp
/// <summary>
/// Processes input data and transforms context.
/// </summary>
/// <param name="context">Context containing input data.</param>
/// <returns>Task containing transformed context.</returns>
/// <exception cref="InvalidOperationException">Thrown when input is invalid.</exception>
/// <example>
/// <code>
/// var ctx = new Context&lt;InputData&gt;(new { numbers = new[] { 1, 2, 3 } });
/// var link = new SumLink();
/// var result = await link.CallAsync(ctx);
/// </code>
/// </example>
public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
{
    // Implementation
}
```

## Testing Patterns

### Basic Test Structure (xUnit)
```csharp
using Xunit;
using CodeUChain.Core;

public class MyLinkTests
{
    [Fact]
    public async Task CallAsync_WithValidInput_ReturnsExpectedResult()
    {
        // Arrange
        var context = new Context<InputData>(new { input = "value" });
        var link = new MyLink();
        
        // Act
        var result = await link.CallAsync(context);
        
        // Assert
        Assert.Equal("expected", result.Get<string>("output"));
    }
}
```

### Theory Tests (Data-Driven)
```csharp
[Theory]
[InlineData(new[] { 1, 2, 3 }, 6)]
[InlineData(new[] { 5, 5 }, 10)]
[InlineData(new int[] { }, 0)]
public async Task SumLink_CalculatesCorrectSum(int[] numbers, int expected)
{
    // Arrange
    var context = new Context<InputData>(new { numbers });
    var link = new SumLink();
    
    // Act
    var result = await link.CallAsync(context);
    
    // Assert
    Assert.Equal(expected, result.Get<int>("result"));
}
```

### Testing Exceptions
```csharp
[Fact]
public async Task CallAsync_WithInvalidInput_ThrowsException()
{
    // Arrange
    var context = new Context<InputData>(new { invalid = "data" });
    var link = new RiskyLink();
    
    // Act & Assert
    await Assert.ThrowsAsync<InvalidOperationException>(
        () => link.CallAsync(context)
    );
}
```

## Common Patterns

### Creating a Simple Link
```csharp
using System.Threading.Tasks;

namespace CodeUChain.Links
{
    public class MyLink : ILink<object, object>
    {
        public async Task<Context<object>> CallAsync(Context<object> context)
        {
            // Get value from context
            var value = context.Get<string>("input_key");
            
            // Process value
            var result = await ProcessAsync(value);
            
            // Return new context with result
            return context.Insert("output_key", result);
        }
        
        private async Task<string> ProcessAsync(string? value)
        {
            // Your async processing logic
            await Task.Delay(10);
            return value ?? string.Empty;
        }
    }
}
```

### Creating a Generic Link
```csharp
public class InputData
{
    public int[] Numbers { get; set; } = Array.Empty<int>();
}

public class OutputData
{
    public double Result { get; set; }
}

public class SumLink : ILink<InputData, OutputData>
{
    public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
    {
        var numbers = context.Get<int[]>("numbers") 
            ?? throw new InvalidOperationException("Numbers not found");
        
        var total = numbers.Sum();
        
        return context.InsertAs<OutputData>("result", (double)total);
    }
}
```

### Creating a Chain
```csharp
using CodeUChain.Core;

// Simple chain
var chain = new Chain<InputData, OutputData>(
    new Link1(),
    new Link2(),
    new Link3()
);

// Execute chain
var initialContext = new Context<InputData>(new { input = "data" });
var result = await chain.ExecuteAsync(initialContext);
```

### Using Middleware
```csharp
using CodeUChain.Middleware;

var chain = new Chain<InputData, OutputData>(
    new[] { new Link1(), new Link2() },
    middleware: new IMiddleware[] { new LoggingMiddleware() }
);
```

## Type Evolution

### Using Insert() - Preserves Type
```csharp
var context = new Context<InputData>(new { numbers = new[] { 1, 2, 3 } });
// Type is still Context<InputData>
var context2 = context.Insert("extra", "value");
```

### Using InsertAs() - Changes Type
```csharp
var context = new Context<InputData>(new { numbers = new[] { 1, 2, 3 } });
// Type is now Context<OutputData>
var context2 = context.InsertAs<OutputData>("result", 6.0);
```

## Async/Await Best Practices

### ConfigureAwait
```csharp
public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
{
    // In library code, use ConfigureAwait(false)
    var result = await ProcessAsync(context).ConfigureAwait(false);
    return context.InsertAs<OutputData>("result", result);
}
```

### Cancellation Tokens
```csharp
public async Task<Context<OutputData>> CallAsync(
    Context<InputData> context,
    CancellationToken cancellationToken = default)
{
    cancellationToken.ThrowIfCancellationRequested();
    
    var result = await ProcessAsync(context, cancellationToken)
        .ConfigureAwait(false);
    
    return context.InsertAs<OutputData>("result", result);
}
```

### Parallel Execution
```csharp
public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
{
    var tasks = new[]
    {
        Process1Async(context),
        Process2Async(context),
        Process3Async(context)
    };
    
    var results = await Task.WhenAll(tasks).ConfigureAwait(false);
    
    // Combine results
    return context.InsertAs<OutputData>("results", results);
}
```

## Error Handling

### Custom Exceptions
```csharp
public class LinkException : Exception
{
    public LinkException(string message) : base(message) { }
    
    public LinkException(string message, Exception innerException) 
        : base(message, innerException) { }
}
```

### Try-Catch in Links
```csharp
public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
{
    try
    {
        var result = await RiskyOperationAsync(context).ConfigureAwait(false);
        return context.InsertAs<OutputData>("result", result);
    }
    catch (ArgumentException ex)
    {
        throw new LinkException("Invalid input", ex);
    }
    catch (Exception ex)
    {
        throw new LinkException("Processing failed", ex);
    }
}
```

## LINQ Integration

### Using LINQ with Context
```csharp
var context = new Context<InputData>(new { numbers = new[] { 1, 2, 3, 4, 5 } });

var numbers = context.Get<int[]>("numbers") ?? Array.Empty<int>();
var result = numbers
    .Where(n => n > 2)
    .Select(n => n * 2)
    .Sum();

return context.InsertAs<OutputData>("result", result);
```

## Best Practices

### Context Management
- **Immutable**: Always create new Context instances
- **Null Safety**: Use nullable reference types
- **Generic Types**: Leverage strong typing

### Link Design
- **Single Responsibility**: Each link does one thing
- **Async by Default**: Use async/await consistently
- **Interface Segregation**: Implement ILink<TIn, TOut>

### Performance
- **Avoid Boxing**: Use generics instead of object
- **ValueTask**: Consider for hot paths
- **Span<T>**: Use for memory-efficient operations
- **ArrayPool**: Reuse arrays when possible

### Code Quality
- **XML Documentation**: Document all public APIs
- **Nullable Annotations**: Enable and use properly
- **Code Analysis**: Enable all analyzers
- **EditorConfig**: Use consistent formatting

## Debugging

### Debug Output
```csharp
var context = new Context<InputData>(new { key = "value" });
System.Diagnostics.Debug.WriteLine($"Context: {context.ToDictionary()}");
```

### Conditional Compilation
```csharp
public async Task<Context<OutputData>> CallAsync(Context<InputData> context)
{
#if DEBUG
    Console.WriteLine($"Processing: {context.Get<string>("key")}");
#endif
    
    var result = await ProcessAsync(context).ConfigureAwait(false);
    return result;
}
```

### Visual Studio Debugger
- Set breakpoints with F9
- Step through with F10 (over) and F11 (into)
- View variables in Locals/Watch windows
- Use Immediate Window for expressions

## Common Issues

### Covariance Issues
```csharp
// Covariant interface
public interface ILink<in TInput, out TOutput>
{
    Task<Context<TOutput>> CallAsync(Context<TInput> context);
}

// Allows this
ILink<object, OutputData> link = new MyLink<InputData, OutputData>();
```

### Async Void
```csharp
// DON'T - async void
public async void ProcessAsync()  // ❌
{
    await Task.Delay(100);
}

// DO - async Task
public async Task ProcessAsync()  // ✅
{
    await Task.Delay(100);
}
```

### Deadlocks
```csharp
// DON'T - can deadlock in UI apps
var result = SomeAsyncMethod().Result;  // ❌

// DO - use await
var result = await SomeAsyncMethod();   // ✅
```

## Building and Distribution

### Build Project
```bash
dotnet build --configuration Release
```

### Create NuGet Package
```bash
dotnet pack --configuration Release
```

### Publishing
```bash
dotnet nuget push bin/Release/CodeUChain.*.nupkg \
    --api-key YOUR_API_KEY \
    --source https://api.nuget.org/v3/index.json
```

## Reference

- **Core Implementation**: `src/Core/`
- **Test Examples**: `tests/`
- **C# Documentation**: https://docs.microsoft.com/en-us/dotnet/csharp/
- **.NET API Reference**: https://docs.microsoft.com/en-us/dotnet/api/
