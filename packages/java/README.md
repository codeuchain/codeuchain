# CodeUChain Java: Enterprise-Grade Implementation

With selfless love, CodeUChain chains your code as links, observes with hook, and flows through forgiving states.

## 🤖 LLM Support

This package supports the [llm.txt standard](https://codeuchain.github.io/codeuchain/java/llm.txt) for easy AI/LLM integration. See [llm-full.txt](https://codeuchain.github.io/codeuchain/java/llm-full.txt) for comprehensive documentation.

## Features
- **State**: Immutable by default with builder pattern—embracing Java's object-oriented model
- **Link**: Functional interface for processing units
- **Chain**: Fluent API orchestrator with hook support
- **Hook**: Interface for cross-cutting concerns
- **Enterprise Ready**: Maven build, comprehensive error handling

## Installation
```xml
<dependency>
    <groupId>com.codeuchain</groupId>
    <artifactId>codeuchain-java</artifactId>
    <version>1.0.0</version>
</dependency>
```

## Quick Start
```java
// Create chain with fluent API
Chain chain = new Chain()
    .addLink("math", new MathLink("sum"))
    .useHook(new LoggingHook());

// Create state
Map<String, Object> data = new HashMap<>();
data.put("numbers", Arrays.asList(1.0, 2.0, 3.0));
State state = State.create(data);

// Run chain
State result = chain.run(state);
System.out.println("Result: " + result.get("result")); // 6.0
```

## Architecture

### Core Classes
- **`State`**: Immutable data container with fluent API
- **`Link`**: Functional interface for processing
- **`Chain`**: Orchestrator with fluent builder pattern
- **`Hook`**: Interface for cross-cutting concerns

### Enterprise Features
- **Maven Build**: Standard Java project structure
- **Jackson Integration**: JSON serialization support
- **Exception Handling**: Comprehensive error management
- **Thread Safety**: Immutable states by default

## Usage Patterns

### Basic Usage
```java
Chain chain = new Chain()
    .addLink("process", myLink)
    .useHook(loggingHook);

State result = chain.run(initialState);
```

### Custom Components
```java
public class MyLink implements Link {
    @Override
    public State call(State state) throws Exception {
        // Your processing logic
        return state.insert("result", "processed");
    }
}
```

### Error Handling
```java
public class ErrorHandlingHook implements Hook {
    @Override
    public State onError(Link link, Exception error, State state) {
        System.err.println("Error: " + error.getMessage());
        return state.insert("error", error.getMessage());
    }
}
```

## Building & Testing
```bash
# Build
mvn clean compile

# Run tests
mvn test

# Package
mvn package
```

## Design Approach
Optimized for Java's enterprise strengths—object-oriented, ecosystem-integrated, with comprehensive tooling. Start fresh, build robust processing pipelines.

## Comparison with Other Languages

| Feature | Java | Go | Rust | Python |
|---------|------|----|------|--------|
| Immutability | Builder Pattern | Struct Copy | Clone Trait | dataclasses |
| Concurrency | Threads | Goroutines | Async/Await | asyncio |
| Type Safety | Static | Static | Static | Dynamic |
| Ecosystem | Maven/Central | go.mod | Cargo | PyPI |
| Performance | High | Very High | Highest | Good |

Java brings enterprise-grade reliability and tooling to the CodeUChain ecosystem.