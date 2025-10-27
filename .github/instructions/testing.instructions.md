---
applyTo: '**'
---

# Testing and CI/CD Instructions

## 🧪 Testing Guidelines

### Universal Testing Principles

1. **Test What You Change**: Focus on code paths affected by your changes
2. **Match Existing Patterns**: Follow the testing style of each language
3. **Edge Cases**: Test boundary conditions and error cases
4. **Backward Compatibility**: Ensure existing tests still pass

## Language-Specific Test Commands

### Python
```bash
cd packages/python
pytest                      # Run all tests
pytest -v                  # Verbose
pytest -k "test_name"      # Specific test
pytest --cov               # With coverage
```

### JavaScript/TypeScript
```bash
cd packages/javascript
npm test                   # Run all tests
npm test -- --watch       # Watch mode
npm test -- test_file     # Specific test
npm run test:coverage     # With coverage
```

### Go
```bash
cd packages/go
go test ./...             # Run all tests
go test -v ./...          # Verbose
go test -cover ./...      # With coverage
go test -run TestName     # Specific test
```

### Rust
```bash
cd packages/rust
cargo test                # Run all tests
cargo test --verbose      # Verbose
cargo test test_name      # Specific test
cargo test -- --nocapture # Show output
```

### C#
```bash
cd packages/csharp
dotnet test                      # Run all tests
dotnet test --verbosity normal   # Verbose
dotnet test --filter TestName    # Specific test
```

### Java
```bash
cd packages/java
mvn test                  # Run all tests
mvn test -Dtest=TestName  # Specific test
```

### C++
```bash
cd packages/cpp
mkdir build && cd build
cmake ..
make
ctest                     # Run all tests
ctest -V                  # Verbose
```

## Test Structure Best Practices

### Naming Conventions
- **Python**: `test_*.py` and `test_*` functions
- **JavaScript**: `*.test.js` or `*.spec.js`
- **Go**: `*_test.go` and `Test*` functions
- **Rust**: `#[test]` attribute in `tests/` or inline
- **C#**: `[Fact]` or `[Test]` attributes
- **Java**: `@Test` annotations

### Test Organization
```
tests/
├── unit/              # Unit tests (individual components)
├── integration/       # Integration tests (multiple components)
└── e2e/              # End-to-end tests (full workflows)
```

### AAA Pattern (Arrange-Act-Assert)
```python
def test_sum_link():
    # Arrange - Set up test data
    ctx = Context({"numbers": [1, 2, 3]})
    link = SumLink()
    
    # Act - Execute the operation
    result = link.call(ctx)
    
    # Assert - Verify the result
    assert result.get("result") == 6
```

## Common Test Scenarios

### Testing Context Operations
```python
# Test insert (preserves type)
def test_context_insert():
    ctx = Context[InputData]({"key": "value"})
    new_ctx = ctx.insert("extra", "data")
    assert new_ctx.get("key") == "value"
    assert new_ctx.get("extra") == "data"

# Test insert_as (changes type)
def test_context_insert_as():
    ctx = Context[InputData]({"numbers": [1, 2, 3]})
    new_ctx = ctx.insert_as("result", 6.0)
    assert new_ctx.get("result") == 6.0
    assert new_ctx.get("numbers") == [1, 2, 3]
```

### Testing Link Execution
```python
def test_link_call():
    link = MyLink()
    ctx = Context({"input": "value"})
    
    result = await link.call(ctx)
    
    assert result.get("output") == "expected"
    assert result.get("input") == "value"  # Original data preserved
```

### Testing Chain Execution
```python
def test_chain_execution():
    chain = Chain([
        Link1(),
        Link2(),
        Link3()
    ])
    ctx = Context({"initial": "data"})
    
    result = await chain.execute(ctx)
    
    assert result.get("final_output") is not None
```

### Testing Error Handling
```python
def test_link_error_handling():
    link = RiskyLink()
    ctx = Context({"invalid": "data"})
    
    with pytest.raises(ValueError):
        await link.call(ctx)
```

### Testing Type Evolution
```python
def test_type_evolution():
    # Start with InputData type
    ctx = Context[InputData]({"numbers": [1, 2, 3]})
    
    # Process with generic link
    link = SumLink()
    result = await link.call(ctx)
    
    # Verify type evolved to OutputData
    assert isinstance(result, Context)
    assert result.get("result") == 6.0
```

### Testing Middleware
```python
def test_logging_middleware():
    logs = []
    middleware = LoggingMiddleware(logger=lambda msg: logs.append(msg))
    chain = Chain([Link1()], middleware=[middleware])
    
    result = await chain.execute(Context({"data": "test"}))
    
    assert len(logs) > 0  # Verify logging occurred
```

## Mocking and Stubs

### Python (using unittest.mock)
```python
from unittest.mock import Mock, patch

def test_with_mock():
    mock_service = Mock()
    mock_service.fetch_data.return_value = {"result": "data"}
    
    link = MyLink(service=mock_service)
    result = await link.call(Context({}))
    
    mock_service.fetch_data.assert_called_once()
```

### JavaScript (using Jest)
```typescript
import { jest } from '@jest/globals';

test('with mock', async () => {
  const mockService = {
    fetchData: jest.fn().mockResolvedValue({ result: 'data' })
  };
  
  const link = new MyLink(mockService);
  const result = await link.call(new Context({}));
  
  expect(mockService.fetchData).toHaveBeenCalledTimes(1);
});
```

### Go (using interfaces)
```go
type MockService struct {
    FetchDataFunc func() (Data, error)
}

func (m *MockService) FetchData() (Data, error) {
    return m.FetchDataFunc()
}

func TestWithMock(t *testing.T) {
    mock := &MockService{
        FetchDataFunc: func() (Data, error) {
            return Data{Result: "data"}, nil
        },
    }
    
    link := &MyLink{service: mock}
    result, err := link.Call(NewContext[any](nil))
    
    // Assertions
}
```

## CI/CD Workflows

### Current Workflows

The repository currently has these CI/CD workflows:
- `conan-center-publish.yml` - C++ package publishing
- `package-cpp-release.yml` - C++ release packaging
- `publish_release_assets.yml` - Release asset publishing

### Adding New Tests to CI

When adding new tests, consider:
1. **Fast Feedback**: Run unit tests on every PR
2. **Integration Tests**: Run on merge to main
3. **Platform Coverage**: Test on Linux, macOS, Windows (if applicable)
4. **Language Versions**: Test multiple versions of language runtime

### Example CI Configuration Pattern

```yaml
name: Test Python

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: [3.8, 3.9, '3.10', '3.11']
    
    steps:
      - uses: actions/checkout@v3
      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: ${{ matrix.python-version }}
      - name: Install dependencies
        run: |
          cd packages/python
          pip install -e ".[dev]"
      - name: Run tests
        run: |
          cd packages/python
          pytest --cov --cov-report=xml
      - name: Upload coverage
        uses: codecov/codecov-action@v3
```

## Coverage Goals

### Target Coverage by Language
- **Python**: 90%+ (reference implementation)
- **Go**: 95%+ (achieved: 97.5%)
- **JavaScript/TypeScript**: 85%+
- **Rust**: 85%+
- **C#**: 80%+
- **Java**: 80%+
- **C++**: 75%+

### What to Cover
✅ All public APIs
✅ Core functionality (Context, Link, Chain)
✅ Error handling paths
✅ Type evolution (where applicable)
✅ Middleware integration

### What Not to Cover
❌ Generated code
❌ Simple getters/setters
❌ External library code
❌ Trivial constructors

## Performance Testing

### Benchmarking
```python
# Python (using pytest-benchmark)
def test_chain_performance(benchmark):
    chain = Chain([Link1(), Link2()])
    ctx = Context({"data": "test"})
    
    result = benchmark(lambda: chain.execute(ctx))
```

```go
// Go (built-in)
func BenchmarkChain(b *testing.B) {
    chain := NewChain[any, any](&Link1{}, &Link2{})
    ctx := NewContext[any](map[string]any{"data": "test"})
    
    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        _, _ = chain.Execute(ctx)
    }
}
```

### Performance Regression Tests
- Establish baseline metrics
- Run benchmarks on each PR
- Alert if performance degrades > 10%

## Debugging Failed Tests

### Common Issues

**Test Isolation**
- Ensure tests don't depend on order
- Clean up resources after tests
- Use fresh Context for each test

**Async/Await Issues**
- Ensure async functions are awaited
- Use proper async test markers
- Check event loop handling

**Type Issues**
- Verify type parameters are correct
- Check type assertions/casts
- Ensure generic constraints are satisfied

**Flaky Tests**
- Avoid time-dependent tests
- Use fixed seeds for random data
- Properly handle async operations

### Debug Commands

```bash
# Python - Run single test with output
pytest -v -s tests/test_file.py::test_name

# JavaScript - Debug mode
node --inspect-brk node_modules/.bin/jest tests/test_file.js

# Go - Verbose with specific test
go test -v -run TestName ./core

# Rust - Show stdout
cargo test test_name -- --nocapture
```

## Best Practices Summary

✅ **DO:**
- Write tests for all new features
- Follow existing test patterns
- Test both success and failure cases
- Keep tests fast and isolated
- Use meaningful test names
- Test edge cases

❌ **DON'T:**
- Skip tests because they're "hard to test"
- Write tests that depend on external services
- Ignore flaky tests
- Write overly complex test setup
- Test implementation details
- Duplicate test coverage

## Reference

- **Testing Best Practices**: https://testing.googleblog.com/
- **Language Testing Docs**:
  - Python: pytest documentation
  - JavaScript: Jest documentation
  - Go: testing package docs
  - Rust: The Book - Testing chapter
  - C#: xUnit documentation
