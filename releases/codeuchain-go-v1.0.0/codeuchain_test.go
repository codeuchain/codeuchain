package codeuchain

import (
	"context"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// MockLink for testing
type MockLink struct {
	result      interface{}
	shouldError bool
}

func NewMockLink(result interface{}) *MockLink {
	return &MockLink{result: result, shouldError: false}
}

func NewMockLinkWithError() *MockLink {
	return &MockLink{shouldError: true}
}

func (ml *MockLink) Call(ctx context.Context, c *Context[any]) (*Context[any], error) {
	if ml.shouldError {
		return nil, errors.New("mock error")
	}
	return c.Insert("result", ml.result), nil
}

// MockMiddleware for testing
type MockMiddleware struct {
	beforeCalled bool
	afterCalled  bool
	errorCalled  bool
}

func NewMockMiddleware() *MockMiddleware {
	return &MockMiddleware{}
}

func (mm *MockMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	mm.beforeCalled = true
	return nil
}

func (mm *MockMiddleware) After(ctx context.Context, link Link[any, any], c *Context[any]) error {
	mm.afterCalled = true
	return nil
}

func (mm *MockMiddleware) OnError(ctx context.Context, link Link[any, any], err error, c *Context[any]) error {
	mm.errorCalled = true
	return nil
}

// SelectiveMiddleware demonstrates the ABC pattern - only implements Before
type SelectiveMiddleware struct {
	nopMiddleware // Embed for default no-op implementations
	beforeCalled  bool
}

func NewSelectiveMiddleware() *SelectiveMiddleware {
	return &SelectiveMiddleware{}
}

// Only override Before - After and OnError will use nopMiddleware's no-op implementations
func (sm *SelectiveMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	sm.beforeCalled = true
	return nil
}

// Example middleware implementations using the ABC pattern

// LoggingMiddleware only implements Before and After for logging
type LoggingMiddleware struct {
	nopMiddleware
	logs []string
}

func NewLoggingMiddleware() *LoggingMiddleware {
	return &LoggingMiddleware{logs: make([]string, 0)}
}

func (lm *LoggingMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	lm.logs = append(lm.logs, "before")
	return nil
}

func (lm *LoggingMiddleware) After(ctx context.Context, link Link[any, any], c *Context[any]) error {
	lm.logs = append(lm.logs, "after")
	return nil
}

// ErrorRecoveryMiddleware only implements OnError for error recovery
type ErrorRecoveryMiddleware struct {
	nopMiddleware
	recovered bool
}

func NewErrorRecoveryMiddleware() *ErrorRecoveryMiddleware {
	return &ErrorRecoveryMiddleware{}
}

func (erm *ErrorRecoveryMiddleware) OnError(ctx context.Context, link Link[any, any], err error, c *Context[any]) error {
	erm.recovered = true
	return nil // Recover from error - for now, just mark as recovered
}

func TestContextOperations(t *testing.T) {
	data := map[string]interface{}{
		"key": "value",
	}
	ctx := NewContext[any](data)

	// Test Get
	assert.Equal(t, "value", ctx.Get("key"))
	assert.Nil(t, ctx.Get("nonexistent"))

	// Test Insert
	newCtx := ctx.Insert("new_key", 42)
	assert.Equal(t, 42, newCtx.Get("new_key"))
	assert.Equal(t, "value", newCtx.Get("key"))

	// Test Merge
	otherData := map[string]interface{}{
		"other_key": true,
	}
	otherCtx := NewContext[any](otherData)
	merged := newCtx.Merge(otherCtx)
	assert.Equal(t, true, merged.Get("other_key"))
	assert.Equal(t, "value", merged.Get("key"))
}

func TestMutableContext(t *testing.T) {
	mc := NewMutableContext()

	// Test Set
	mc.Set("key", "value")
	assert.Equal(t, "value", mc.Get("key"))

	// Test ToImmutable
	immutable := mc.ToImmutable()
	assert.Equal(t, "value", immutable.Get("key"))
}

func TestChainExecution(t *testing.T) {
	chain := NewChain()
	mockLink := NewMockLink("test_result")
	chain.AddLink("test", mockLink)

	ctx := NewContext[any](nil)
	result, err := chain.Run(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, "test_result", result.Get("result"))
}

func TestChainWithMiddleware(t *testing.T) {
	chain := NewChain()
	mockLink := NewMockLink("test_result")
	mockMw := NewMockMiddleware()

	chain.AddLink("test", mockLink)
	chain.UseMiddleware(mockMw)

	ctx := NewContext[any](map[string]interface{}{})
	result, err := chain.Run(context.Background(), ctx)

	require.NoError(t, err)
	assert.Equal(t, "test_result", result.Get("result"))
	assert.True(t, mockMw.beforeCalled)
	assert.True(t, mockMw.afterCalled)
	assert.False(t, mockMw.errorCalled)
}

func TestChainWithError(t *testing.T) {
	chain := NewChain()
	mockLink := NewMockLinkWithError()
	mockMw := NewMockMiddleware()

	chain.AddLink("test", mockLink)
	chain.UseMiddleware(mockMw)

	ctx := NewContext[any](map[string]interface{}{})
	_, err := chain.Run(context.Background(), ctx)

	require.Error(t, err)
	assert.True(t, mockMw.beforeCalled)
	assert.False(t, mockMw.afterCalled)
	assert.True(t, mockMw.errorCalled)
}

func TestRetryLink(t *testing.T) {
	// Test successful retry
	retryLink := NewRetryLink(NewMockLink("success"), 3)

	ctx := NewContext[any](map[string]interface{}{})
	result, err := retryLink.Call(context.Background(), ctx)

	require.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))

	// Test failed retry
	failingLink := NewRetryLink(NewMockLinkWithError(), 2)
	result, err = failingLink.Call(context.Background(), ctx)

	require.Error(t, err)
	assert.Equal(t, "mock error", result.Get("error"))
}

func TestErrorHandlingMixin(t *testing.T) {
	ehm := NewErrorHandlingMixin()

	// Add error handler
	ehm.OnError("failing_link", "error_handler", func(err error) bool {
		return err.Error() == "test error"
	})

	// Create links
	links := map[string]Link[any, any]{
		"error_handler": NewMockLink("handled_error"),
	}

	// Test error handling
	ctx := NewContext[any](map[string]interface{}{})
	result, err := ehm.HandleError("failing_link", errors.New("test error"), ctx, links)

	require.NoError(t, err)
	assert.Equal(t, "handled_error", result.Get("result"))
	assert.Equal(t, "test error", result.Get("error"))
}

func TestLinkCall(t *testing.T) {
	link := NewMockLink(123)
	ctx := NewContext[any](nil)

	result, err := link.Call(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, 123, result.Get("result"))
}

// Typed features tests

func TestTypedContextOperations(t *testing.T) {
	// Test basic typed context
	data := map[string]interface{}{
		"key": "value",
	}
	ctx := NewContext[string](data)

	// Test Get
	assert.Equal(t, "value", ctx.Get("key"))
	assert.Nil(t, ctx.Get("nonexistent"))

	// Test Insert (maintains type)
	newCtx := ctx.Insert("new_key", 42)
	assert.Equal(t, 42, newCtx.Get("new_key"))
	assert.Equal(t, "value", newCtx.Get("key"))

	// Test InsertAs (type evolution)
	evolvedCtx := ctx.InsertAs("number", 42)
	assert.Equal(t, 42, evolvedCtx.Get("number"))
	assert.Equal(t, "value", evolvedCtx.Get("key"))
}

func TestTypedContextTypeEvolution(t *testing.T) {
	// Start with string context
	inputCtx := NewContext[string](map[string]interface{}{
		"input": "hello",
	})

	// Evolve to any context (type evolution)
	evolvedCtx := inputCtx.InsertAs("number", 42)
	assert.Equal(t, 42, evolvedCtx.Get("number"))
	assert.Equal(t, "hello", evolvedCtx.Get("input"))

	// Can still access as any type
	assert.Equal(t, "hello", evolvedCtx.Get("input"))
	assert.Equal(t, 42, evolvedCtx.Get("number"))
}

func TestTypedLinkExecution(t *testing.T) {
	// Create a typed link that processes string input to int output
	link := NewMockLink(42)

	// Create input context
	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	// Execute link
	resultCtx, err := link.Call(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, 42, resultCtx.Get("result"))
	assert.Equal(t, "test", resultCtx.Get("input"))
}

func TestTypedChainExecution(t *testing.T) {
	// Create typed chain
	chain := NewChain()

	// Add typed link
	link := NewMockLink(100)
	chain.AddLink("test", link)

	// Create input context
	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	// Execute chain
	resultCtx, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, 100, resultCtx.Get("result"))
	assert.Equal(t, "test", resultCtx.Get("input"))
}

func TestTypedChainWithMiddleware(t *testing.T) {
	// Create typed chain
	chain := NewChain()

	// Add typed link
	link := NewMockLink(200)
	chain.AddLink("test", link)

	// Add middleware
	mockMw := NewMockMiddleware()
	chain.UseMiddleware(mockMw)

	// Create input context
	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	// Execute chain
	resultCtx, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, 200, resultCtx.Get("result"))
	assert.True(t, mockMw.beforeCalled)
	assert.True(t, mockMw.afterCalled)
	assert.False(t, mockMw.errorCalled)
}

func TestMixedTypedAndUntypedUsage(t *testing.T) {
	// Start with untyped context
	untypedCtx := NewContext[any](map[string]interface{}{
		"input": "hello",
	})

	// Use typed operations
	evolvedCtx := untypedCtx.InsertAs("number", 42)

	assert.Equal(t, "hello", evolvedCtx.Get("input"))
	assert.Equal(t, 42, evolvedCtx.Get("number"))
}

func TestTypedErrorHandling(t *testing.T) {
	// Create typed chain
	chain := NewChain()

	// Add failing typed link
	link := NewMockLinkWithError()
	chain.AddLink("failing", link)

	// Add middleware
	mockMw := NewMockMiddleware()
	chain.UseMiddleware(mockMw)

	// Create input context
	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	// Execute chain (should fail)
	_, err := chain.Run(context.Background(), inputCtx)

	assert.Error(t, err)
	assert.True(t, mockMw.beforeCalled)
	assert.False(t, mockMw.afterCalled)
	assert.True(t, mockMw.errorCalled)
}

func TestTypedContextMerge(t *testing.T) {
	// Create two typed contexts
	ctx1 := NewContext[string](map[string]interface{}{
		"key1": "value1",
	})

	ctx2 := NewContext[string](map[string]interface{}{
		"key2": "value2",
	})

	// Merge them
	merged := ctx1.Merge(ctx2)

	assert.Equal(t, "value1", merged.Get("key1"))
	assert.Equal(t, "value2", merged.Get("key2"))
}

// Enhanced Type Tests for Better Coverage

func TestTypedContextWithCustomTypes(t *testing.T) {
	// Test with custom struct
	type User struct {
		Name  string
		Age   int
		Email string
	}

	user := User{Name: "Alice", Age: 30, Email: "alice@example.com"}
	ctx := NewContext[User](map[string]interface{}{
		"user": user,
	})

	// Test retrieval
	retrieved := ctx.Get("user")
	assert.IsType(t, User{}, retrieved)
	assert.Equal(t, "Alice", retrieved.(User).Name)
	assert.Equal(t, 30, retrieved.(User).Age)

	// Test type evolution
	evolved := ctx.InsertAs("processed", true)
	assert.Equal(t, true, evolved.Get("processed"))
	assert.Equal(t, user, evolved.Get("user"))
}

func TestTypedContextWithPrimitiveTypes(t *testing.T) {
	// Test with int type
	intCtx := NewContext[int](map[string]interface{}{
		"count": 42,
	})
	assert.Equal(t, 42, intCtx.Get("count"))

	// Test with float type
	floatCtx := NewContext[float64](map[string]interface{}{
		"price": 99.99,
	})
	assert.Equal(t, 99.99, floatCtx.Get("price"))

	// Test with bool type
	boolCtx := NewContext[bool](map[string]interface{}{
		"active": true,
	})
	assert.Equal(t, true, boolCtx.Get("active"))
}

func TestTypedContextNilHandling(t *testing.T) {
	// Test with nil data
	ctx := NewContext[string](nil)
	assert.NotNil(t, ctx)
	assert.Nil(t, ctx.Get("nonexistent"))

	// Test inserting into nil context
	newCtx := ctx.Insert("key", "value")
	assert.Equal(t, "value", newCtx.Get("key"))
}

func TestTypedContextTypeEvolutionChain(t *testing.T) {
	// Start with string context
	stringCtx := NewContext[string](map[string]interface{}{
		"input": "hello",
	})

	// Evolve to int context
	intCtx := stringCtx.InsertAs("number", 42)

	// Evolve to complex context
	complexCtx := intCtx.InsertAs("data", map[string]interface{}{
		"nested": "value",
	})

	// Verify all data is preserved
	assert.Equal(t, "hello", complexCtx.Get("input"))
	assert.Equal(t, 42, complexCtx.Get("number"))
	assert.Equal(t, "value", complexCtx.Get("data").(map[string]interface{})["nested"])
}

func TestTypedContextImmutability(t *testing.T) {
	original := NewContext[string](map[string]interface{}{
		"key": "original",
	})

	// Modify the context
	modified := original.Insert("key", "modified")

	// Original should remain unchanged
	assert.Equal(t, "original", original.Get("key"))
	assert.Equal(t, "modified", modified.Get("key"))

	// Different instances
	assert.NotEqual(t, original, modified)
}

func TestTypedContextMergeWithOverwrites(t *testing.T) {
	ctx1 := NewContext[string](map[string]interface{}{
		"key": "value1",
		"shared": "original",
	})

	ctx2 := NewContext[string](map[string]interface{}{
		"key": "value2", // This should overwrite
		"shared": "overwritten",
		"new": "added",
	})

	merged := ctx1.Merge(ctx2)

	// ctx2 values should win
	assert.Equal(t, "value2", merged.Get("key"))
	assert.Equal(t, "overwritten", merged.Get("shared"))
	assert.Equal(t, "added", merged.Get("new"))
}

func TestTypedContextToMap(t *testing.T) {
	data := map[string]interface{}{
		"string": "value",
		"number": 42,
		"bool":   true,
	}

	ctx := NewContext[string](data)
	result := ctx.ToMap()

	// Should be a copy, not the same reference
	assert.NotSame(t, data, result)
	assert.Equal(t, data, result)

	// Modifying the result shouldn't affect original
	result["new"] = "added"
	assert.Nil(t, ctx.Get("new"))
}

func TestTypedLinkWithSpecificTypes(t *testing.T) {
	// Create a link that expects string input and returns int output
	link := NewMockLink(100)

	// Test with string context
	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test string",
	})

	result, err := link.Call(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, 100, result.Get("result"))
	assert.Equal(t, "test string", result.Get("input"))
}

func TestTypedChainWithMultipleLinks(t *testing.T) {
	chain := NewChain()

	// Add multiple links
	link1 := NewMockLink("processed1")
	link2 := NewMockLink("processed2")
	link3 := NewMockLink("final")

	chain.AddLink("step1", link1)
	chain.AddLink("step2", link2)
	chain.AddLink("step3", link3)

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "start",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	// Last link's result should be returned
	assert.Equal(t, "final", result.Get("result"))
	assert.Equal(t, "start", result.Get("input"))
}

func TestTypedChainWithConditionalConnections(t *testing.T) {
	chain := NewChain()

	link1 := NewMockLink("success")
	link2 := NewMockLink("fallback")

	chain.AddLink("primary", link1)
	chain.AddLink("secondary", link2)

	// Add conditional connection (stored but not used in current implementation)
	chain.Connect("primary", "secondary", func(ctx *Context[any]) bool {
		return ctx.Get("error") != nil
	})

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	// Current implementation runs all links, so last link's result is returned
	assert.Equal(t, "fallback", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedRetryLinkWithTypeSafety(t *testing.T) {
	// Test successful retry with typed context
	retryLink := NewRetryLink(NewMockLink("success"), 3)

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	result, err := retryLink.Call(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedErrorHandlingWithContextTypes(t *testing.T) {
	ehm := NewErrorHandlingMixin()

	// Add error handler
	ehm.OnError("failing_link", "error_handler", func(err error) bool {
		return err.Error() == "typed error"
	})

	// Create typed error handler
	errorHandler := NewMockLink("error_handled")
	links := map[string]Link[any, any]{
		"error_handler": errorHandler,
	}

	// Test with typed context
	ctx := NewContext[any](map[string]interface{}{
		"input": "test",
		"type":  "string",
	})

	result, err := ehm.HandleError("failing_link", errors.New("typed error"), ctx, links)

	assert.NoError(t, err)
	assert.Equal(t, "error_handled", result.Get("result"))
	assert.Equal(t, "typed error", result.Get("error"))
	assert.Equal(t, "test", result.Get("input"))
	assert.Equal(t, "string", result.Get("type"))
}

func TestTypedMiddlewareWithContextEvolution(t *testing.T) {
	// Create chain with middleware
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	// Add middleware (simplified for testing)
	mockMw := NewMockMiddleware()
	chain.UseMiddleware(mockMw)

	inputCtx := NewContext[any](map[string]interface{}{
		"stage": "initial",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "result", result.Get("result"))
	assert.True(t, mockMw.beforeCalled)
	assert.True(t, mockMw.afterCalled)
}

func TestSelectiveMiddlewareABCPattern(t *testing.T) {
	// Test the ABC pattern - middleware that only implements Before
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	selectiveMw := NewSelectiveMiddleware()
	chain.UseMiddleware(selectiveMw)

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "result", result.Get("result"))
	// Only Before should be called, After and OnError should be no-ops
	assert.True(t, selectiveMw.beforeCalled)
}

func TestLoggingMiddlewareABCPattern(t *testing.T) {
	// Test middleware that only implements Before and After
	chain := NewChain()
	link := NewMockLink("processed")
	chain.AddLink("test", link)

	loggingMw := NewLoggingMiddleware()
	chain.UseMiddleware(loggingMw)

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "processed", result.Get("result"))
	// Should have logged both before and after
	assert.Contains(t, loggingMw.logs, "before")
	assert.Contains(t, loggingMw.logs, "after")
}

func TestErrorRecoveryMiddlewareABCPattern(t *testing.T) {
	// Test middleware that only implements OnError
	chain := NewChain()
	failingLink := NewMockLinkWithError()
	chain.AddLink("failing", failingLink)

	recoveryMw := NewErrorRecoveryMiddleware()
	chain.UseMiddleware(recoveryMw)

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	// This should still fail, but our recovery middleware should be notified
	_, err := chain.Run(context.Background(), inputCtx)

	// The error should still propagate, but middleware should be notified
	assert.Error(t, err)
	assert.True(t, recoveryMw.recovered)
}

func TestTypedContextWithSliceTypes(t *testing.T) {
	// Test with slice of strings
	strings := []string{"a", "b", "c"}
	ctx := NewContext[[]string](map[string]interface{}{
		"list": strings,
	})

	retrieved := ctx.Get("list")
	assert.IsType(t, []string{}, retrieved)
	assert.Equal(t, strings, retrieved)

	// Test type evolution with slice
	evolved := ctx.InsertAs("count", len(strings))
	assert.Equal(t, 3, evolved.Get("count"))
	assert.Equal(t, strings, evolved.Get("list"))
}

func TestTypedContextWithMapTypes(t *testing.T) {
	// Test with map type
	config := map[string]interface{}{
		"debug": true,
		"level": "info",
	}

	ctx := NewContext[map[string]interface{}](map[string]interface{}{
		"config": config,
	})

	retrieved := ctx.Get("config")
	assert.IsType(t, map[string]interface{}{}, retrieved)
	assert.Equal(t, config, retrieved)

	// Test nested access
	evolved := ctx.InsertAs("enabled", config["debug"])
	assert.Equal(t, true, evolved.Get("enabled"))
}

func TestTypedChainEmptyExecution(t *testing.T) {
	// Test chain with no links
	chain := NewChain()

	inputCtx := NewContext[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedContextConcurrentAccess(t *testing.T) {
	// Test that context operations are safe for concurrent access
	// (Note: This tests the immutability aspect)
	ctx := NewContext[string](map[string]interface{}{
		"shared": "value",
	})

	// Create multiple derived contexts
	ctx1 := ctx.Insert("key1", "value1")
	ctx2 := ctx.Insert("key2", "value2")

	// All should have access to original data
	assert.Equal(t, "value", ctx1.Get("shared"))
	assert.Equal(t, "value", ctx2.Get("shared"))
	assert.Equal(t, "value1", ctx1.Get("key1"))
	assert.Equal(t, "value2", ctx2.Get("key2"))

	// Original should be unchanged
	assert.Nil(t, ctx.Get("key1"))
	assert.Nil(t, ctx.Get("key2"))
}

// Test Middleware Interface Methods Directly
func TestMiddlewareInterfaceOnError(t *testing.T) {
	// Test that OnError method in Middleware interface gets coverage
	mockMw := NewMockMiddleware()

	// Create a failing link and context
	failingLink := NewMockLinkWithError()
	ctx := NewContext[any](map[string]interface{}{"input": "test"})
	testErr := errors.New("test error")

	// Directly call OnError method to ensure interface coverage
	err := mockMw.OnError(context.Background(), failingLink, testErr, ctx)

	// Should return nil (no-op implementation)
	assert.NoError(t, err)
	assert.True(t, mockMw.errorCalled)
}

func TestMiddlewareInterfaceBeforeAndAfter(t *testing.T) {
	// Test Before and After methods directly for completeness
	mockMw := NewMockMiddleware()
	link := NewMockLink("result")
	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Test Before
	err := mockMw.Before(context.Background(), link, ctx)
	assert.NoError(t, err)
	assert.True(t, mockMw.beforeCalled)

	// Test After
	resultCtx := ctx.Insert("result", "processed")
	err = mockMw.After(context.Background(), link, resultCtx)
	assert.NoError(t, err)
	assert.True(t, mockMw.afterCalled)
}

// Test Chain.Run Missing Code Paths

// FailingBeforeMiddleware fails on Before hook
type FailingBeforeMiddleware struct {
	nopMiddleware
}

func (fbm *FailingBeforeMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	return errors.New("before hook failed")
}

// FailingAfterMiddleware fails on After hook
type FailingAfterMiddleware struct {
	nopMiddleware
}

func (fam *FailingAfterMiddleware) After(ctx context.Context, link Link[any, any], c *Context[any]) error {
	return errors.New("after hook failed")
}

func TestChainRunInitialBeforeHookFailure(t *testing.T) {
	// Test failure in initial before hooks (before any links execute)
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	failingMw := &FailingBeforeMiddleware{}
	chain.UseMiddleware(failingMw)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Should fail at initial before hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "before hook failed", err.Error())
}

func TestChainRunFinalAfterHookFailure(t *testing.T) {
	// Test failure in final after hooks (after all links complete)
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	failingMw := &FailingAfterMiddleware{}
	chain.UseMiddleware(failingMw)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Should fail at final after hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "after hook failed", err.Error())
}

func TestChainRunWithMiddlewareOnly(t *testing.T) {
	// Test chain with middleware but no links to exercise final after hooks
	chain := NewChain()

	mockMw := NewMockMiddleware()
	chain.UseMiddleware(mockMw)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := chain.Run(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, "test", result.Get("input"))
	// Should have called before and after hooks
	assert.True(t, mockMw.beforeCalled)
	assert.True(t, mockMw.afterCalled)
	assert.False(t, mockMw.errorCalled)
}

// Test ErrorHandlingMixin.HandleError No Handler Path

func TestErrorHandlingMixinNoHandlerFound(t *testing.T) {
	// Test HandleError when no matching handler is found
	ehm := NewErrorHandlingMixin()

	// Add a handler that won't match
	ehm.OnError("different_link", "handler", func(err error) bool {
		return err.Error() == "different error"
	})

	links := map[string]Link[any, any]{
		"handler": NewMockLink("handled"),
	}

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Call with error that doesn't match any condition
	result, err := ehm.HandleError("failing_link", errors.New("unmatched error"), ctx, links)

	// Should return nil, nil when no handler found
	assert.NoError(t, err)
	assert.Nil(t, result)
}

func TestErrorHandlingMixinHandlerNotExists(t *testing.T) {
	// Test HandleError when handler exists in connections but not in links map
	ehm := NewErrorHandlingMixin()

	// Add a handler that matches but doesn't exist in links
	ehm.OnError("failing_link", "nonexistent_handler", func(err error) bool {
		return err.Error() == "test error"
	})

	links := map[string]Link[any, any]{
		"existing_handler": NewMockLink("handled"),
	}

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Call with matching error but nonexistent handler
	result, err := ehm.HandleError("failing_link", errors.New("test error"), ctx, links)

	// Should return nil, nil when handler doesn't exist
	assert.NoError(t, err)
	assert.Nil(t, result)
}

// Test RetryLink Edge Cases

// CountingMockLink tracks how many times it's called
type CountingMockLink struct {
	callCount int
	result    interface{}
	shouldError bool
	failUntilAttempt int // Fail until this attempt number (0-based)
}

func NewCountingMockLink(result interface{}, failUntilAttempt int) *CountingMockLink {
	return &CountingMockLink{
		result: result,
		shouldError: failUntilAttempt > 0,
		failUntilAttempt: failUntilAttempt,
	}
}

func (cml *CountingMockLink) Call(ctx context.Context, c *Context[any]) (*Context[any], error) {
	cml.callCount++
	if cml.shouldError && cml.callCount <= cml.failUntilAttempt {
		return nil, errors.New("simulated failure")
	}
	return c.Insert("result", cml.result), nil
}

func TestRetryLinkMaxRetriesExceeded(t *testing.T) {
	// Test when all retries are exhausted
	// Note: The implementation returns the last error, not "Max retries exceeded"
	countingLink := NewCountingMockLink("success", 10) // Always fails
	retryLink := NewRetryLink(countingLink, 2) // Only 2 retries

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := retryLink.Call(context.Background(), ctx)

	// Should have tried 3 times (initial + 2 retries)
	assert.Equal(t, 3, countingLink.callCount)
	assert.Error(t, err)
	// Implementation returns the actual last error, not a generic message
	assert.Equal(t, "simulated failure", err.Error())
	assert.Equal(t, "simulated failure", result.Get("error"))
}

func TestRetryLinkZeroRetries(t *testing.T) {
	// Test with 0 retries (should only try once)
	countingLink := NewCountingMockLink("success", 1) // Fails on first attempt
	retryLink := NewRetryLink(countingLink, 0)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := retryLink.Call(context.Background(), ctx)

	// Should have tried only once
	assert.Equal(t, 1, countingLink.callCount)
	assert.Error(t, err)
	assert.Equal(t, "simulated failure", err.Error())
	assert.Equal(t, "simulated failure", result.Get("error"))
}

func TestRetryLinkExactRetryCount(t *testing.T) {
	// Test that it retries exactly the specified number of times
	countingLink := NewCountingMockLink("success", 2) // Fails twice, succeeds on third
	retryLink := NewRetryLink(countingLink, 3)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := retryLink.Call(context.Background(), ctx)

	// Should have tried 3 times: fail, fail, success
	assert.Equal(t, 3, countingLink.callCount)
	assert.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

func TestRetryLinkSuccessOnFirstTry(t *testing.T) {
	// Test when link succeeds immediately (no retries needed)
	countingLink := NewCountingMockLink("success", 0) // Never fails
	retryLink := NewRetryLink(countingLink, 3)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := retryLink.Call(context.Background(), ctx)

	// Should have tried only once
	assert.Equal(t, 1, countingLink.callCount)
	assert.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

// Test Interface Method Coverage

func TestMiddlewareInterfaceDirectCall(t *testing.T) {
	// Test calling middleware methods through interface to ensure coverage
	var mw Middleware[any, any] = &nopMiddleware{}

	link := NewMockLink("result")
	ctx := NewContext[any](map[string]interface{}{"input": "test"})
	testErr := errors.New("test error")

	// Call methods through interface
	err := mw.Before(context.Background(), link, ctx)
	assert.NoError(t, err)

	resultCtx := ctx.Insert("result", "processed")
	err = mw.After(context.Background(), link, resultCtx)
	assert.NoError(t, err)

	err = mw.OnError(context.Background(), link, testErr, ctx)
	assert.NoError(t, err)
}

// Test Chain.Run with no middleware
func TestChainRunNoMiddleware(t *testing.T) {
	// Test chain execution with no middleware at all
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	result, err := chain.Run(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, "result", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

// Test Chain.Run Per-Link Before Hook Failure
func TestChainRunPerLinkBeforeHookFailure(t *testing.T) {
	// Test failure in per-link before hooks (different from initial before hooks)
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	// Middleware that fails only on per-link before (not initial before)
	perLinkFailingMw := &PerLinkFailingBeforeMiddleware{}
	chain.UseMiddleware(perLinkFailingMw)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Should fail at per-link before hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "per-link before failed", err.Error())
}

// PerLinkFailingBeforeMiddleware fails only on per-link before hooks
type PerLinkFailingBeforeMiddleware struct {
	nopMiddleware
	callCount int
}

func (plfbm *PerLinkFailingBeforeMiddleware) Before(ctx context.Context, link Link[any, any], c *Context[any]) error {
	plfbm.callCount++
	// Fail only on the second call (per-link before, not initial before)
	if plfbm.callCount == 2 && link != nil {
		return errors.New("per-link before failed")
	}
	return nil
}

// Test Chain.Run Per-Link After Hook Failure
func TestChainRunPerLinkAfterHookFailure(t *testing.T) {
	// Test failure in per-link after hooks
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	perLinkFailingAfterMw := &PerLinkFailingAfterMiddleware{}
	chain.UseMiddleware(perLinkFailingAfterMw)

	ctx := NewContext[any](map[string]interface{}{"input": "test"})

	// Should fail at per-link after hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "per-link after failed", err.Error())
}

// PerLinkFailingAfterMiddleware fails on per-link after hooks
type PerLinkFailingAfterMiddleware struct {
	nopMiddleware
}

func (plfam *PerLinkFailingAfterMiddleware) After(ctx context.Context, link Link[any, any], c *Context[any]) error {
	// Fail only when called with a link (per-link after, not final after)
	if link != nil {
		return errors.New("per-link after failed")
	}
	return nil
}