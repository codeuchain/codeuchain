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

func (ml *MockLink) Call(ctx context.Context, c *State[any]) (*State[any], error) {
	if ml.shouldError {
		return nil, errors.New("mock error")
	}
	return c.Insert("result", ml.result), nil
}

// MockHook for testing
type MockHook struct {
	beforeCalled bool
	afterCalled  bool
	errorCalled  bool
}

func NewMockHook() *MockHook {
	return &MockHook{}
}

func (mm *MockHook) Before(ctx context.Context, link Link[any, any], c *State[any]) error {
	mm.beforeCalled = true
	return nil
}

func (mm *MockHook) After(ctx context.Context, link Link[any, any], c *State[any]) error {
	mm.afterCalled = true
	return nil
}

func (mm *MockHook) OnError(ctx context.Context, link Link[any, any], err error, c *State[any]) error {
	mm.errorCalled = true
	return nil
}

// SelectiveHook demonstrates the ABC pattern - only implements Before
type SelectiveHook struct {
	nopHook // Embed for default no-op implementations
	beforeCalled  bool
}

func NewSelectiveHook() *SelectiveHook {
	return &SelectiveHook{}
}

// Only override Before - After and OnError will use nopHook's no-op implementations
func (sm *SelectiveHook) Before(ctx context.Context, link Link[any, any], c *State[any]) error {
	sm.beforeCalled = true
	return nil
}

// Example hook implementations using the ABC pattern

// LoggingHook only implements Before and After for logging
type LoggingHook struct {
	nopHook
	logs []string
}

func NewLoggingHook() *LoggingHook {
	return &LoggingHook{logs: make([]string, 0)}
}

func (lm *LoggingHook) Before(ctx context.Context, link Link[any, any], c *State[any]) error {
	lm.logs = append(lm.logs, "before")
	return nil
}

func (lm *LoggingHook) After(ctx context.Context, link Link[any, any], c *State[any]) error {
	lm.logs = append(lm.logs, "after")
	return nil
}

// ErrorRecoveryHook only implements OnError for error recovery
type ErrorRecoveryHook struct {
	nopHook
	recovered bool
}

func NewErrorRecoveryHook() *ErrorRecoveryHook {
	return &ErrorRecoveryHook{}
}

func (erm *ErrorRecoveryHook) OnError(ctx context.Context, link Link[any, any], err error, c *State[any]) error {
	erm.recovered = true
	return nil // Recover from error - for now, just mark as recovered
}

func TestStateOperations(t *testing.T) {
	data := map[string]interface{}{
		"key": "value",
	}
	ctx := NewState[any](data)

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
	otherCtx := NewState[any](otherData)
	merged := newCtx.Merge(otherCtx)
	assert.Equal(t, true, merged.Get("other_key"))
	assert.Equal(t, "value", merged.Get("key"))
}

func TestMutableState(t *testing.T) {
	mc := NewMutableState()

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

	ctx := NewState[any](nil)
	result, err := chain.Run(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, "test_result", result.Get("result"))
}

func TestChainWithHook(t *testing.T) {
	chain := NewChain()
	mockLink := NewMockLink("test_result")
	mockMw := NewMockHook()

	chain.AddLink("test", mockLink)
	chain.UseHook(mockMw)

	ctx := NewState[any](map[string]interface{}{})
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
	mockMw := NewMockHook()

	chain.AddLink("test", mockLink)
	chain.UseHook(mockMw)

	ctx := NewState[any](map[string]interface{}{})
	_, err := chain.Run(context.Background(), ctx)

	require.Error(t, err)
	assert.True(t, mockMw.beforeCalled)
	assert.False(t, mockMw.afterCalled)
	assert.True(t, mockMw.errorCalled)
}

func TestRetryLink(t *testing.T) {
	// Test successful retry
	retryLink := NewRetryLink(NewMockLink("success"), 3)

	ctx := NewState[any](map[string]interface{}{})
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
	ctx := NewState[any](map[string]interface{}{})
	result, err := ehm.HandleError("failing_link", errors.New("test error"), ctx, links)

	require.NoError(t, err)
	assert.Equal(t, "handled_error", result.Get("result"))
	assert.Equal(t, "test error", result.Get("error"))
}

func TestLinkCall(t *testing.T) {
	link := NewMockLink(123)
	ctx := NewState[any](nil)

	result, err := link.Call(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, 123, result.Get("result"))
}

// Typed features tests

func TestTypedStateOperations(t *testing.T) {
	// Test basic typed state
	data := map[string]interface{}{
		"key": "value",
	}
	ctx := NewState[string](data)

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

func TestTypedStateTypeEvolution(t *testing.T) {
	// Start with string state
	inputCtx := NewState[string](map[string]interface{}{
		"input": "hello",
	})

	// Evolve to any state (type evolution)
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

	// Create input state
	inputCtx := NewState[any](map[string]interface{}{
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

	// Create input state
	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	// Execute chain
	resultCtx, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, 100, resultCtx.Get("result"))
	assert.Equal(t, "test", resultCtx.Get("input"))
}

func TestTypedChainWithHook(t *testing.T) {
	// Create typed chain
	chain := NewChain()

	// Add typed link
	link := NewMockLink(200)
	chain.AddLink("test", link)

	// Add hook
	mockMw := NewMockHook()
	chain.UseHook(mockMw)

	// Create input state
	inputCtx := NewState[any](map[string]interface{}{
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
	// Start with untyped state
	untypedCtx := NewState[any](map[string]interface{}{
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

	// Add hook
	mockMw := NewMockHook()
	chain.UseHook(mockMw)

	// Create input state
	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	// Execute chain (should fail)
	_, err := chain.Run(context.Background(), inputCtx)

	assert.Error(t, err)
	assert.True(t, mockMw.beforeCalled)
	assert.False(t, mockMw.afterCalled)
	assert.True(t, mockMw.errorCalled)
}

func TestTypedStateMerge(t *testing.T) {
	// Create two typed states
	ctx1 := NewState[string](map[string]interface{}{
		"key1": "value1",
	})

	ctx2 := NewState[string](map[string]interface{}{
		"key2": "value2",
	})

	// Merge them
	merged := ctx1.Merge(ctx2)

	assert.Equal(t, "value1", merged.Get("key1"))
	assert.Equal(t, "value2", merged.Get("key2"))
}

// Enhanced Type Tests for Better Coverage

func TestTypedStateWithCustomTypes(t *testing.T) {
	// Test with custom struct
	type User struct {
		Name  string
		Age   int
		Email string
	}

	user := User{Name: "Alice", Age: 30, Email: "alice@example.com"}
	ctx := NewState[User](map[string]interface{}{
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

func TestTypedStateWithPrimitiveTypes(t *testing.T) {
	// Test with int type
	intCtx := NewState[int](map[string]interface{}{
		"count": 42,
	})
	assert.Equal(t, 42, intCtx.Get("count"))

	// Test with float type
	floatCtx := NewState[float64](map[string]interface{}{
		"price": 99.99,
	})
	assert.Equal(t, 99.99, floatCtx.Get("price"))

	// Test with bool type
	boolCtx := NewState[bool](map[string]interface{}{
		"active": true,
	})
	assert.Equal(t, true, boolCtx.Get("active"))
}

func TestTypedStateNilHandling(t *testing.T) {
	// Test with nil data
	ctx := NewState[string](nil)
	assert.NotNil(t, ctx)
	assert.Nil(t, ctx.Get("nonexistent"))

	// Test inserting into nil state
	newCtx := ctx.Insert("key", "value")
	assert.Equal(t, "value", newCtx.Get("key"))
}

func TestTypedStateTypeEvolutionChain(t *testing.T) {
	// Start with string state
	stringCtx := NewState[string](map[string]interface{}{
		"input": "hello",
	})

	// Evolve to int state
	intCtx := stringCtx.InsertAs("number", 42)

	// Evolve to complex state
	complexCtx := intCtx.InsertAs("data", map[string]interface{}{
		"nested": "value",
	})

	// Verify all data is preserved
	assert.Equal(t, "hello", complexCtx.Get("input"))
	assert.Equal(t, 42, complexCtx.Get("number"))
	assert.Equal(t, "value", complexCtx.Get("data").(map[string]interface{})["nested"])
}

func TestTypedStateImmutability(t *testing.T) {
	original := NewState[string](map[string]interface{}{
		"key": "original",
	})

	// Modify the state
	modified := original.Insert("key", "modified")

	// Original should remain unchanged
	assert.Equal(t, "original", original.Get("key"))
	assert.Equal(t, "modified", modified.Get("key"))

	// Different instances
	assert.NotEqual(t, original, modified)
}

func TestTypedStateMergeWithOverwrites(t *testing.T) {
	ctx1 := NewState[string](map[string]interface{}{
		"key": "value1",
		"shared": "original",
	})

	ctx2 := NewState[string](map[string]interface{}{
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

func TestTypedStateToMap(t *testing.T) {
	data := map[string]interface{}{
		"string": "value",
		"number": 42,
		"bool":   true,
	}

	ctx := NewState[string](data)
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

	// Test with string state
	inputCtx := NewState[any](map[string]interface{}{
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

	inputCtx := NewState[any](map[string]interface{}{
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
	chain.Connect("primary", "secondary", func(ctx *State[any]) bool {
		return ctx.Get("error") != nil
	})

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	// Current implementation runs all links, so last link's result is returned
	assert.Equal(t, "fallback", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedRetryLinkWithTypeSafety(t *testing.T) {
	// Test successful retry with typed state
	retryLink := NewRetryLink(NewMockLink("success"), 3)

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	result, err := retryLink.Call(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedErrorHandlingWithStateTypes(t *testing.T) {
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

	// Test with typed state
	ctx := NewState[any](map[string]interface{}{
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

func TestTypedHookWithStateEvolution(t *testing.T) {
	// Create chain with hook
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	// Add hook (simplified for testing)
	mockMw := NewMockHook()
	chain.UseHook(mockMw)

	inputCtx := NewState[any](map[string]interface{}{
		"stage": "initial",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "result", result.Get("result"))
	assert.True(t, mockMw.beforeCalled)
	assert.True(t, mockMw.afterCalled)
}

func TestSelectiveHookABCPattern(t *testing.T) {
	// Test the ABC pattern - hook that only implements Before
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	selectiveMw := NewSelectiveHook()
	chain.UseHook(selectiveMw)

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "result", result.Get("result"))
	// Only Before should be called, After and OnError should be no-ops
	assert.True(t, selectiveMw.beforeCalled)
}

func TestLoggingHookABCPattern(t *testing.T) {
	// Test hook that only implements Before and After
	chain := NewChain()
	link := NewMockLink("processed")
	chain.AddLink("test", link)

	loggingMw := NewLoggingHook()
	chain.UseHook(loggingMw)

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "processed", result.Get("result"))
	// Should have logged both before and after
	assert.Contains(t, loggingMw.logs, "before")
	assert.Contains(t, loggingMw.logs, "after")
}

func TestErrorRecoveryHookABCPattern(t *testing.T) {
	// Test hook that only implements OnError
	chain := NewChain()
	failingLink := NewMockLinkWithError()
	chain.AddLink("failing", failingLink)

	recoveryMw := NewErrorRecoveryHook()
	chain.UseHook(recoveryMw)

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	// This should still fail, but our recovery hook should be notified
	_, err := chain.Run(context.Background(), inputCtx)

	// The error should still propagate, but hook should be notified
	assert.Error(t, err)
	assert.True(t, recoveryMw.recovered)
}

func TestTypedStateWithSliceTypes(t *testing.T) {
	// Test with slice of strings
	strings := []string{"a", "b", "c"}
	ctx := NewState[[]string](map[string]interface{}{
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

func TestTypedStateWithMapTypes(t *testing.T) {
	// Test with map type
	config := map[string]interface{}{
		"debug": true,
		"level": "info",
	}

	ctx := NewState[map[string]interface{}](map[string]interface{}{
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

	inputCtx := NewState[any](map[string]interface{}{
		"input": "test",
	})

	result, err := chain.Run(context.Background(), inputCtx)

	assert.NoError(t, err)
	assert.Equal(t, "test", result.Get("input"))
}

func TestTypedStateConcurrentAccess(t *testing.T) {
	// Test that state operations are safe for concurrent access
	// (Note: This tests the immutability aspect)
	ctx := NewState[string](map[string]interface{}{
		"shared": "value",
	})

	// Create multiple derived states
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

// Test Hook Interface Methods Directly
func TestHookInterfaceOnError(t *testing.T) {
	// Test that OnError method in Hook interface gets coverage
	mockMw := NewMockHook()

	// Create a failing link and state
	failingLink := NewMockLinkWithError()
	ctx := NewState[any](map[string]interface{}{"input": "test"})
	testErr := errors.New("test error")

	// Directly call OnError method to ensure interface coverage
	err := mockMw.OnError(context.Background(), failingLink, testErr, ctx)

	// Should return nil (no-op implementation)
	assert.NoError(t, err)
	assert.True(t, mockMw.errorCalled)
}

func TestHookInterfaceBeforeAndAfter(t *testing.T) {
	// Test Before and After methods directly for completeness
	mockMw := NewMockHook()
	link := NewMockLink("result")
	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

// FailingBeforeHook fails on Before hook
type FailingBeforeHook struct {
	nopHook
}

func (fbm *FailingBeforeHook) Before(ctx context.Context, link Link[any, any], c *State[any]) error {
	return errors.New("before hook failed")
}

// FailingAfterHook fails on After hook
type FailingAfterHook struct {
	nopHook
}

func (fam *FailingAfterHook) After(ctx context.Context, link Link[any, any], c *State[any]) error {
	return errors.New("after hook failed")
}

func TestChainRunInitialBeforeHookFailure(t *testing.T) {
	// Test failure in initial before hooks (before any links execute)
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	failingMw := &FailingBeforeHook{}
	chain.UseHook(failingMw)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	failingMw := &FailingAfterHook{}
	chain.UseHook(failingMw)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

	// Should fail at final after hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "after hook failed", err.Error())
}

func TestChainRunWithHookOnly(t *testing.T) {
	// Test chain with hook but no links to exercise final after hooks
	chain := NewChain()

	mockMw := NewMockHook()
	chain.UseHook(mockMw)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

func (cml *CountingMockLink) Call(ctx context.Context, c *State[any]) (*State[any], error) {
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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	ctx := NewState[any](map[string]interface{}{"input": "test"})

	result, err := retryLink.Call(context.Background(), ctx)

	// Should have tried only once
	assert.Equal(t, 1, countingLink.callCount)
	assert.NoError(t, err)
	assert.Equal(t, "success", result.Get("result"))
	assert.Equal(t, "test", result.Get("input"))
}

// Test Interface Method Coverage

func TestHookInterfaceDirectCall(t *testing.T) {
	// Test calling hook methods through interface to ensure coverage
	var mw Hook[any, any] = &nopHook{}

	link := NewMockLink("result")
	ctx := NewState[any](map[string]interface{}{"input": "test"})
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

// Test Chain.Run with no hook
func TestChainRunNoHook(t *testing.T) {
	// Test chain execution with no hook at all
	chain := NewChain()
	link := NewMockLink("result")
	chain.AddLink("test", link)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

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

	// Hook that fails only on per-link before (not initial before)
	perLinkFailingMw := &PerLinkFailingBeforeHook{}
	chain.UseHook(perLinkFailingMw)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

	// Should fail at per-link before hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "per-link before failed", err.Error())
}

// PerLinkFailingBeforeHook fails only on per-link before hooks
type PerLinkFailingBeforeHook struct {
	nopHook
	callCount int
}

func (plfbm *PerLinkFailingBeforeHook) Before(ctx context.Context, link Link[any, any], c *State[any]) error {
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

	perLinkFailingAfterMw := &PerLinkFailingAfterHook{}
	chain.UseHook(perLinkFailingAfterMw)

	ctx := NewState[any](map[string]interface{}{"input": "test"})

	// Should fail at per-link after hook
	_, err := chain.Run(context.Background(), ctx)
	assert.Error(t, err)
	assert.Equal(t, "per-link after failed", err.Error())
}

// PerLinkFailingAfterHook fails on per-link after hooks
type PerLinkFailingAfterHook struct {
	nopHook
}

func (plfam *PerLinkFailingAfterHook) After(ctx context.Context, link Link[any, any], c *State[any]) error {
	// Fail only when called with a link (per-link after, not final after)
	if link != nil {
		return errors.New("per-link after failed")
	}
	return nil
}