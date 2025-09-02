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
	result interface{}
	shouldError bool
}

func NewMockLink(result interface{}) *MockLink {
	return &MockLink{result: result, shouldError: false}
}

func NewMockLinkWithError() *MockLink {
	return &MockLink{shouldError: true}
}

func (ml *MockLink) Call(ctx context.Context, c *Context) (*Context, error) {
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

func (mm *MockMiddleware) Before(ctx context.Context, link Link, c *Context) error {
	mm.beforeCalled = true
	return nil
}

func (mm *MockMiddleware) After(ctx context.Context, link Link, c *Context) error {
	mm.afterCalled = true
	return nil
}

func (mm *MockMiddleware) OnError(ctx context.Context, link Link, err error, c *Context) error {
	mm.errorCalled = true
	return nil
}

func TestContextOperations(t *testing.T) {
	data := map[string]interface{}{
		"key": "value",
	}
	ctx := NewContext(data)

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
	otherCtx := NewContext(otherData)
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

	ctx := NewContext(nil)
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

	ctx := NewContext(map[string]interface{}{})
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

	ctx := NewContext(map[string]interface{}{})
	_, err := chain.Run(context.Background(), ctx)

	require.Error(t, err)
	assert.True(t, mockMw.beforeCalled)
	assert.False(t, mockMw.afterCalled)
	assert.True(t, mockMw.errorCalled)
}

func TestRetryLink(t *testing.T) {
	// Test successful retry
	retryLink := NewRetryLink(NewMockLink("success"), 3)

	ctx := NewContext(map[string]interface{}{})
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
	links := map[string]Link{
		"error_handler": NewMockLink("handled_error"),
	}

	// Test error handling
	ctx := NewContext(map[string]interface{}{})
	result, err := ehm.HandleError("failing_link", errors.New("test error"), ctx, links)

	require.NoError(t, err)
	assert.Equal(t, "handled_error", result.Get("result"))
	assert.Equal(t, "test error", result.Get("error"))
}

func TestLinkCall(t *testing.T) {
	link := NewMockLink(123)
	ctx := NewContext(nil)

	result, err := link.Call(context.Background(), ctx)

	assert.NoError(t, err)
	assert.Equal(t, 123, result.Get("result"))
}