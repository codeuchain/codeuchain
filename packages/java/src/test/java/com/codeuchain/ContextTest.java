package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class ContextTest {

    @Test
    void testContextCreation() {
        Map<String, Object> data = new HashMap<>();
        data.put("key1", "value1");
        data.put("key2", 42);

        Context ctx = Context.create(data);

        assertEquals("value1", ctx.get("key1"));
        assertEquals(42, ctx.get("key2"));
        assertNull(ctx.get("nonexistent"));
    }

    @Test
    void testContextInsert() {
        Context ctx = Context.create();
        Context newCtx = ctx.insert("newKey", "newValue");

        assertNull(ctx.get("newKey")); // Original context unchanged
        assertEquals("newValue", newCtx.get("newKey"));
    }

    @Test
    void testContextMerge() {
        Map<String, Object> data1 = new HashMap<>();
        data1.put("key1", "value1");

        Map<String, Object> data2 = new HashMap<>();
        data2.put("key2", "value2");

        Context ctx1 = Context.create(data1);
        Context ctx2 = Context.create(data2);

        Context merged = ctx1.merge(ctx2);

        assertEquals("value1", merged.get("key1"));
        assertEquals("value2", merged.get("key2"));
    }

    @Test
    void testEmptyContext() {
        Context ctx = Context.create();
        assertNull(ctx.get("anyKey"));
    }
}