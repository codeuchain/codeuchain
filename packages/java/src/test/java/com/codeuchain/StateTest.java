package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class StateTest {

    @Test
    void testStateCreation() {
        Map<String, Object> data = new HashMap<>();
        data.put("key1", "value1");
        data.put("key2", 42);

        State ctx = State.create(data);

        assertEquals("value1", ctx.get("key1"));
        assertEquals(42, ctx.get("key2"));
        assertNull(ctx.get("nonexistent"));
    }

    @Test
    void testStateInsert() {
        State ctx = State.create();
        State newCtx = ctx.insert("newKey", "newValue");

        assertNull(ctx.get("newKey")); // Original state unchanged
        assertEquals("newValue", newCtx.get("newKey"));
    }

    @Test
    void testStateMerge() {
        Map<String, Object> data1 = new HashMap<>();
        data1.put("key1", "value1");

        Map<String, Object> data2 = new HashMap<>();
        data2.put("key2", "value2");

        State ctx1 = State.create(data1);
        State ctx2 = State.create(data2);

        State merged = ctx1.merge(ctx2);

        assertEquals("value1", merged.get("key1"));
        assertEquals("value2", merged.get("key2"));
    }

    @Test
    void testEmptyState() {
        State ctx = State.create();
        assertNull(ctx.get("anyKey"));
    }
}