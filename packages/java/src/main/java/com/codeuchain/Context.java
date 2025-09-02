package com.codeuchain;

import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Context: The Loving Vessel
 * With agape compassion, holds data tenderly, immutable by default for safety.
 */
public class Context {
    private final Map<String, Object> data;
    private static final ObjectMapper objectMapper = new ObjectMapper();

    private Context(Map<String, Object> data) {
        this.data = new HashMap<>(data);
    }

    public static Context create() {
        return new Context(new HashMap<>());
    }

    public static Context create(Map<String, Object> initialData) {
        return new Context(initialData != null ? initialData : new HashMap<>());
    }

    public Object get(String key) {
        return data.get(key);
    }

    public Context insert(String key, Object value) {
        Map<String, Object> newData = new HashMap<>(this.data);
        newData.put(key, value);
        return new Context(newData);
    }

    public Context merge(Context other) {
        Map<String, Object> newData = new HashMap<>(this.data);
        newData.putAll(other.data);
        return new Context(newData);
    }

    public Map<String, Object> toMap() {
        return new HashMap<>(data);
    }

    @Override
    public String toString() {
        try {
            return objectMapper.writeValueAsString(data);
        } catch (Exception e) {
            return "Context" + data.toString();
        }
    }
}