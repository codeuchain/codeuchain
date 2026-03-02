package com.codeuchain;

import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * State: The Data Container
 * Holds data carefully, immutable by default for safety.
 */
public class State {
    private final Map<String, Object> data;
    private static final ObjectMapper objectMapper = new ObjectMapper();

    private State(Map<String, Object> data) {
        this.data = new HashMap<>(data);
    }

    public static State create() {
        return new State(new HashMap<>());
    }

    public static State create(Map<String, Object> initialData) {
        return new State(initialData != null ? initialData : new HashMap<>());
    }

    public Object get(String key) {
        return data.get(key);
    }

    public State insert(String key, Object value) {
        Map<String, Object> newData = new HashMap<>(this.data);
        newData.put(key, value);
        return new State(newData);
    }

    public State merge(State other) {
        Map<String, Object> newData = new HashMap<>(this.data);
        newData.putAll(other.data);
        return new State(newData);
    }

    public Map<String, Object> toMap() {
        return new HashMap<>(data);
    }

    @Override
    public String toString() {
        try {
            return objectMapper.writeValueAsString(data);
        } catch (Exception e) {
            return "State" + data.toString();
        }
    }
}