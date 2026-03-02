package com.codeuchain;

/**
 * Hook: The Gentle Enhancer
 * Optional hooks for cross-cutting concerns.
 */
public interface Hook {
    default State before(Link link, State state) throws Exception {
        return state;
    }

    default State after(Link link, State state) throws Exception {
        return state;
    }

    default State onError(Link link, Exception error, State state) throws Exception {
        return state;
    }
}