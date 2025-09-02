package com.codeuchain;

/**
 * Middleware: The Gentle Enhancer
 * Optional hooks for cross-cutting concerns.
 */
public interface Middleware {
    default Context before(Link link, Context context) throws Exception {
        return context;
    }

    default Context after(Link link, Context context) throws Exception {
        return context;
    }

    default Context onError(Link link, Exception error, Context context) throws Exception {
        return context;
    }
}