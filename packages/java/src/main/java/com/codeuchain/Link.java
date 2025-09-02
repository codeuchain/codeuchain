package com.codeuchain;

/**
 * Link: The Selfless Processor
 * Pure interface for context processors.
 */
@FunctionalInterface
public interface Link {
    /**
     * Process the context and return transformed context
     */
    Context call(Context context) throws Exception;
}