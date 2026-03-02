package com.codeuchain;

/**
 * Link: The Selfless Processor
 * Pure interface for state processors.
 */
@FunctionalInterface
public interface Link {
    /**
     * Process the state and return transformed state
     */
    State call(State state) throws Exception;
}