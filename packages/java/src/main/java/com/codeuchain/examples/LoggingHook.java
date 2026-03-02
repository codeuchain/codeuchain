package com.codeuchain.examples;

import com.codeuchain.*;

/**
 * Logging Hook
 */
public class LoggingHook implements Hook {
    @Override
    public State before(Link link, State state) throws Exception {
        if (link == null) {
            System.out.println("Starting chain execution: " + state.toMap());
        } else {
            System.out.println("Before link execution: " + state.toMap());
        }
        return state;
    }

    @Override
    public State after(Link link, State state) throws Exception {
        if (link == null) {
            System.out.println("Chain execution completed: " + state.toMap());
        } else {
            System.out.println("After link execution: " + state.toMap());
        }
        return state;
    }

    @Override
    public State onError(Link link, Exception error, State state) throws Exception {
        System.err.println("Error in execution: " + error.getMessage());
        return state;
    }
}