package com.codeuchain;

import java.util.*;

/**
 * Chain: The Harmonious Connector
 * Orchestrates link execution with hook support.
 */
public class Chain {
    private final Map<String, Link> links = new HashMap<>();
    private final List<Hook> hooks = new ArrayList<>();

    public Chain addLink(String name, Link link) {
        links.put(name, link);
        return this;
    }

    public Chain useHook(Hook hook) {
        hooks.add(hook);
        return this;
    }

    public State run(State initialState) throws Exception {
        State currentState = initialState;

        // Execute before hooks
        for (Hook mw : hooks) {
            try {
                currentState = mw.before(null, currentState);
            } catch (Exception e) {
                // Handle hook errors
                for (Hook errorMw : hooks) {
                    try {
                        currentState = errorMw.onError(null, e, currentState);
                    } catch (Exception errorMwException) {
                        // Continue with other error handlers
                    }
                }
                throw e;
            }
        }

        // Execute links
        for (Map.Entry<String, Link> entry : links.entrySet()) {
            Link link = entry.getValue();

            // Before each link
            for (Hook mw : hooks) {
                try {
                    currentState = mw.before(link, currentState);
                } catch (Exception e) {
                    // Handle hook errors
                    for (Hook errorMw : hooks) {
                        try {
                            currentState = errorMw.onError(link, e, currentState);
                        } catch (Exception errorMwException) {
                            // Continue with other error handlers
                        }
                    }
                    throw e;
                }
            }

            // Execute link
            try {
                currentState = link.call(currentState);
            } catch (Exception e) {
                // Handle link errors
                for (Hook mw : hooks) {
                    try {
                        currentState = mw.onError(link, e, currentState);
                    } catch (Exception errorMwException) {
                        // Continue with other error handlers
                    }
                }
                throw e;
            }

            // After each link
            for (Hook mw : hooks) {
                try {
                    currentState = mw.after(link, currentState);
                } catch (Exception e) {
                    // Handle hook errors
                    for (Hook errorMw : hooks) {
                        try {
                            currentState = errorMw.onError(link, e, currentState);
                        } catch (Exception errorMwException) {
                            // Continue with other error handlers
                        }
                    }
                    throw e;
                }
            }
        }

        // Final after hooks
        for (Hook mw : hooks) {
            try {
                currentState = mw.after(null, currentState);
            } catch (Exception e) {
                // Handle hook errors
                for (Hook errorMw : hooks) {
                    try {
                        currentState = errorMw.onError(null, e, currentState);
                    } catch (Exception errorMwException) {
                        // Continue with other error handlers
                    }
                }
                throw e;
            }
        }

        return currentState;
    }
}