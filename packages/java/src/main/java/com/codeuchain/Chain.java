package com.codeuchain;

import java.util.*;

/**
 * Chain: The Harmonious Connector
 * Orchestrates link execution with middleware support.
 */
public class Chain {
    private final Map<String, Link> links = new HashMap<>();
    private final List<Middleware> middlewares = new ArrayList<>();

    public Chain addLink(String name, Link link) {
        links.put(name, link);
        return this;
    }

    public Chain useMiddleware(Middleware middleware) {
        middlewares.add(middleware);
        return this;
    }

    public Context run(Context initialContext) throws Exception {
        Context currentContext = initialContext;

        // Execute before hooks
        for (Middleware mw : middlewares) {
            try {
                currentContext = mw.before(null, currentContext);
            } catch (Exception e) {
                // Handle middleware errors
                for (Middleware errorMw : middlewares) {
                    try {
                        currentContext = errorMw.onError(null, e, currentContext);
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
            for (Middleware mw : middlewares) {
                try {
                    currentContext = mw.before(link, currentContext);
                } catch (Exception e) {
                    // Handle middleware errors
                    for (Middleware errorMw : middlewares) {
                        try {
                            currentContext = errorMw.onError(link, e, currentContext);
                        } catch (Exception errorMwException) {
                            // Continue with other error handlers
                        }
                    }
                    throw e;
                }
            }

            // Execute link
            try {
                currentContext = link.call(currentContext);
            } catch (Exception e) {
                // Handle link errors
                for (Middleware mw : middlewares) {
                    try {
                        currentContext = mw.onError(link, e, currentContext);
                    } catch (Exception errorMwException) {
                        // Continue with other error handlers
                    }
                }
                throw e;
            }

            // After each link
            for (Middleware mw : middlewares) {
                try {
                    currentContext = mw.after(link, currentContext);
                } catch (Exception e) {
                    // Handle middleware errors
                    for (Middleware errorMw : middlewares) {
                        try {
                            currentContext = errorMw.onError(link, e, currentContext);
                        } catch (Exception errorMwException) {
                            // Continue with other error handlers
                        }
                    }
                    throw e;
                }
            }
        }

        // Final after hooks
        for (Middleware mw : middlewares) {
            try {
                currentContext = mw.after(null, currentContext);
            } catch (Exception e) {
                // Handle middleware errors
                for (Middleware errorMw : middlewares) {
                    try {
                        currentContext = errorMw.onError(null, e, currentContext);
                    } catch (Exception errorMwException) {
                        // Continue with other error handlers
                    }
                }
                throw e;
            }
        }

        return currentContext;
    }
}