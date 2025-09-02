package com.codeuchain.examples;

import com.codeuchain.*;

/**
 * Logging Middleware
 */
public class LoggingMiddleware implements Middleware {
    @Override
    public Context before(Link link, Context context) throws Exception {
        if (link == null) {
            System.out.println("Starting chain execution: " + context.toMap());
        } else {
            System.out.println("Before link execution: " + context.toMap());
        }
        return context;
    }

    @Override
    public Context after(Link link, Context context) throws Exception {
        if (link == null) {
            System.out.println("Chain execution completed: " + context.toMap());
        } else {
            System.out.println("After link execution: " + context.toMap());
        }
        return context;
    }

    @Override
    public Context onError(Link link, Exception error, Context context) throws Exception {
        System.err.println("Error in execution: " + error.getMessage());
        return context;
    }
}