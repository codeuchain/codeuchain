package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class IntegrationTest {

    @Test
    void testMathProcessingChain() {
        Chain chain = new Chain();

        // Link that adds two numbers
        Link addLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                Integer a = (Integer) ctx.get("a");
                Integer b = (Integer) ctx.get("b");
                if (a != null && b != null) {
                    return ctx.insert("sum", a + b);
                }
                return ctx;
            }
        };

        // Link that multiplies result by 2
        Link multiplyLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                Integer sum = (Integer) ctx.get("sum");
                if (sum != null) {
                    return ctx.insert("result", sum * 2);
                }
                return ctx;
            }
        };

        chain.addLink("add", addLink);
        chain.addLink("multiply", multiplyLink);

                // Add logging middleware
        Middleware loggingMiddleware = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) {
                // Log before execution
                String linkName = link != null ? link.getClass().getName() : "Chain";
                System.out.println("Executing: " + linkName);
                return ctx;
            }

            @Override
            public Context after(Link link, Context ctx) {
                // Log after execution
                String linkName = link != null ? link.getClass().getName() : "Chain";
                System.out.println("Completed: " + linkName);
                return ctx;
            }

            @Override
            public Context onError(Link link, Exception error, Context ctx) {
                // Log errors
                String linkName = link != null ? link.getClass().getName() : "Chain";
                System.out.println("Error in: " + linkName + " - " + error.getMessage());
                return ctx;
            }
        };

        chain.useMiddleware(loggingMiddleware);

        Map<String, Object> data = new HashMap<>();
        data.put("a", 3);
        data.put("b", 4);

        Context input = Context.create(data);
        Context result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        // Verify the chain worked: (3 + 4) * 2 = 14
        assertEquals(3, result.get("a"));
        assertEquals(4, result.get("b"));
        assertEquals(7, result.get("sum"));
        assertEquals(14, result.get("result"));
    }

    @Test
    void testChainWithErrorHandling() {
        Chain chain = new Chain();

        Link failingLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                throw new RuntimeException("Test error");
            }
        };

        chain.addLink("failing", failingLink);

        final boolean[] errorHandled = {false};

        Middleware errorHandlingMiddleware = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) { return ctx; }

            @Override
            public Context after(Link link, Context ctx) { return ctx; }

            @Override
            public Context onError(Link link, Exception error, Context ctx) {
                errorHandled[0] = true;
                assertEquals("Test error", error.getMessage());
                return ctx;
            }
        };

        chain.useMiddleware(errorHandlingMiddleware);

        Context input = Context.create();

        // The chain should throw the exception, but the middleware should still be called
        try {
            chain.run(input);
            fail("Expected RuntimeException to be thrown");
        } catch (Exception e) {
            if (e instanceof RuntimeException) {
                assertEquals("Test error", e.getMessage());
                assertTrue(errorHandled[0], "Error middleware should be called before exception is re-thrown");
            } else {
                fail("Expected RuntimeException but got: " + e.getClass().getSimpleName());
            }
        }
    }
}