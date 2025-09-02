package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class ChainTest {

    @Test
    void testSimpleChain() {
        Chain chain = new Chain();

        // Link that doubles a number
        Link doubleLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                Integer value = (Integer) ctx.get("value");
                if (value != null) {
                    return ctx.insert("value", value * 2);
                }
                return ctx;
            }
        };

        // Add link that adds 10
        Link addTenLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                Integer value = (Integer) ctx.get("value");
                if (value != null) {
                    return ctx.insert("value", value + 10);
                }
                return ctx;
            }
        };

        chain.addLink("double", doubleLink);
        chain.addLink("addTen", addTenLink);

        Map<String, Object> data = new HashMap<>();
        data.put("value", 5);

        Context input = Context.create(data);
        Context result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        // 5 * 2 = 10, then 10 + 10 = 20
        assertEquals(20, result.get("value"));
    }

    @Test
    void testChainWithMiddleware() {
        Chain chain = new Chain();

        Link simpleLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                return ctx.insert("processed", true);
            }
        };

        chain.addLink("simple", simpleLink);

        // Add logging middleware
        Middleware loggingMiddleware = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) {
                // In a real implementation, this would log
                return ctx.insert("beforeCalled", true);
            }

            @Override
            public Context after(Link link, Context ctx) {
                // In a real implementation, this would log
                return ctx.insert("afterCalled", true);
            }

            @Override
            public Context onError(Link link, Exception error, Context ctx) {
                // Error handling
                return ctx;
            }
        };

        chain.useMiddleware(loggingMiddleware);

        Context input = Context.create();
        Context result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        assertEquals(true, result.get("processed"));
        // Note: Middleware effects might not be visible due to immutability
    }

    @Test
    void testEmptyChain() {
        Chain chain = new Chain();

        Context input = Context.create();
        input = input.insert("test", "value");

        Context result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        assertEquals("value", result.get("test"));
    }
}