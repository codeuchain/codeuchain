package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class MiddlewareTest {

    @Test
    void testMiddlewareHooks() {
        Chain chain = new Chain();

        Link testLink = new Link() {
            @Override
            public Context call(Context ctx) {
                return ctx.insert("linkExecuted", true);
            }
        };

        chain.addLink("test", testLink);

        // Create a test middleware that tracks hook calls
        final boolean[] hooksCalled = new boolean[3]; // before, after, onError

        Middleware testMiddleware = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) {
                hooksCalled[0] = true;
                return ctx;
            }

            @Override
            public Context after(Link link, Context ctx) {
                hooksCalled[1] = true;
                return ctx;
            }

            @Override
            public Context onError(Link link, Exception error, Context ctx) {
                hooksCalled[2] = true;
                return ctx;
            }
        };

        chain.useMiddleware(testMiddleware);

        Context input = Context.create();
        Context result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        assertEquals(true, result.get("linkExecuted"));
        assertTrue(hooksCalled[0], "Before hook should be called");
        assertTrue(hooksCalled[1], "After hook should be called");
        assertFalse(hooksCalled[2], "OnError hook should not be called for successful execution");
    }

    @Test
    void testMultipleMiddleware() {
        Chain chain = new Chain();

        Link testLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                return ctx.insert("executed", true);
            }
        };

        chain.addLink("test", testLink);

        final int[] callCount = {0};

        Middleware middleware1 = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) { callCount[0]++; return ctx; }
            @Override
            public Context after(Link link, Context ctx) { callCount[0]++; return ctx; }
            @Override
            public Context onError(Link link, Exception error, Context ctx) { return ctx; }
        };

        Middleware middleware2 = new Middleware() {
            @Override
            public Context before(Link link, Context ctx) { callCount[0]++; return ctx; }
            @Override
            public Context after(Link link, Context ctx) { callCount[0]++; return ctx; }
            @Override
            public Context onError(Link link, Exception error, Context ctx) { return ctx; }
        };

        chain.useMiddleware(middleware1);
        chain.useMiddleware(middleware2);

        Context input = Context.create();
        try {
            chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        // Each middleware should have its before and after hooks called
        // For 2 middlewares and 1 link: initial before (2), per-link before (2), per-link after (2), final after (2) = 8 total
        assertEquals(8, callCount[0]);
    }
}