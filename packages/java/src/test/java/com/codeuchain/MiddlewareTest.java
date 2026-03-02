package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class HookTest {

    @Test
    void testHookHooks() {
        Chain chain = new Chain();

        Link testLink = new Link() {
            @Override
            public State call(State ctx) {
                return ctx.insert("linkExecuted", true);
            }
        };

        chain.addLink("test", testLink);

        // Create a test hook that tracks hook calls
        final boolean[] hooksCalled = new boolean[3]; // before, after, onError

        Hook testHook = new Hook() {
            @Override
            public State before(Link link, State ctx) {
                hooksCalled[0] = true;
                return ctx;
            }

            @Override
            public State after(Link link, State ctx) {
                hooksCalled[1] = true;
                return ctx;
            }

            @Override
            public State onError(Link link, Exception error, State ctx) {
                hooksCalled[2] = true;
                return ctx;
            }
        };

        chain.useHook(testHook);

        State input = State.create();
        State result = null;
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
    void testMultipleHook() {
        Chain chain = new Chain();

        Link testLink = new Link() {
            @Override
            public State call(State ctx) throws Exception {
                return ctx.insert("executed", true);
            }
        };

        chain.addLink("test", testLink);

        final int[] callCount = {0};

        Hook hook1 = new Hook() {
            @Override
            public State before(Link link, State ctx) { callCount[0]++; return ctx; }
            @Override
            public State after(Link link, State ctx) { callCount[0]++; return ctx; }
            @Override
            public State onError(Link link, Exception error, State ctx) { return ctx; }
        };

        Hook hook2 = new Hook() {
            @Override
            public State before(Link link, State ctx) { callCount[0]++; return ctx; }
            @Override
            public State after(Link link, State ctx) { callCount[0]++; return ctx; }
            @Override
            public State onError(Link link, Exception error, State ctx) { return ctx; }
        };

        chain.useHook(hook1);
        chain.useHook(hook2);

        State input = State.create();
        try {
            chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        // Each hook should have its before and after hooks called
        // For 2 hooks and 1 link: initial before (2), per-link before (2), per-link after (2), final after (2) = 8 total
        assertEquals(8, callCount[0]);
    }
}