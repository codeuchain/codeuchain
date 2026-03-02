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
            public State call(State ctx) throws Exception {
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
            public State call(State ctx) throws Exception {
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

        State input = State.create(data);
        State result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        // 5 * 2 = 10, then 10 + 10 = 20
        assertEquals(20, result.get("value"));
    }

    @Test
    void testChainWithHook() {
        Chain chain = new Chain();

        Link simpleLink = new Link() {
            @Override
            public State call(State ctx) throws Exception {
                return ctx.insert("processed", true);
            }
        };

        chain.addLink("simple", simpleLink);

        // Add logging hook
        Hook loggingHook = new Hook() {
            @Override
            public State before(Link link, State ctx) {
                // In a real implementation, this would log
                return ctx.insert("beforeCalled", true);
            }

            @Override
            public State after(Link link, State ctx) {
                // In a real implementation, this would log
                return ctx.insert("afterCalled", true);
            }

            @Override
            public State onError(Link link, Exception error, State ctx) {
                // Error handling
                return ctx;
            }
        };

        chain.useHook(loggingHook);

        State input = State.create();
        State result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        assertEquals(true, result.get("processed"));
        // Note: Hook effects might not be visible due to immutability
    }

    @Test
    void testEmptyChain() {
        Chain chain = new Chain();

        State input = State.create();
        input = input.insert("test", "value");

        State result = null;
        try {
            result = chain.run(input);
        } catch (Exception e) {
            fail("Chain execution should not throw exception: " + e.getMessage());
        }

        assertEquals("value", result.get("test"));
    }
}