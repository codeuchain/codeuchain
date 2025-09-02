package com.codeuchain;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

class LinkTest {

    @Test
    void testMathLink() {
        // Create a simple math link that adds two numbers
        Link mathLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                Integer a = (Integer) ctx.get("a");
                Integer b = (Integer) ctx.get("b");
                if (a != null && b != null) {
                    return ctx.insert("result", a + b);
                }
                return ctx;
            }
        };

        Map<String, Object> data = new HashMap<>();
        data.put("a", 5);
        data.put("b", 3);

        Context input = Context.create(data);
        Context result = null;
        try {
            result = mathLink.call(input);
        } catch (Exception e) {
            fail("Link execution should not throw exception: " + e.getMessage());
        }

        assertEquals(5, result.get("a"));
        assertEquals(3, result.get("b"));
        assertEquals(8, result.get("result"));
    }

    @Test
    void testLinkWithNullValues() {
        Link identityLink = new Link() {
            @Override
            public Context call(Context ctx) throws Exception {
                return ctx.insert("processed", true);
            }
        };

        Context input = Context.create();
        Context result = null;
        try {
            result = identityLink.call(input);
        } catch (Exception e) {
            fail("Link execution should not throw exception: " + e.getMessage());
        }

        assertEquals(true, result.get("processed"));
    }
}