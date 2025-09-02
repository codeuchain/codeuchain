package com.codeuchain.examples;

import com.codeuchain.*;
import java.util.*;

/**
 * Simple Math Example
 */
public class MathExample {
    public static void main(String[] args) {
        // Create chain
        Chain chain = new Chain()
            .addLink("sum", new MathLink("sum"))
            .addLink("mean", new MathLink("mean"))
            .useMiddleware(new LoggingMiddleware());

        // Create context
        Map<String, Object> data = new HashMap<>();
        data.put("numbers", Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0));
        Context context = Context.create(data);

        try {
            Context result = chain.run(context);
            System.out.println("Result: " + result.get("result"));
            System.out.println("Full context: " + result.toMap());
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}