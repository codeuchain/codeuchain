package com.codeuchain.examples;

import com.codeuchain.*;
import java.util.*;

/**
 * Math Link Implementation
 */
public class MathLink implements Link {
    private final String operation;

    public MathLink(String operation) {
        this.operation = operation;
    }

    @Override
    public Context call(Context context) throws Exception {
        @SuppressWarnings("unchecked")
        List<Double> numbers = (List<Double>) context.get("numbers");
        if (numbers == null || numbers.isEmpty()) {
            return context.insert("error", "Invalid numbers");
        }

        double result;
        switch (operation) {
            case "sum":
                result = numbers.stream().mapToDouble(Double::doubleValue).sum();
                break;
            case "mean":
                result = numbers.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
                break;
            default:
                result = 0.0;
        }

        return context.insert("result", result);
    }
}