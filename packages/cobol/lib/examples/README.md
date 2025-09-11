# CodeUChain COBOL Library Examples

This directory contains concrete implementations of CodeUChain components that serve as examples and reference implementations.

## Files

### `financial_calculator.cob`
**Purpose**: Example implementation of a Link component for financial calculations
**Demonstrates**:
- COBOL's strength in decimal arithmetic (COMP-3 fields)
- Link interface implementation
- Financial calculation patterns
- Variable-length name handling

**Usage**: Can be used as a reference for implementing custom financial calculation links or as a working example in applications.

### `logging_middleware.cob`
**Purpose**: Example implementation of a Middleware component for logging
**Demonstrates**:
- Middleware interface implementation
- File I/O operations in COBOL
- Logging patterns and audit trails
- Before/After operation handling
- Variable-length name and operation handling

**Usage**: Can be used as a reference for implementing custom middleware or as a working logging component in applications.

## Architecture Notes

These implementations follow the CodeUChain patterns:

- **Links**: Process data and return results
- **Middleware**: Intercept and enhance processing (logging, validation, etc.)
- **Chains**: Orchestrate multiple links and middleware

## Integration

To use these implementations in your COBOL programs:

1. Include the CodeUChain library interface:
   ```cobol
   COPY "lib/include/codeuchain.cob".
   ```

2. Call the implementations:
   ```cobol
   * For financial calculations
   CALL "FINANCIAL-CALCULATOR" USING link-name, input-data, output-data, result

   * For logging middleware
   CALL "LOGGING-MIDDLEWARE" USING middleware-name, context-data, operation, result
   ```

## Building

These examples are built as part of the main library build process. See the root `Makefile` for build targets.