# CodeUChain COBOL Examples

This directory contains example programs that demonstrate the CodeUChain COBOL implementation.

## Examples

### 1. `simple_chain_example.cob`
**Purpose**: Basic demonstration of chain execution and state passing
**Features**:
- Simple state initialization
- Link interface execution
- Chain orchestration
- Basic result handling

### 2. `financial_example.cob`
**Purpose**: Showcases COBOL's strength in financial calculations
**Features**:
- Compound interest calculations
- Financial data processing
- COBOL's decimal arithmetic (COMP-3 fields)
- Business-oriented calculations

### 3. `hook_example.cob`
**Purpose**: Demonstrates hook functionality and logging
**Features**:
- Hook name retrieval
- Before/After operations
- Logging hook execution
- Audit trail generation

### 4. `complete_architecture_demo.cob`
**Purpose**: Complete demonstration of all CodeUChain components
**Features**:
- Full architecture integration
- State management
- Link processing (financial + general)
- Hook operations
- Chain orchestration
- Comprehensive workflow

## Building and Running Examples

### Prerequisites
- GnuCOBOL compiler (`cobc`)
- Make utility
- Compiled CodeUChain COBOL core modules

### Build Process
```bash
# From the packages/cobol directory
make clean
make all

# Compile individual examples
cobc -x -O2 -debug -Wall -o simple_chain_example examples/simple_chain_example.cob
cobc -x -O2 -debug -Wall -o financial_example examples/financial_example.cob
cobc -x -O2 -debug -Wall -o hook_example examples/hook_example.cob
cobc -x -O2 -debug -Wall -o complete_demo examples/complete_architecture_demo.cob
```

### Running Examples
```bash
# Run individual examples
./simple_chain_example
./financial_example
./hook_example
./complete_demo

# Check log files (for hook examples)
cat codeuchain.log
```

## Architecture Demonstrated

These examples showcase CodeUChain's universal patterns implemented in COBOL:

- **State**: Data persistence and passing between components
- **Links**: Processing units with standardized interfaces
- **Hook**: Cross-cutting concerns (logging, auditing)
- **Chains**: Orchestration of processing workflows
- **Modularity**: Clean separation of concerns

## COBOL-Specific Features

- Fixed-format source code
- COMP-3 packed decimal for financial calculations
- Line sequential file I/O
- Traditional COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- CALL conventions for subprogram communication

## Business Relevance

These examples demonstrate how CodeUChain brings modern software architecture patterns to legacy COBOL systems, enabling:
- Modular business logic
- Reusable processing components
- Comprehensive audit trails
- Maintainable enterprise applications