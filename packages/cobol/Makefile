#================================================================
# CodeUChain COBOL Implementation - Build System
#
# Makefile for compiling the complete COBOL implementation
# of CodeUChain using GnuCOBOL compiler.
#================================================================

# Compiler settings
COBC = cobc
COBCFLAGS = -x -O2 -debug -Wall

# Source directory
LIB_SRC_DIR = lib/src
LIB_EXAMPLES_DIR = lib/examples
EXAMPLES_DIR = examples
TESTS_DIR = tests

# Source files - separate main program from subprograms
MAIN_SRC = $(LIB_SRC_DIR)/main.cob
SUBPROGRAMS = $(LIB_SRC_DIR)/context.cob \
              $(LIB_SRC_DIR)/link.cob \
              $(LIB_SRC_DIR)/chain.cob \
              $(LIB_SRC_DIR)/middleware.cob \
              $(LIB_EXAMPLES_DIR)/financial_calculator.cob \
              $(LIB_EXAMPLES_DIR)/logging_middleware.cob

# Test files
TESTS = $(TESTS_DIR)/test_link.cob \
        $(TESTS_DIR)/test_financial_calculator.cob \
        $(TESTS_DIR)/test_logging_middleware.cob \
        $(TESTS_DIR)/test_context.cob \
        $(TESTS_DIR)/test_chain.cob \
        $(TESTS_DIR)/test_middleware.cob

# Test executables
TEST_EXES = $(TESTS_DIR)/test_link \
            $(TESTS_DIR)/test_financial_calculator \
            $(TESTS_DIR)/test_logging_middleware \
            $(TESTS_DIR)/test_context \
            $(TESTS_DIR)/test_chain \
            $(TESTS_DIR)/test_middleware

# Example files
EXAMPLES = $(EXAMPLES_DIR)/simple_chain_example.cob \
           $(EXAMPLES_DIR)/financial_example.cob \
           $(EXAMPLES_DIR)/middleware_example.cob \
           $(EXAMPLES_DIR)/complete_architecture_demo.cob

# Example executables
EXAMPLE_EXES = $(EXAMPLES_DIR)/simple_chain_example \
               $(EXAMPLES_DIR)/financial_example \
               $(EXAMPLES_DIR)/middleware_example \
               $(EXAMPLES_DIR)/complete_architecture_demo

# Executable name
EXECUTABLE = codeuchain-cobol

# Default target
all: $(EXECUTABLE)

# Link all object files into executable
$(EXECUTABLE): $(OBJECTS)
	$(COBC) $(COBCFLAGS) -o $@ $^

# Compile individual COBOL files to object files
$(LIB_SRC_DIR)/%.o: $(LIB_SRC_DIR)/%.cob
	$(COBC) -c $(COBCFLAGS) -o $@ $<

$(LIB_EXAMPLES_DIR)/%.o: $(LIB_EXAMPLES_DIR)/%.cob
	$(COBC) -c $(COBCFLAGS) -o $@ $<

# Compile main program
$(LIB_SRC_DIR)/main.o: $(LIB_SRC_DIR)/main.cob
	$(COBC) -c $(COBCFLAGS) -o $@ $<

# Compile subprograms as modules
$(LIB_SRC_DIR)/context.o: $(LIB_SRC_DIR)/context.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

$(LIB_SRC_DIR)/link.o: $(LIB_SRC_DIR)/link.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

$(LIB_SRC_DIR)/chain.o: $(LIB_SRC_DIR)/chain.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

$(LIB_SRC_DIR)/middleware.o: $(LIB_SRC_DIR)/middleware.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

$(LIB_EXAMPLES_DIR)/financial_calculator.o: $(LIB_EXAMPLES_DIR)/financial_calculator.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

$(LIB_EXAMPLES_DIR)/logging_middleware.o: $(LIB_EXAMPLES_DIR)/logging_middleware.cob
	$(COBC) -c -m -O2 -debug -Wall -o $@ $<

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(EXECUTABLE) $(EXAMPLE_EXES) $(TEST_EXES) *.log *.dat *.txt

# Clean and rebuild
rebuild: clean all

# Run the executable
run: $(EXECUTABLE)
	./$(EXECUTABLE)

# Build examples
examples: $(EXAMPLE_EXES)

# Compile example programs
$(EXAMPLES_DIR)/simple_chain_example: $(EXAMPLES_DIR)/simple_chain_example.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(EXAMPLES_DIR)/financial_example: $(EXAMPLES_DIR)/financial_example.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(EXAMPLES_DIR)/middleware_example: $(EXAMPLES_DIR)/middleware_example.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(EXAMPLES_DIR)/complete_architecture_demo: $(EXAMPLES_DIR)/complete_architecture_demo.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

# Build tests
tests: $(TEST_EXES)

# Compile test programs
$(TESTS_DIR)/test_link: $(TESTS_DIR)/test_link.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(TESTS_DIR)/test_financial_calculator: $(TESTS_DIR)/test_financial_calculator.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(TESTS_DIR)/test_logging_middleware: $(TESTS_DIR)/test_logging_middleware.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(TESTS_DIR)/test_context: $(TESTS_DIR)/test_context.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(TESTS_DIR)/test_chain: $(TESTS_DIR)/test_chain.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

$(TESTS_DIR)/test_middleware: $(TESTS_DIR)/test_middleware.cob $(SUBPROGRAMS)
	$(COBC) $(COBCFLAGS) -o $@ $< $(SUBPROGRAMS)

# Run tests
run-tests: tests
	@echo "=========================================="
	@echo "Running CodeUChain COBOL Test Suite"
	@echo "=========================================="
	@echo "Running Link Tests:"
	./$(TESTS_DIR)/test_link
	@echo ""
	@echo "Running Financial Calculator Tests:"
	./$(TESTS_DIR)/test_financial_calculator
	@echo ""
	@echo "Running Logging Middleware Tests:"
	./$(TESTS_DIR)/test_logging_middleware
	@echo ""
	@echo "Running Context Tests:"
	./$(TESTS_DIR)/test_context
	@echo ""
	@echo "Running Chain Tests:"
	./$(TESTS_DIR)/test_chain
	@echo ""
	@echo "Running Middleware Tests:"
	./$(TESTS_DIR)/test_middleware
	@echo "=========================================="
	@echo "Test Suite Complete"
	@echo "=========================================="

# Show help
help:
	@echo "CodeUChain COBOL Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all          - Build the complete executable (default)"
	@echo "  examples     - Build all example programs"
	@echo "  tests        - Build all test programs"
	@echo "  clean        - Remove all build artifacts"
	@echo "  rebuild      - Clean and rebuild everything"
	@echo "  run          - Build and run the main executable"
	@echo "  run-examples - Build and run all examples"
	@echo "  run-tests    - Build and run all tests"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make examples && ./examples/simple_chain_example"
	@echo "  make run-examples  # Run all examples"
	@echo "  make tests && ./tests/test_context"
	@echo "  make run-tests     # Run all tests"
	@echo ""
	@echo "Requirements:"
	@echo "  - GnuCOBOL compiler (cobc)"
	@echo "  - Make utility"
	@echo ""
	@echo "Usage:"
	@echo "  make all        # Build everything"
	@echo "  make run        # Build and run main program"
	@echo "  make examples   # Build example programs"
	@echo "  make tests      # Build test programs"
	@echo "  make run-tests  # Build and run all tests"

.PHONY: all clean rebuild run help