#!/bin/bash

# CodeUChain COBOL Build Script
# This script provides common build operations for maintainers

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the right directory
check_environment() {
    if [ ! -f "Makefile" ]; then
        log_error "Makefile not found. Please run from the cobol package root."
        exit 1
    fi

    if ! command -v cobc &> /dev/null; then
        log_error "GnuCOBOL (cobc) is not installed or not in PATH."
        exit 1
    fi

    log_success "Environment check passed"
}

# Clean build artifacts
clean() {
    log_info "Cleaning build artifacts..."
    make clean
    find . -name "*.o" -delete
    find . -name "*.exe" -delete
    find . -name "codeuchain.log" -delete
    log_success "Clean completed"
}

# Build the library
build_lib() {
    log_info "Building CodeUChain COBOL library..."
    make lib
    log_success "Library build completed"
}

# Build examples
build_examples() {
    log_info "Building examples..."
    make examples
    log_success "Examples build completed"
}

# Run tests
run_tests() {
    log_info "Running tests..."
    if [ -d "tests" ] && [ "$(ls -A tests)" ]; then
        make run-tests
        log_success "Tests completed"
    else
        log_warning "No tests found in tests/ directory"
    fi
}

# Create distribution package
create_dist() {
    log_info "Creating distribution package..."
    VERSION=$(cat VERSION | grep VERSION | cut -d'=' -f2)
    DIST_DIR="dist/codeuchain-cobol-${VERSION}"

    mkdir -p "$DIST_DIR"
    cp -r lib/ "$DIST_DIR/"
    cp -r examples/ "$DIST_DIR/"
    cp -r docs/ "$DIST_DIR/"
    cp README.md Makefile package.json VERSION "$DIST_DIR/"

    cd dist
    tar -czf "codeuchain-cobol-${VERSION}.tar.gz" "codeuchain-cobol-${VERSION}"
    cd ..

    log_success "Distribution package created: dist/codeuchain-cobol-${VERSION}.tar.gz"
}

# Install for development
install_dev() {
    log_info "Installing for development..."
    # Create symbolic links or copy to development locations
    log_success "Development installation completed"
}

# Show usage
usage() {
    echo "CodeUChain COBOL Build Script"
    echo ""
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  check     - Check build environment"
    echo "  clean     - Clean build artifacts"
    echo "  build     - Build library and examples"
    echo "  lib       - Build only the library"
    echo "  examples  - Build only the examples"
    echo "  test      - Run tests"
    echo "  dist      - Create distribution package"
    echo "  install   - Install for development"
    echo "  all       - Clean, build, and test"
    echo "  help      - Show this help"
    echo ""
}

# Main command dispatcher
case "${1:-help}" in
    check)
        check_environment
        ;;
    clean)
        clean
        ;;
    lib)
        check_environment
        build_lib
        ;;
    examples)
        check_environment
        build_examples
        ;;
    build)
        check_environment
        build_lib
        build_examples
        ;;
    test)
        check_environment
        run_tests
        ;;
    dist)
        check_environment
        clean
        build_lib
        build_examples
        create_dist
        ;;
    install)
        install_dev
        ;;
    all)
        check_environment
        clean
        build_lib
        build_examples
        run_tests
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        log_error "Unknown command: $1"
        echo ""
        usage
        exit 1
        ;;
esac