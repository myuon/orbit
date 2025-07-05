# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Orbit is a statically typed programming language with JIT compilation for AArch64. The compiler is currently being migrated from Zig to Rust. The original Zig implementation is preserved in `zig_refs/` directory for reference.

## Commands for Claude Code

- `cargo check --message-format=short` - Check the project for compilation errors (preferred for development)
- `cargo test --message-format=short` - Run unit tests (ALWAYS run after making changes)
- `cargo test test_orbit_files --message-format=short` - Run integration tests with .ob files
- `cargo run <file.ob>` - Execute an Orbit source file

## Project Architecture

### Current Migration Status

**🚧 MIGRATION IN PROGRESS**: The project is being migrated from Zig to Rust

- **Reference Code**: Original Zig implementation is in `zig_refs/`
- **Target Code**: New Rust implementation is in `src/`
- **Specifications**: Complete language specification is in `spec/`
- **Migration Plan**: Detailed plan is in `TASK.md`

### Core Compiler Pipeline

The compiler follows a traditional multi-stage pipeline:

1. **Lexing** (`src/lexer.rs`) - Tokenizes source code
2. **Parsing** (`src/parser.rs`) - Builds AST from tokens
3. **Type Checking** (`src/typecheck.rs`) - Validates types and semantics
4. **Desugaring** (`src/desugar.rs`) - Simplifies complex constructs
5. **Monomorphization** (`src/monomorphization.rs`) - Instantiates generic types
6. **Compilation** (`src/compiler.rs`) - Orchestrates the pipeline and generates IR
7. **Execution** - Either VM execution (`src/vm.rs`) or JIT compilation (`src/jit.rs`)

### Key Files

**Current Rust Implementation:**

- `src/lib.rs` - Library interface with compiler entry points (`execute_code`, `execute_file`, etc.)
- `src/main.rs` - Binary entry point for file execution mode
- `src/ast.rs` - AST definitions and token types for arithmetic expressions
- `src/lexer.rs` - Tokenizer for numbers and arithmetic operators
- `src/parser.rs` - Parser that builds AST from tokens
- `src/runtime.rs` - Runtime system with Value type and expression evaluation
- `src/compiler.rs` - Main compiler orchestration (to be implemented)
- `src/jit.rs` - JIT compiler for AArch64 native code generation (to be implemented)
- `src/vm.rs` - Stack-based virtual machine for interpretation (to be implemented)

**Reference Zig Implementation:**

- `zig_refs/src/main.zig` - Entry point with TUI and debugging features
- `zig_refs/src/compiler.zig` - Main compiler orchestration with compilation stages
- `zig_refs/src/ast.zig` - AST definitions and instruction set
- `zig_refs/src/runtime.zig` - Runtime system for built-in functions
- `zig_refs/src/jit.zig` - JIT compiler for AArch64 native code generation
- `zig_refs/src/vm.zig` - Stack-based virtual machine for interpretation

### Language Features

The language supports:

- Static typing with type inference
- Generic types and functions
- Structs with methods
- Built-in data structures (vectors, maps, slices)
- Global variables
- Control flow (if/else, while loops)
- JIT compilation to native code

### Testing

**Integration Tests**: The Rust implementation includes integration tests in the `tests/testcase/` directory:

- `.ob` files contain Orbit source code to execute
- `.stdout` files contain expected output
- Run with `cargo test test_orbit_files`
- Tests call the library directly (no subprocess execution)
- Tests verify end-to-end functionality of the compiler and runtime

**Unit Tests**: Comprehensive unit tests are embedded in each module:

- Lexer tests verify token generation
- Parser tests verify AST construction
- Runtime tests verify expression evaluation and function execution
- Run with `cargo test`

**Reference Tests**: Original Zig implementation tests are in `zig_refs/test/` directory with `.ob` extension for Orbit source files and `.stdout` files for expected output. Heavy performance tests are in `zig_refs/test/heavy/`.

### VS Code Extension

The `zig_refs/orbit-mode/` directory contains a VS Code extension with syntax highlighting and language support for Orbit files.

## Development Notes

- The compiler can dump intermediate representations using command-line flags
- JIT compilation can be enabled/disabled via compiler flags
- The project uses Rust's ownership system for memory management
- Migration should maintain feature parity with the original Zig implementation

## Development Guidelines

- **Code Check**: Use `cargo check` for quick compilation checks during development
- **Testing**: Always run `cargo test` after making changes to ensure all tests pass
- **Test Style**: Use table-driven testing for unit tests whenever possible to reduce duplication and improve maintainability
- **Code Duplication**: Use `similarity-rs src/` to detect code duplication and guide refactoring efforts
- **Migration**: Refer to `TASK.md` for the structured migration plan
- **Reference**: Use the Zig code in `zig_refs/` as reference for implementation
- **Specifications**: Use the language specification in `spec/` for authoritative language definition
- **Comments**: Add comments to explain "why" something is done, not "what" is being done, especially for non-standard implementations
- **Git Commits**: Use Conventional Commit format (e.g., `feat: add new feature`, `fix(parser): resolve parsing issue`)
- **Language**: All source code, comments, and commit messages should be in English

## Migration Priority

Follow the implementation phases outlined in `TASK.md`:

1. **Phase 1**: AST definitions, lexer, basic parser
2. **Phase 2**: Type checker, desugaring, monomorphization, compiler integration
3. **Phase 3**: Virtual machine, runtime system, JIT compiler
4. **Phase 4**: TUI debugger, test suite migration, VS Code extension verification

## Testing Strategy

- Port all test cases from `zig_refs/test/` to verify functionality
- Ensure all `.ob` files produce the expected output in their corresponding `.stdout` files
- Maintain performance characteristics of the original implementation

### Writing Tests

**Table-Driven Testing**: Prefer table-driven testing to reduce code duplication and improve test coverage:

```rust
#[test]
fn test_arithmetic_operations() {
    let test_cases = vec![
        ("2 + 3", Value::Number(5.0)),
        ("4 * 5", Value::Number(20.0)),
        ("10 / 2", Value::Number(5.0)),
    ];

    let runtime = Runtime::new();
    for (input, expected) in test_cases {
        let result = execute_expression(input, &runtime).unwrap();
        assert_eq!(result, expected, "Failed for input: {}", input);
    }
}
```

**Refactoring Guide**: Use similarity-rs to identify code duplication:

```bash
# Detect code duplication in the source directory
similarity-rs

# Use the output to guide refactoring efforts:
# - 100% similarity indicates exact duplicates that should be consolidated
# - High similarity (>90%) suggests opportunities for helper functions
# - Group related test cases into table-driven tests
```
