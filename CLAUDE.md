# CLAUDE.md

This file provides guidance to Claude Code when working with the Orbit compiler project.

## Overview

Orbit is a statically typed programming language with JIT compilation for AArch64. Currently migrating from Zig (`zig_refs/`) to Rust (`src/`).

**Language Specification**: Complete language specification is in `spec/` directory. All language features and behaviors are documented there. Implementation must follow the specification.

## Commands

- `cargo check --message-format=short` - Check compilation errors
- `cargo test --message-format=short` - Run all tests (ALWAYS run after changes)
- `cargo test test_orbit_files --message-format=short` - Run integration tests
- `cargo fmt` - Format code before commits
- `cargo fix` - Auto-fix warnings

## Architecture

### Compiler Pipeline

1. **Lexing** (`lexer.rs`)
2. **Parsing** (`parser.rs`)
3. **Type Checking** (`typecheck.rs`)
4. **Monomorphization** (`monomorphization.rs`)
5. **Desugaring** (`desugar.rs`)
6. **Code Generation** (`codegen.rs`)
7. **Execution** (`vm.rs` or `jit.rs`)

### Key Modules

- **Core**: `ast.rs`, `lexer.rs`, `parser.rs` - Language definition and parsing
- **Analysis**: `typecheck.rs`, `desugar.rs`, `monomorphization.rs` - Static analysis
- **Execution**: `codegen.rs`, `vm.rs`, `runtime.rs` - Code generation and runtime
- **Interface**: `lib.rs`, `main.rs`, `compiler.rs` - Entry points and orchestration

### Language Features

- Static typing with inference, generics, structs with methods
- Built-in data structures (vectors, maps, slices), global variables
- Control flow (if/else, while loops), JIT compilation

## Testing

- **Unit Tests**: `cargo test` - Tests embedded in each module
- **Integration Tests**: `cargo test test_orbit_files` - End-to-end tests with `.ob` files
- **Reference**: Original tests in `zig_refs/test/` with `.ob` and `.stdout` files

## Development Guidelines

- **Testing**: Always run `cargo test` after changes; use table-driven testing
- **Code Quality**: Run `cargo fmt` before commits; use `similarity-rs src/` for duplication detection
- **Migration**: Use `zig_refs/` as reference
- **Commits**: Use Conventional Commit format (e.g., `feat:`, `fix:`)
- **Debug**: Create temp files in `./tmp/`, not project root
- **Specifications**: New language features MUST be documented in `spec/` before implementation. When implementation differs from spec, fix implementation to match spec.

## Debugging

Useful debugging options for development:

- `--dump-ir` / `--dump-ir-output=<file>` - Dump VM bytecode instructions
- `--dump-desugared-code` / `--dump-desugared-code-output=<file>` - Show simplified code after desugaring
- `--dump-monomorphized-code` / `--dump-monomorphized-code-output=<file>` - Show code after generic instantiation
- `--print-stacks` - Show stack state after each VM instruction
- `--print-stacks-on-call=<function>` - Show stack traces for specific function
- `--profile` / `--profile-output=<file>` - Enable execution profiling

## Migration Phases

1. **Phase 1**: AST, lexer, parser
2. **Phase 2**: Type checker, desugaring, monomorphization
3. **Phase 3**: VM, runtime, JIT compiler
4. **Phase 4**: TUI debugger, test migration
