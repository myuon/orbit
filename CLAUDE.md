# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Orbit is a statically typed programming language with JIT compilation for AArch64. The compiler is built in Zig and implements a complete toolchain from lexing to native code generation.

## Build Commands

- `zig build` - Build the project (creates both library and executable)
- `zig build run` - Build and run the orbit executable
- `zig build test` - Run unit tests for both library and executable (ALWAYS run after making changes)
- `zig build run -- <args>` - Run with arguments (e.g., `zig build run -- hello_world.ob`)

## Project Architecture

### Core Compiler Pipeline

The compiler follows a traditional multi-stage pipeline:

1. **Lexing** (`lexer.zig`) - Tokenizes source code
2. **Parsing** (`parser.zig`) - Builds AST from tokens
3. **Type Checking** (`typecheck.zig`) - Validates types and semantics
4. **Desugaring** (`desugar.zig`) - Simplifies complex constructs
5. **Monomorphization** (`monomorphization.zig`) - Instantiates generic types
6. **Compilation** (`compiler.zig`) - Orchestrates the pipeline and generates IR
7. **Execution** - Either VM execution (`vm.zig`) or JIT compilation (`jit.zig`)

### Key Files

- `src/main.zig` - Entry point with TUI and debugging features
- `src/compiler.zig` - Main compiler orchestration with compilation stages
- `src/ast.zig` - AST definitions and instruction set
- `src/runtime.zig` - Runtime system for built-in functions
- `src/jit.zig` - JIT compiler for AArch64 native code generation
- `src/vm.zig` - Stack-based virtual machine for interpretation

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

Test files are in `test/` directory with `.ob` extension for Orbit source files and `.stdout` files for expected output. Heavy performance tests are in `test/heavy/`.

### VS Code Extension

The `orbit-mode/` directory contains a VS Code extension with syntax highlighting and language support for Orbit files.

## Development Notes

- The compiler can dump intermediate representations using command-line flags
- JIT compilation can be enabled/disabled via compiler flags
- The project uses an arena allocator for memory management during compilation
- Profiling support is available via the profiler.zig dependency

## Development Guidelines

- **Testing**: Always run `zig build test` after making changes to ensure all tests pass
- **Comments**: Add comments to explain "why" something is done, not "what" is being done, especially for non-standard implementations
- **Git Commits**: Use Conventional Commit format (e.g., `feat: add new feature`, `fix(parser): resolve parsing issue`)
- **Language**: All source code, comments, and commit messages should be in English