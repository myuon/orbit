# Compiler Pipeline

This document describes the compilation pipeline phases and their execution order in the Orbit compiler.

## Overview

The Orbit compiler follows a multi-phase compilation pipeline that transforms source code through several intermediate representations before execution:

```
Source Code → Lexing → Parsing → Type Checking → Monomorphization → Desugaring → Dead Code Elimination → Code Generation → Execution
```

## Compilation Phases

### 1. Preprocessing Phase
- **Purpose**: Load and merge standard library if enabled
- **Input**: Raw source code string
- **Output**: Combined source code with standard library
- **Details**: Automatically prepends `lib/std.ob` content if `enable_load_std` option is enabled

### 2. Lexing Phase
- **Purpose**: Convert source code into tokens
- **Module**: `lexer.rs`
- **Input**: Source code string
- **Output**: Vector of tokens (`Vec<Token>`)
- **Details**: Tokenizes keywords, identifiers, literals, operators, and punctuation

### 3. Parsing Phase
- **Purpose**: Build Abstract Syntax Tree (AST) from tokens
- **Module**: `parser.rs`
- **Input**: Vector of tokens
- **Output**: Program AST (`Program`)
- **Details**: Parses declarations (functions, structs, global variables) and statements

### 4. Type Checking Phase
- **Purpose**: Analyze types and perform type inference
- **Module**: `typecheck.rs`
- **Input**: Program AST
- **Output**: Program AST with type information
- **Details**: 
  - First registers struct types and function signatures
  - Then performs type inference to set `type_name` fields throughout the AST
  - Validates type compatibility and constraints

### 5. Monomorphization Phase
- **Purpose**: Instantiate generic types and functions with concrete types
- **Module**: `monomorphization.rs`
- **Input**: Type-checked program
- **Output**: Monomorphized program with concrete types
- **Details**:
  - Collects all generic instantiation targets
  - Generates concrete versions of generic functions and structs
  - Replaces generic calls with concrete implementations

### 6. Desugaring Phase
- **Purpose**: Transform high-level constructs into simpler forms
- **Module**: `desugar.rs`
- **Input**: Monomorphized program
- **Output**: Desugared program
- **Details**:
  - Converts method calls to function calls
  - Expands `push()` operations into explicit function calls
  - Transforms `new` expressions into function calls
  - Simplifies complex language constructs

### 7. Dead Code Elimination Phase
- **Purpose**: Remove unused functions, types, and global variables
- **Module**: `dead_code_elimination.rs`
- **Input**: Desugared program
- **Output**: Optimized program with dead code removed
- **Details**:
  - Performs reachability analysis starting from `main()` function
  - Eliminates unreachable functions, structs, and global variables
  - Can be disabled with `--no-dead-code-elimination` flag

### 8. Final Type Checking Phase
- **Purpose**: Validate the final program structure
- **Module**: `typecheck.rs`
- **Input**: Dead code eliminated program
- **Output**: Final validated program
- **Details**: Ensures all transformations maintained type safety

### 9. Code Generation Phase
- **Purpose**: Convert AST to VM bytecode instructions
- **Module**: `codegen.rs`
- **Input**: Final program AST
- **Output**: VM instructions (`Vec<Instruction>`)
- **Details**: Generates stack-based bytecode for the virtual machine

### 10. Label Resolution Phase
- **Purpose**: Resolve jump targets and function addresses
- **Module**: `label_resolution.rs`
- **Input**: VM instructions with labels
- **Output**: VM instructions with resolved addresses
- **Details**: Converts symbolic labels to actual instruction addresses

### 11. Execution Phase
- **Purpose**: Execute the compiled bytecode
- **Module**: `vm.rs`, `runtime.rs`
- **Input**: Resolved VM instructions
- **Output**: Program result (`Option<Value>`)
- **Details**: Runs the bytecode on the virtual machine with runtime support

## Debug and Dump Options

The compiler provides several debugging options to inspect intermediate representations:

- `--dump-desugared-code`: Show code after desugaring transformations
- `--dump-monomorphized-code`: Show code after generic instantiation
- `--dump-dce-code`: Show code after dead code elimination
- `--dump-ir`: Show generated VM bytecode instructions
- `--no-dead-code-elimination`: Skip dead code elimination for debugging

## Pipeline Flow

1. **Source → Tokens**: Lexical analysis breaks source into tokens
2. **Tokens → AST**: Parser builds syntax tree from tokens
3. **AST → Typed AST**: Type checker adds type information
4. **Typed AST → Monomorphized AST**: Generic instantiation with concrete types
5. **Monomorphized AST → Desugared AST**: High-level constructs simplified
6. **Desugared AST → Optimized AST**: Dead code elimination removes unused code
7. **Optimized AST → Bytecode**: Code generation produces VM instructions
8. **Bytecode → Resolved Bytecode**: Label resolution fixes addresses
9. **Resolved Bytecode → Result**: VM execution produces final result

Each phase validates its input and may report errors, stopping compilation if issues are detected.