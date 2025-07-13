# 1. Introduction

## Overview

Orbit is a statically typed programming language designed for high-performance computing with JIT (Just-In-Time) compilation capabilities. The language combines the safety of static typing with the performance of native code generation through its innovative compilation pipeline.

## Design Philosophy

Orbit is designed with the following principles:

- **Static Typing**: All types are known at compile time, enabling optimization and preventing runtime type errors
- **Performance**: Efficient virtual machine execution with planned JIT compilation for maximum performance
- **Safety**: Strong type system prevents common programming errors
- **Simplicity**: Clean, readable syntax that is easy to learn and use
- **Generics**: Powerful generic system for code reuse without performance overhead

## Key Features

- **Static Type System**: Comprehensive type checking at compile time
- **Virtual Machine**: Efficient stack-based execution with comprehensive debugging support
- **Generic Programming**: Generic types and functions with monomorphization
- **Structured Programming**: Support for structs with methods
- **Built-in Data Structures**: Native support for vectors, maps, and slices
- **Memory Safety**: Controlled memory management with pointers
- **Global Variables**: Support for global state management

## Compilation Pipeline

The Orbit compiler follows a multi-stage compilation pipeline:

1. **Lexical Analysis**: Source code is tokenized into a stream of tokens
2. **Parsing**: Tokens are parsed into an Abstract Syntax Tree (AST)
3. **Type Checking**: Type analysis and validation
4. **Desugaring**: Complex language constructs are simplified
5. **Monomorphization**: Generic types and functions are instantiated
6. **Dead Code Elimination**: Unused functions and types are removed based on dependency analysis
7. **Code Generation**: IR (Intermediate Representation) is generated
8. **Execution**: Virtual machine interpretation with comprehensive debugging and profiling support

## Runtime Execution

Orbit currently supports virtual machine execution:

- **Virtual Machine**: Stack-based interpreter for development and production
- **JIT Compilation**: (Planned) Direct compilation to native AArch64 machine code for maximum performance

## Language Status

Orbit is currently in active development. The core language features are implemented and stable, with ongoing work on advanced features like garbage collection, pattern matching, and module system.

### Implemented Features ✓
- Static typing with type inference
- Functions with parameters and return types
- Structs with methods
- Generic types and functions with monomorphization
- Control flow (if/else, while loops)
- Built-in data structures (vectors, maps, slices)
- Global variables
- Virtual machine execution
- Comprehensive debugging tools (IR dump, stack traces, profiling)

### Planned Features (Roadmap)
- JIT compilation for AArch64
- Garbage collection
- Variable shadowing
- Operator overloading
- Macros
- Pattern matching
- Destructuring
- Closures
- Error handling and Result type
- Module system