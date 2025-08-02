pub mod arm64;
pub mod ast;
pub mod ast_visitor;
pub mod codegen;
pub mod compiler;
pub mod dead_code_elimination;
pub mod desugar;
pub mod diagnostics;
pub mod formatter;
pub mod jit;
pub mod label_resolution;
pub mod lexer;
pub mod monomorphization;
pub mod parser;
pub mod profiler;
pub mod runtime;
pub mod typecheck;
pub mod utils;
pub mod value_encoding;
pub mod vm;

// Stack size constants
pub const STACK_SIZE: usize = 1024 * 1024;

// Re-export commonly used items
pub use compiler::{execute_code, execute_code_with_output, Compiler, CompilerOptions};
pub use runtime::Value;
