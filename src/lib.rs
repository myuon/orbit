pub mod ast;
pub mod codegen;
pub mod compiler;
pub mod desugar;
pub mod lexer;
pub mod monomorphization;
pub mod parser;
pub mod profiler;
pub mod runtime;
pub mod typecheck;
pub mod utils;
pub mod vm;

// Re-export commonly used items
pub use compiler::{execute_code, execute_code_with_output, Compiler, CompilerOptions};
pub use runtime::Value;
