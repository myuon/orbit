pub mod ast;
pub mod compiler;
pub mod desugar;
pub mod lexer;
pub mod parser;
pub mod profiler;
pub mod runtime;
pub mod typecheck;
pub mod vm;

// Re-export commonly used items
pub use compiler::{
    execute_code, execute_file, execute_file_with_ir_dump, execute_file_with_ir_dump_and_options,
    execute_file_with_ir_dump_and_options_on_call, execute_file_with_options,
    execute_file_with_options_on_call, execute_file_with_profiling, Compiler, CompilerOptions,
};
pub use runtime::{Runtime, Value};
pub use vm::VMCompiler;
