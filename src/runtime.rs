use crate::ast::Program;
use crate::vm::{VMCompiler, VM};
use anyhow::{bail, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Address(usize),
    VectorIndex(usize),
    MapIndex(usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Address(addr) => write!(f, "@{}", addr),
            Value::VectorIndex(index) => write!(f, "vector@{}", index),
            Value::MapIndex(index) => write!(f, "map@{}", index),
        }
    }
}

pub struct Runtime {
    vm: VM,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { vm: VM::new() }
    }

    pub fn new_with_call_tracing(print_stacks: bool, print_stacks_on_call: Option<String>) -> Self {
        Runtime {
            vm: VM::with_all_options(print_stacks, print_stacks_on_call, false),
        }
    }

    /// Execute a complete program by compiling to VM bytecode
    pub fn execute_program(&mut self, program: &Program) -> Result<Option<Value>> {
        // Compile program to VM bytecode
        let mut compiler = VMCompiler::new();
        let instructions = compiler.compile_program(program);

        // Execute on VM
        self.vm.reset();
        self.vm.load_program(instructions);

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Number(result as f64))),
            Err(err) => bail!("VM execution error: {}", err),
        }
    }

    /// Execute a complete program with options (like stack printing)
    pub fn execute_program_with_options(
        &mut self,
        program: &Program,
        print_stacks: bool,
    ) -> Result<Option<Value>> {
        // Compile program to VM bytecode
        let mut compiler = VMCompiler::new();
        let instructions = compiler.compile_program(program);

        // Execute on VM with stack printing option
        self.vm.print_stacks = print_stacks;
        self.vm.reset();
        self.vm.load_program(instructions);

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Number(result as f64))),
            Err(err) => bail!("VM execution error: {}", err),
        }
    }

    /// Enable profiling in the VM
    pub fn enable_profiling(&mut self) {
        self.vm.profiler.enable();
    }

    /// Disable profiling in the VM
    pub fn disable_profiling(&mut self) {
        self.vm.profiler.disable();
    }

    /// Get profiling results from the VM
    pub fn get_profile(&self) -> String {
        self.vm.dump_profile()
    }

    /// Dump profiling results to a file
    pub fn dump_profile_to_file(&self, filename: &str) -> Result<(), std::io::Error> {
        self.vm.dump_profile_to_file(filename)
    }
}
