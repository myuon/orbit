use crate::ast::Program;
use crate::vm::{VMCompiler, VM};
use anyhow::{bail, Result};
use std::collections::HashMap;

/// Index into the heap for heap-allocated objects
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapIndex(pub usize);

/// Objects stored on the heap
#[derive(Debug, Clone, PartialEq)]
pub enum HeapObject {
    String(String),
    Vector(Vec<Value>),
    Map(HashMap<String, Value>),
    Struct(HashMap<String, Value>),
    Pointer(Vec<Value>), // Pointer is essentially an array of values
}

/// Values in the Orbit runtime system
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Address(usize),
    HeapRef(HeapIndex),
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
            Value::Address(addr) => write!(f, "@{}", addr),
            Value::HeapRef(index) => write!(f, "heap@{}", index.0),
        }
    }
}

impl std::fmt::Display for HeapObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapObject::String(s) => write!(f, "{}", s),
            HeapObject::Vector(v) => {
                write!(
                    f,
                    "[{}]",
                    v.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            HeapObject::Map(m) => {
                let entries: Vec<String> = m.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            HeapObject::Struct(fields) => {
                let entries: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect();
                write!(f, "{{{}}}", entries.join(", "))
            }
            HeapObject::Pointer(v) => {
                write!(
                    f,
                    "[*]{{{}}}",
                    v.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
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
        let string_constants = compiler.get_string_constants();

        // Execute on VM
        self.vm.reset();
        self.vm.load_program_with_constants(instructions, string_constants);

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
        let string_constants = compiler.get_string_constants();

        // Execute on VM with stack printing option
        self.vm.print_stacks = print_stacks;
        self.vm.reset();
        self.vm.load_program_with_constants(instructions, string_constants);

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
