use crate::codegen::CodeGenerator;
use crate::jit::ARM64JITCompiler;
use crate::label_resolution::LabelResolver;
use crate::profiler::InstructionTimer;
use crate::value_stack::ValueStack;
use crate::vm::Instruction;
use crate::{ast::Program, profiler::Profiler};
use anyhow::{bail, Result};
use std::collections::HashMap;

/// Index into the heap for heap-allocated objects
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapIndex(pub usize);

/// Values in the Orbit runtime system
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Boolean(bool),
    Byte(u8),
    Address(usize),
    HeapRef(HeapIndex),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Byte(b) => write!(f, "{}", b),
            Value::Address(addr) => write!(f, "@{}", addr),
            Value::HeapRef(index) => write!(f, "heap@{}", index.0),
        }
    }
}

pub enum ControlFlow {
    Exit(i64),
    Continue,
}

#[derive(Debug)]
pub struct VM {
    stack: ValueStack, // JIT-compatible encoded stack with Value API
    pc: usize,         // program counter
    bp: usize,         // base pointer for stack frame
    sp: usize,         // stack pointer
    hp: usize,         // heap pointer
    program: Vec<Instruction>,
    pub print_stacks: bool, // whether to print stack state during execution
    pub print_heaps: bool,  // whether to print heap state during execution
    print_stacks_on_call: Option<String>, // print stacks only when calling this function
    heap: Vec<Value>,       // unified heap storage
    globals: Vec<Value>,    // global variables
    // Output capture for testing
    pub captured_output: Option<String>,
    // Profiling
    pub profiler: Profiler,
    // JIT compilation
    jit_compiler: Option<ARM64JITCompiler>,
    // Function call counts for JIT compilation decision
    function_call_counts: HashMap<usize, u64>,
    // Functions marked for JIT compilation (function_name -> label_address)
    jit_compile_functions: HashMap<String, usize>,
    // Reverse mapping for JIT functions (label_address -> function_name)
    jit_function_addresses: HashMap<usize, String>,
}

impl VM {
    pub fn new() -> Self {
        Self::with_options(false, false, false)
    }

    pub fn new_with_stack_printing(print_stacks: bool) -> Self {
        Self::with_options(print_stacks, false, false)
    }

    pub fn new_with_profiling(enable_profiling: bool) -> Self {
        Self::with_options(false, false, enable_profiling)
    }

    pub fn with_options(print_stacks: bool, print_heaps: bool, enable_profiling: bool) -> Self {
        Self {
            stack: ValueStack::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            hp: 0,
            program: Vec::new(),
            print_stacks,
            print_heaps,
            print_stacks_on_call: None,
            heap: Vec::new(),
            globals: Vec::new(),
            captured_output: None,
            profiler: Profiler::new_with_enabled(enable_profiling),
            jit_compiler: ARM64JITCompiler::new().ok(),
            function_call_counts: HashMap::new(),
            jit_compile_functions: HashMap::new(),
            jit_function_addresses: HashMap::new(),
        }
    }

    pub fn with_all_options(
        print_stacks: bool,
        print_heaps: bool,
        print_stacks_on_call: Option<String>,
        enable_profiling: bool,
    ) -> Self {
        Self {
            stack: ValueStack::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            hp: 0,
            program: Vec::new(),
            print_stacks,
            print_heaps,
            print_stacks_on_call,
            heap: Vec::new(),
            globals: Vec::new(),
            captured_output: None,
            profiler: Profiler::new_with_enabled(enable_profiling),
            jit_compiler: ARM64JITCompiler::new().ok(),
            function_call_counts: HashMap::new(),
            jit_compile_functions: HashMap::new(),
            jit_function_addresses: HashMap::new(),
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.pc = 0;
    }

    /// Set JIT compile functions for forced compilation
    pub fn set_jit_compile_functions(&mut self, jit_functions: HashMap<String, usize>) {
        // Create reverse mapping for CallRel instructions
        self.jit_function_addresses.clear();
        for (func_name, addr) in &jit_functions {
            self.jit_function_addresses.insert(*addr, func_name.clone());
        }
        self.jit_compile_functions = jit_functions;
    }

    /// Get the raw encoded stack for JIT operations
    pub fn get_encoded_stack(&self) -> &Vec<u64> {
        self.stack.as_encoded_vec()
    }

    /// Get mutable access to the raw encoded stack for JIT operations
    pub fn get_encoded_stack_mut(&mut self) -> &mut Vec<u64> {
        self.stack.as_encoded_vec_mut()
    }

    /// Enable output capture for testing
    pub fn enable_output_capture(&mut self) {
        self.captured_output = Some(String::new());
    }

    /// Get captured output and clear the buffer
    pub fn take_captured_output(&mut self) -> Option<String> {
        self.captured_output.take()
    }

    pub fn step(&mut self) -> Result<ControlFlow, String> {
        let instruction = &self.program[self.pc];
        let pc_before_execution = self.pc;

        // Start timing if profiling is enabled
        let timer = InstructionTimer::start(self.profiler.enabled);

        match instruction {
            Instruction::Label(_) => {
                // Labels are just markers, no action needed
            }

            Instruction::Push(value) => {
                self.stack.push(Value::Int(*value));
            }

            Instruction::PushString(s) => {
                // Store string as consecutive bytes in heap with null termination
                let start_index = self.heap.len();
                // Store each byte as RawValue(Byte)
                for byte in s.bytes() {
                    self.heap.push(Value::Byte(byte));
                }
                // Add null terminator
                self.heap.push(Value::Byte(0));
                // Push address pointing to first byte
                self.stack.push(Value::Address(start_index));
                // Update HP to keep it in sync with heap length
                self.hp = self.heap.len();
            }

            Instruction::PushHeapRef(index) => {
                if *index >= self.heap.len() {
                    return Err(format!("Invalid heap index: {}", index));
                }
                self.stack.push(Value::HeapRef(HeapIndex(*index)));
            }

            Instruction::PushAddress(addr) => {
                self.stack.push(Value::Address(*addr));
            }

            Instruction::Pop => {
                if self.stack.is_empty() {
                    return Err("Stack underflow".to_string());
                }
                self.stack.pop();
            }

            Instruction::Add => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Add".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a + b));
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a + b as i64));
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a as i64 + b));
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a as i64 + b as i64));
                    }
                    _ => return Err("Add operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Sub => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Sub".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a - b));
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a - b as i64));
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a as i64 - b));
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a as i64 - b as i64));
                    }
                    _ => return Err("Subtract operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Mul => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Mul".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a * b));
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a * b as i64));
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a as i64 * b));
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        self.stack.push(Value::Int(a as i64 * b as i64));
                    }
                    _ => return Err("Multiply operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Div => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Div".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Int(a / b));
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Int(a / b as i64));
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Int(a as i64 / b));
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.stack.push(Value::Int(a as i64 / b as i64));
                    }
                    _ => return Err("Divide operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Mod => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Mod".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Modulo by zero".to_string());
                        }
                        self.stack.push(Value::Int(a % b));
                    }
                    _ => return Err("Modulo operation requires numbers".to_string()),
                }
            }

            Instruction::AddressAdd => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for AddressAdd".to_string());
                }
                let b = self.stack.pop().unwrap(); // second operand (index)
                let a = self.stack.pop().unwrap(); // first operand (container)
                match (&a, &b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        self.stack.push(Value::Address(addr + *offset as usize));
                    }
                    (Value::HeapRef(heap_ref), Value::Int(offset)) => {
                        // HeapRef + offset = new HeapRef with adjusted index
                        self.stack
                            .push(Value::HeapRef(HeapIndex(heap_ref.0 + *offset as usize)));
                    }
                    (Value::Int(offset), Value::HeapRef(heap_ref)) => {
                        // Handle reversed order: Int + HeapRef -> HeapRef
                        self.stack
                            .push(Value::HeapRef(HeapIndex(heap_ref.0 + *offset as usize)));
                    }
                    _ => {
                        return Err(format!(
                            "AddressAdd requires Address/HeapRef + Number, got {:?} + {:?}",
                            a, b
                        ))
                    }
                }
            }

            Instruction::AddressSub => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for AddressSub".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (&a, &b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        self.stack.push(Value::Address(addr - *offset as usize));
                    }
                    (Value::HeapRef(heap_ref), Value::Int(offset)) => {
                        // HeapRef - offset = new HeapRef with adjusted index
                        self.stack
                            .push(Value::HeapRef(HeapIndex(heap_ref.0 - *offset as usize)));
                    }
                    _ => {
                        return Err(format!(
                            "AddressSub requires Address/HeapRef - Number, got {:?} + {:?}",
                            a, b
                        ))
                    }
                }
            }

            Instruction::Eq => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Eq".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(Value::Boolean(a == b));
            }

            Instruction::Lt => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Lt".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean(a < b));
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        self.stack.push(Value::Boolean(a < b as i64));
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean((a as i64) < b));
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        self.stack.push(Value::Boolean(a < b));
                    }
                    _ => return Err("Less than operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Lte => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Lte".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean(a <= b));
                    }
                    _ => return Err("Less than or equal operation requires numbers".to_string()),
                }
            }

            Instruction::Gt => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Gt".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean(a > b));
                    }
                    _ => return Err("Greater than operation requires numbers".to_string()),
                }
            }

            Instruction::Gte => {
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Gte".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean(a >= b));
                    }
                    _ => return Err("Greater than or equal operation requires numbers".to_string()),
                }
            }

            Instruction::Not => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for Not".to_string());
                }
                let value = self.stack.pop().unwrap();
                match value {
                    Value::Boolean(b) => {
                        self.stack.push(Value::Boolean(!b));
                    }
                    Value::Int(n) => {
                        self.stack.push(Value::Boolean(n == 0));
                    }
                    Value::Byte(b) => {
                        self.stack.push(Value::Boolean(b == 0));
                    }
                    _ => return Err("Not operation requires boolean, number, or byte".to_string()),
                }
            }

            Instruction::Jump(addr) => {
                self.pc = *addr;

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::JumpIfZero(addr) => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for JumpIfZero".to_string());
                }
                let value = self.stack.pop().unwrap();
                let should_jump = match value {
                    Value::Int(n) => n == 0,
                    Value::Boolean(b) => !b,
                    Value::Byte(b) => b == 0,
                    _ => false,
                };
                if should_jump {
                    self.pc = *addr;
                } else {
                    self.pc += 1;
                }

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::JumpRel(offset) => {
                let new_pc = (self.pc as i32 + offset) as usize;
                self.pc = new_pc;

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::JumpIfZeroRel(offset) => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for JumpIfZeroRel".to_string());
                }
                let value = self.stack.pop().unwrap();
                let should_jump = match value {
                    Value::Int(n) => n == 0,
                    Value::Boolean(b) => !b,
                    Value::Byte(b) => b == 0,
                    _ => false,
                };
                if should_jump {
                    let new_pc = (self.pc as i32 + offset) as usize;
                    self.pc = new_pc;
                } else {
                    self.pc += 1;
                }

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::GetLocal(offset) => {
                let index = if *offset < 0 {
                    // Negative offset: access parameters (before BP)
                    let abs_offset = (-offset) as usize;
                    if self.bp < abs_offset {
                        return Err(format!(
                            "Parameter access out of bounds: BP={}, offset={}",
                            self.bp, offset
                        ));
                    }
                    self.bp - abs_offset
                } else {
                    // Positive offset: access local variables (after BP)
                    self.bp + (*offset as usize)
                };

                if index >= self.stack.len() {
                    return Err(format!(
                        "Local variable access out of bounds: index={}, stack_len={}",
                        index,
                        self.stack.len()
                    ));
                }
                let value = self
                    .stack
                    .get(index)
                    .ok_or_else(|| format!("Invalid stack index: {}", index))?;
                self.stack.push(value);
            }

            Instruction::SetLocal(offset) => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetLocal".to_string());
                }
                let value = self.stack.pop().unwrap();

                let index = if *offset < 0 {
                    // Negative offset: access parameters (before BP)
                    let abs_offset = (-offset) as usize;
                    if self.bp < abs_offset {
                        return Err(format!(
                            "Parameter access out of bounds: BP={}, offset={}",
                            self.bp, offset
                        ));
                    }
                    self.bp - abs_offset
                } else {
                    // Positive offset: access local variables (after BP)
                    self.bp + (*offset as usize)
                };

                // Extend stack if needed for positive offsets
                while self.stack.len() <= index {
                    self.stack.push(Value::Int(0));
                }

                self.stack.set(index, value).map_err(|e| e)?;
            }

            Instruction::GetGlobal(index) => {
                if *index >= self.globals.len() {
                    return Err(format!("Global variable index out of bounds: {}", index));
                }
                self.stack.push(self.globals[*index].clone());
            }

            Instruction::SetGlobal(index) => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetGlobal".to_string());
                }
                let value = self.stack.pop().unwrap();
                // Extend globals vector if needed
                while self.globals.len() <= *index {
                    self.globals.push(Value::Int(0)); // Default value
                }
                self.globals[*index] = value;
            }

            Instruction::Call(func_name) => {
                // Record function call for profiling
                self.profiler.record_function_call(func_name.clone());

                // Check if this is the function we want to trace and print stack state
                if let Some(ref target_func) = self.print_stacks_on_call {
                    if func_name == target_func {
                        println!(
                            "{:04} {:20} [{}]",
                            self.pc,
                            format!("{}", instruction),
                            self.stack
                                .iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                    }
                }

                // Find the target address
                let mut target_addr = None;
                for (i, inst) in self.program.iter().enumerate() {
                    if let Instruction::Label(label_name) = inst {
                        if label_name == func_name {
                            target_addr = Some(i + 1); // Jump to instruction after label
                            break;
                        }
                    }
                }

                if let Some(addr) = target_addr {
                    // Handle JIT compilation - first increment call count and check if we should compile
                    let should_jit_compile = if self.jit_compiler.is_some() {
                        let count = self.function_call_counts.entry(addr).or_insert(0);
                        *count += 1;

                        // Check if we should JIT compile:
                        // 1. If function is marked for forced JIT compilation, or
                        // 2. If it meets the normal threshold (10+ calls)
                        if self.jit_compile_functions.contains_key(func_name) {
                            // For forced JIT compilation, always compile
                            true
                        } else {
                            // For threshold-based compilation
                            self.jit_compiler
                                .as_ref()
                                .unwrap()
                                .should_jit_compile(addr, *count)
                        }
                    } else {
                        false
                    };

                    // Track if JIT compilation was successful for immediate execution
                    let mut jit_compilation_successful = false;

                    // If we should compile, do it now
                    if should_jit_compile {
                        let function_instructions = self.extract_function_instructions(addr);
                        let count = *self.function_call_counts.get(&addr).unwrap(); // We know this exists

                        if let Some(ref mut jit_compiler) = self.jit_compiler {
                            match jit_compiler.compile_function(addr, &function_instructions) {
                                Ok(()) => {
                                    jit_compilation_successful = true;
                                    if self.jit_compile_functions.contains_key(func_name) {
                                        eprintln!("JIT: Successfully compiled function '{}' at address {} (forced compilation)", 
                                                 func_name, addr);
                                    } else {
                                        eprintln!("JIT: Successfully compiled function '{}' at address {} (after {} calls)", 
                                                 func_name, addr, count);
                                    }
                                }
                                Err(e) => {
                                    eprintln!(
                                        "JIT: Failed to compile function '{}' at address {}: {}",
                                        func_name, addr, e
                                    );
                                }
                            }
                        }
                    }

                    // Check if we have a JIT compiled version to execute
                    // Either just compiled successfully, or already exists
                    let should_execute_jit = jit_compilation_successful
                        || self
                            .jit_compiler
                            .as_ref()
                            .and_then(|jit| jit.get_compiled_function(addr))
                            .is_some();

                    if should_execute_jit {
                        // Execute JIT compiled function
                        if let Some(ref jit_compiler) = self.jit_compiler {
                            if let Some(jit_function) = jit_compiler.get_compiled_function(addr) {
                                eprintln!(
                                    "JIT: Executing compiled function '{}' at address {}",
                                    func_name, addr
                                );

                                // Execute the JIT compiled function with error handling
                                // Get mutable pointers to VM state
                                let stack_ptr = self.stack.as_encoded_vec_mut().as_mut_ptr();
                                let pc_ptr = &mut self.pc as *mut usize;
                                let bp_ptr = &mut self.bp as *mut usize;
                                let sp_ptr = &mut self.sp as *mut usize;
                                let hp_ptr = &mut self.hp as *mut usize;
                                let heap_ptr = &mut self.heap as *mut Vec<Value>;
                                let globals_ptr = &mut self.globals as *mut Vec<Value>;
                                let func_ptr = jit_function.function_ptr;

                                let jit_result =
                                    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                        func_ptr(
                                            stack_ptr,
                                            pc_ptr,
                                            bp_ptr,
                                            sp_ptr,
                                            hp_ptr,
                                            heap_ptr,
                                            globals_ptr,
                                        );
                                    }));

                                match jit_result {
                                    Ok(()) => {
                                        eprintln!(
                                            "JIT: Successfully executed compiled function '{}'",
                                            func_name
                                        );
                                    }
                                    Err(_) => {
                                        eprintln!("JIT: Execution failed for function '{}', falling back to interpreter", func_name);
                                        // Fall through to interpreter execution
                                        self.pc = addr;
                                        self.print_debug_visualization(
                                            pc_before_execution,
                                            instruction,
                                        );
                                        return Ok(ControlFlow::Continue);
                                    }
                                }

                                // Print debug visualization (heap and/or stack) if enabled
                                self.print_debug_visualization(pc_before_execution, instruction);

                                // JIT execution completed, return immediately
                                return Ok(ControlFlow::Continue);
                            }
                        }
                        eprintln!("JIT: Failed to get compiled function for '{}' at address {} (fallback to interpreter)", func_name, addr);
                    }

                    // Fallback to interpreter execution
                    self.pc = addr;

                    // Print debug visualization (heap and/or stack) if enabled
                    self.print_debug_visualization(pc_before_execution, instruction);

                    return Ok(ControlFlow::Continue);
                } else {
                    return Err(format!("Function not found: {}", func_name));
                }
            }

            Instruction::CallRel(offset) => {
                let new_pc = (self.pc as i32 + offset) as usize;

                // Record function call for profiling using the target address
                let call_name = format!("func_addr_{}", new_pc);
                self.profiler.record_function_call(call_name);

                // Handle JIT compilation - first increment call count and check if we should compile
                let should_jit_compile = if self.jit_compiler.is_some() {
                    let count = self.function_call_counts.entry(new_pc).or_insert(0);
                    *count += 1;

                    // Check if we should JIT compile:
                    // 1. If function is marked for forced JIT compilation (check label address), or
                    // 2. If it meets the normal threshold (10+ calls)
                    if self.jit_function_addresses.contains_key(&new_pc) {
                        // new_pc is the label address for forced JIT functions
                        true
                    } else {
                        self.jit_compiler
                            .as_ref()
                            .unwrap()
                            .should_jit_compile(new_pc, *count)
                    }
                } else {
                    false
                };

                // Track if JIT compilation was successful for immediate execution
                let mut jit_compilation_successful = false;

                // If we should compile, do it now
                if should_jit_compile {
                    let function_instructions = self.extract_function_instructions(new_pc);
                    let count = *self.function_call_counts.get(&new_pc).unwrap(); // We know this exists

                    if let Some(ref mut jit_compiler) = self.jit_compiler {
                        match jit_compiler.compile_function(new_pc, &function_instructions) {
                            Ok(()) => {
                                jit_compilation_successful = true;
                                // Try to get function name for better logging
                                let func_name = self
                                    .jit_function_addresses
                                    .get(&new_pc)
                                    .map(|s| s.as_str())
                                    .unwrap_or("unknown");

                                if self.jit_function_addresses.contains_key(&new_pc) {
                                    eprintln!("JIT: Successfully compiled function '{}' at address {} (forced compilation)", 
                                             func_name, new_pc);
                                } else {
                                    eprintln!("JIT: Successfully compiled function at address {} (after {} calls)", 
                                             new_pc, count);
                                }
                            }
                            Err(e) => {
                                let func_name = self
                                    .jit_function_addresses
                                    .get(&new_pc)
                                    .map(|s| s.as_str())
                                    .unwrap_or("unknown");
                                eprintln!(
                                    "JIT: Failed to compile function '{}' at address {}: {}",
                                    func_name, new_pc, e
                                );
                            }
                        }
                    }
                }

                // Check if we have a JIT compiled version to execute
                // Either just compiled successfully, or already exists
                let should_execute_jit = jit_compilation_successful
                    || self
                        .jit_compiler
                        .as_ref()
                        .and_then(|jit| jit.get_compiled_function(new_pc))
                        .is_some();

                if should_execute_jit {
                    // Execute JIT compiled function
                    if let Some(ref jit_compiler) = self.jit_compiler {
                        if let Some(jit_function) = jit_compiler.get_compiled_function(new_pc) {
                            let func_name = self
                                .jit_function_addresses
                                .get(&new_pc)
                                .map(|s| s.as_str())
                                .unwrap_or("unknown");
                            eprintln!(
                                "JIT: Executing compiled function '{}' at address {}",
                                func_name, new_pc
                            );

                            // Execute the JIT compiled function with error handling
                            // Get mutable pointers to VM state
                            let stack_ptr = self.stack.as_encoded_vec_mut().as_mut_ptr();
                            let pc_ptr = &mut self.pc as *mut usize;
                            let bp_ptr = &mut self.bp as *mut usize;
                            let sp_ptr = &mut self.sp as *mut usize;
                            let hp_ptr = &mut self.hp as *mut usize;
                            let heap_ptr = &mut self.heap as *mut Vec<Value>;
                            let globals_ptr = &mut self.globals as *mut Vec<Value>;
                            let func_ptr = jit_function.function_ptr;

                            let jit_result =
                                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                    func_ptr(
                                        stack_ptr,
                                        pc_ptr,
                                        bp_ptr,
                                        sp_ptr,
                                        hp_ptr,
                                        heap_ptr,
                                        globals_ptr,
                                    )
                                }));

                            match jit_result {
                                Ok(()) => {
                                    eprintln!(
                                        "JIT: Successfully executed compiled function '{}'",
                                        func_name
                                    );
                                }
                                Err(_) => {
                                    eprintln!("JIT: Execution failed for function '{}', falling back to interpreter", func_name);
                                    // Fall through to interpreter execution
                                    self.pc = new_pc;
                                    self.print_debug_visualization(
                                        pc_before_execution,
                                        instruction,
                                    );
                                    return Ok(ControlFlow::Continue);
                                }
                            }

                            // Print debug visualization (heap and/or stack) if enabled
                            self.print_debug_visualization(pc_before_execution, instruction);

                            // JIT execution completed, return immediately
                            return Ok(ControlFlow::Continue);
                        }
                    }
                    let func_name = self
                        .jit_function_addresses
                        .get(&new_pc)
                        .map(|s| s.as_str())
                        .unwrap_or("unknown");
                    eprintln!("JIT: Failed to get compiled function for '{}' at address {} (fallback to interpreter)", func_name, new_pc);
                }

                // Fallback to interpreter execution
                self.pc = new_pc;

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::Ret => {
                let return_addr = self.stack.pop();
                match return_addr {
                    Some(Value::Address(addr)) => {
                        self.pc = addr;
                    }
                    Some(Value::Int(n)) if n == -1 => {
                        let value = self.stack.pop();
                        match value {
                            Some(Value::Int(n)) => return Ok(ControlFlow::Exit(n)),
                            Some(Value::Byte(b)) => return Ok(ControlFlow::Exit(b as i64)),
                            Some(Value::Boolean(b)) => {
                                return Ok(ControlFlow::Exit(if b { 1 } else { 0 }))
                            }
                            Some(Value::Address(addr)) => {
                                return Ok(ControlFlow::Exit(addr as i64))
                            }
                            Some(Value::HeapRef(heap_index)) => {
                                return Ok(ControlFlow::Exit(heap_index.0 as i64))
                            }
                            None => return Err("Stack underflow for Ret".to_string()),
                        }
                    }
                    value => {
                        return Err(format!(
                            "Return address must be an address, but got {:?} [{}]",
                            value, self.pc,
                        ))
                    }
                }
            }

            Instruction::GetBP => {
                self.stack.push(Value::Address(self.bp));
            }

            Instruction::SetBP => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetBP".to_string());
                }
                let value = self.stack.pop().unwrap();
                match value {
                    Value::Address(addr) => {
                        self.bp = addr;
                    }
                    Value::Int(n) => {
                        self.bp = n as usize;
                    }
                    _ => return Err("SetBP requires an address or number".to_string()),
                }
            }

            Instruction::GetSP => {
                self.stack.push(Value::Address(self.stack.len()));
            }

            Instruction::SetSP => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetSP".to_string());
                }
                let value = self.stack.pop().unwrap();
                match value {
                    Value::Address(addr) => {
                        if addr > self.stack.len() {
                            // Extend stack
                            self.stack.resize(addr, Value::Int(0));
                        } else {
                            // Truncate stack
                            self.stack.truncate(addr);
                        }
                    }
                    Value::Int(n) => {
                        let new_sp = n as usize;
                        if new_sp > self.stack.len() {
                            // Extend stack
                            self.stack.resize(new_sp, Value::Int(0));
                        } else {
                            // Truncate stack
                            self.stack.truncate(new_sp);
                        }
                    }
                    _ => return Err("SetSP requires an address or number".to_string()),
                }
            }

            Instruction::GetHP => {
                self.stack.push(Value::Address(self.hp));
            }

            Instruction::SetHP => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetHP".to_string());
                }
                let value = self.stack.pop().unwrap();
                let new_hp = match value {
                    Value::Address(addr) => addr,
                    Value::Int(n) => n as usize,
                    _ => return Err("SetHP requires an address or number".to_string()),
                };

                // Extend heap if HP is advanced beyond current heap size
                while self.heap.len() < new_hp {
                    self.heap.push(Value::Int(0)); // Initialize new heap slots with zero
                }

                self.hp = new_hp;
            }

            Instruction::GetPC => {
                self.stack.push(Value::Address(self.pc));
            }

            Instruction::SetPC => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetPC".to_string());
                }
                let value = self.stack.pop().unwrap();
                match value {
                    Value::Address(addr) => {
                        self.pc = addr;
                    }
                    Value::Int(n) => {
                        self.pc = n as usize;
                    }
                    _ => return Err("SetPC requires an address or number".to_string()),
                }
            }

            Instruction::Nop => {
                // Do nothing
            }

            // (HeapAlloc removed - now handled via GetHP/SetHP)

            // (HeapGet removed - now handled via Load)

            // (HeapSet removed - now handled via Store)

            // (HeapGetOffset removed - now handled via AddressAdd + Load)

            // (HeapSetOffset removed - now handled via AddressAdd + Store)
            Instruction::Load => {
                // Stack: [heap_ref] -> [value]
                // Load value from heap at specified reference
                if self.stack.is_empty() {
                    return Err("Stack underflow for Load".to_string());
                }
                let heap_ref = self.stack.pop().unwrap();
                match heap_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        let value = &self.heap[heap_index.0];
                        self.stack.push(value.clone());
                    }
                    Value::Address(addr) => {
                        if addr >= self.heap.len() {
                            return Err(format!("Invalid address: {}", addr));
                        }
                        let value = &self.heap[addr];
                        self.stack.push(value.clone());
                    }
                    _ => return Err("Load requires a heap reference or address".to_string()),
                }
            }

            Instruction::Store => {
                // Stack: [value] [heap_ref] -> []
                // Store value to heap at specified reference
                let stack_len = self.stack.len();
                if stack_len < 2 {
                    return Err("Stack underflow for Store".to_string());
                }
                let heap_ref = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();
                match heap_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        // Set the value as a RawValue in the heap
                        self.heap[heap_index.0] = value;
                    }
                    Value::Address(addr) => {
                        if addr >= self.heap.len() {
                            return Err(format!("Invalid address: {}", addr));
                        }
                        self.heap[addr] = value;
                    }
                    _ => return Err("Store requires a heap reference or address".to_string()),
                }
            }

            Instruction::Syscall => {
                // Stack: [return_placeholder] [syscall_number] [fd] [buffer] [length] (pushed left-to-right)
                // For syscall(1): write(fd, buffer, length)
                let stack_len = self.stack.len();
                if stack_len < 5 {
                    return Err("Stack underflow for Syscall".to_string());
                }

                // Pop the 4 arguments (length, buffer, fd, syscall_number) in reverse order
                let length = self.stack.pop().unwrap();
                let buffer_ref = self.stack.pop().unwrap();
                let fd = self.stack.pop().unwrap();
                let syscall_number = self.stack.pop().unwrap();

                // The return placeholder is left on the stack for the result

                match syscall_number {
                    Value::Int(1) => {
                        // Write syscall: write(fd, buffer, length)

                        // Validate fd is a number
                        let _fd_num = match fd {
                            Value::Int(n) => n,
                            _ => return Err("Write syscall: fd must be a number".to_string()),
                        };

                        // Validate length is a number
                        let length_num = match length {
                            Value::Int(n) => n as usize,
                            _ => return Err("Write syscall: length must be a number".to_string()),
                        };

                        // Get buffer content - now expecting [*]byte (pointer) directly
                        match buffer_ref {
                            Value::Address(data_addr) => {
                                // data_addr points directly to byte data (from s.data)
                                let string_content = self.address_to_string(data_addr)?;
                                let actual_length = length_num.min(string_content.len());
                                let output = &string_content[..actual_length];

                                if let Some(ref mut captured) = self.captured_output {
                                    captured.push_str(output);
                                } else {
                                    print!("{}", output);
                                    use std::io::Write;
                                    std::io::stdout().flush().unwrap();
                                }

                                // Push the actual number of bytes written as return value
                                self.stack.push(Value::Int(actual_length as i64));
                            }
                            Value::HeapRef(heap_index) => {
                                // Handle array(byte) structures stored as RawValue
                                if heap_index.0 >= self.heap.len() {
                                    return Err(format!("Invalid heap index: {}", heap_index.0));
                                }

                                // New RawValue-based approach: read the struct fields directly from heap
                                // For array(byte), we expect: [data_address, length, capacity] at consecutive heap locations
                                let data_addr = match &self.heap[heap_index.0] {
                                    Value::Address(addr) => *addr,
                                    Value::HeapRef(heap_ref) => {
                                        // If data is a heap reference, treat it as the start address
                                        heap_ref.0
                                    }
                                    _ => {
                                        return Err("Write syscall: array.data field must be an address or heap reference".to_string());
                                    }
                                };

                                // Extract string from the data address
                                let string_content = if data_addr < self.heap.len() {
                                    // Read bytes from heap starting at data_addr
                                    let mut bytes = Vec::new();
                                    let mut current_addr = data_addr;

                                    while current_addr < self.heap.len() {
                                        match &self.heap[current_addr] {
                                            Value::Byte(byte) => {
                                                if *byte == 0 {
                                                    break; // Null terminator
                                                }
                                                bytes.push(*byte);
                                            }
                                            _ => break,
                                        }
                                        current_addr += 1;
                                    }

                                    String::from_utf8_lossy(&bytes).to_string()
                                } else {
                                    return Err("Write syscall: invalid data address".to_string());
                                };

                                let actual_length = length_num.min(string_content.len());
                                let output = &string_content[..actual_length];

                                if let Some(ref mut captured) = self.captured_output {
                                    captured.push_str(output);
                                } else {
                                    print!("{}", output);
                                    use std::io::Write;
                                    std::io::stdout().flush().unwrap();
                                }

                                self.stack.push(Value::Int(actual_length as i64));
                            }
                            _ => {
                                return Err(
                                    "Write syscall: buffer must be a string address or array(byte) structure".to_string()
                                )
                            }
                        }
                    }
                    _ => {
                        return Err(format!("Unsupported syscall number: {:?}", syscall_number));
                    }
                }
            }
        }

        // Record profiling data if enabled
        if let Some(elapsed) = timer.finish() {
            let instruction_name = format!("{:?}", instruction)
                .split('(')
                .next()
                .unwrap_or("Unknown")
                .to_string();
            self.profiler.record_instruction(instruction_name, elapsed);

            // Special handling for Call instructions
            if let Instruction::Call(func_name) = instruction {
                self.profiler.record_function_call(func_name.clone());
            }
        }

        self.pc += 1;

        // Print debug visualization (heap and/or stack) if enabled
        self.print_debug_visualization(pc_before_execution, instruction);

        Ok(ControlFlow::Continue)
    }

    pub fn execute(&mut self) -> Result<i64, String> {
        // Initialize BP to point to the start of the stack
        self.bp = 0;

        while self.pc < self.program.len() {
            if let ControlFlow::Exit(n) = self.step()? {
                return Ok(n);
            }
        }

        // Program ended, return top of stack or 0
        if self.stack.is_empty() {
            Ok(0)
        } else {
            let value = self.stack.pop().unwrap();
            match value {
                Value::Int(n) => Ok(n as i64),
                Value::Boolean(b) => Ok(if b { 1 } else { 0 }),
                Value::Byte(b) => Ok(b as i64),
                Value::Address(addr) => Ok(addr as i64),
                Value::HeapRef(heap_index) => Ok(heap_index.0 as i64),
            }
        }
    }

    /// Reset the VM state for a fresh execution
    pub fn reset(&mut self) {
        self.stack.clear();
        self.pc = 0;
        self.bp = 0;
        self.sp = 0;
        self.hp = 0;
        self.heap.clear();
        // Keep print_stacks and profiler settings unchanged
        // Reset captured output if it was enabled
        if self.captured_output.is_some() {
            self.captured_output = Some(String::new());
        }

        // Reset profiling data if profiling is enabled
        self.profiler.clear();
    }

    /// Dump profiling results to a string
    pub fn dump_profile(&self) -> String {
        self.profiler.generate_report()
    }

    /// Dump profiling results to a file
    pub fn dump_profile_to_file(&self, filename: &str) -> Result<(), std::io::Error> {
        self.profiler.save_report_to_file(filename)
    }

    /// Get function call TOP5 ranking for --print-timings
    pub fn get_function_call_top5(&self) -> String {
        self.profiler.generate_function_call_top5()
    }

    /// Get the current program counter for debugging
    pub fn get_program_counter(&self) -> usize {
        self.pc
    }

    /// Get the current instruction for debugging
    pub fn get_current_instruction(&self) -> Option<&Instruction> {
        if self.pc < self.program.len() {
            Some(&self.program[self.pc])
        } else {
            None
        }
    }

    /// Get a snapshot of the stack values for debugging
    pub fn get_stack(&self) -> Vec<Value> {
        self.stack.iter().collect()
    }

    /// Extract function instructions starting from the given address until Ret
    fn extract_function_instructions(&self, start_addr: usize) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        let mut current_addr = start_addr;

        while current_addr < self.program.len() {
            let instruction = &self.program[current_addr];
            instructions.push(instruction.clone());

            // Stop at Ret instruction
            if matches!(instruction, Instruction::Ret) {
                break;
            }

            current_addr += 1;
        }

        instructions
    }

    /// Calculate the length of a null-terminated string starting at the given address
    fn string_length(&self, start_addr: usize) -> Result<usize, String> {
        let mut length = 0;
        let mut current_addr = start_addr;

        while current_addr < self.heap.len() {
            match &self.heap[current_addr] {
                Value::Byte(byte) => {
                    if *byte == 0 {
                        return Ok(length);
                    }
                    length += 1;
                    current_addr += 1;
                }
                _ => return Err("Invalid byte in string".to_string()),
            }
        }

        Err("String not null-terminated".to_string())
    }

    /// Convert address-based string to Rust String for compatibility
    fn address_to_string(&self, start_addr: usize) -> Result<String, String> {
        let length = self.string_length(start_addr)?;
        let mut bytes = Vec::with_capacity(length);

        for i in 0..length {
            match &self.heap[start_addr + i] {
                Value::Byte(byte) => {
                    bytes.push(*byte);
                }
                _ => return Err("Invalid byte in string".to_string()),
            }
        }

        String::from_utf8(bytes).map_err(|_| "Invalid UTF-8 in string".to_string())
    }

    /// Print combined heap and stack visualization
    fn print_debug_visualization(&self, pc_before_execution: usize, instruction: &Instruction) {
        if !self.print_heaps && !self.print_stacks {
            return;
        }

        // Start the line with PC and instruction
        print!(
            "{:04} {:20}",
            pc_before_execution,
            format!("{}", instruction)
        );

        // Add heap visualization if enabled
        if self.print_heaps {
            // Block characters for memory usage visualization
            const BLOCKS: &[char] = &[' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

            print!(" [");

            // Always show 8 blocks (representing 64 cells total), regardless of heap size
            for block_index in 0..8 {
                let block_start = block_index * 8;

                // Check how many cells in this 8-cell block are actually used
                let occupied_count = if block_start < self.heap.len() {
                    std::cmp::min(self.heap.len() - block_start, 8)
                } else {
                    0
                };

                // Map 0-8 occupied cells to block characters (0-8 index)
                print!("{}", BLOCKS[occupied_count]);
            }

            print!("]");
        }

        // Add stack visualization if enabled
        if self.print_stacks {
            print!(
                " [{}]",
                self.stack
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        println!();
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
            vm: VM::with_all_options(print_stacks, false, print_stacks_on_call, false),
        }
    }

    pub fn new_with_debug_options(
        print_stacks: bool,
        print_heaps: bool,
        print_stacks_on_call: Option<String>,
    ) -> Self {
        Runtime {
            vm: VM::with_all_options(print_stacks, print_heaps, print_stacks_on_call, false),
        }
    }

    /// Execute a complete program by compiling to VM bytecode
    pub fn execute_program(&mut self, program: &Program) -> Result<Option<Value>> {
        // Compile program to VM bytecode
        let mut compiler = CodeGenerator::new();
        let instructions = compiler.compile_program(program);

        // Apply label resolution
        let mut label_resolver = LabelResolver::new();
        let resolved_instructions = label_resolver
            .resolve_labels(instructions)
            .map_err(|e| anyhow::anyhow!("Label resolution error: {}", e))?;

        // Execute on VM
        self.vm.reset();
        self.vm.load_program(resolved_instructions);

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Int(result))),
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
        let mut compiler = CodeGenerator::new();
        let instructions = compiler.compile_program(program);

        // Apply label resolution
        let mut label_resolver = LabelResolver::new();
        let resolved_instructions = label_resolver
            .resolve_labels(instructions)
            .map_err(|e| anyhow::anyhow!("Label resolution error: {}", e))?;

        // Execute on VM with stack printing option
        self.vm.print_stacks = print_stacks;
        self.vm.reset();
        self.vm.load_program(resolved_instructions);

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Int(result))),
            Err(err) => bail!("VM execution error: {}", err),
        }
    }

    /// Execute pre-resolved instructions directly
    pub fn execute_instructions(&mut self, instructions: &[Instruction]) -> Result<Option<Value>> {
        // Execute on VM
        self.vm.reset();
        self.vm.load_program(instructions.to_vec());

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Int(result))),
            Err(err) => bail!("VM execution error: {}", err),
        }
    }

    /// Execute pre-resolved instructions with options (like stack printing)
    pub fn execute_instructions_with_options(
        &mut self,
        instructions: &[Instruction],
        print_stacks: bool,
    ) -> Result<Option<Value>> {
        // Execute on VM with stack printing option
        self.vm.print_stacks = print_stacks;
        self.vm.reset();
        self.vm.load_program(instructions.to_vec());

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Int(result))),
            Err(err) => bail!("VM execution error: {}", err),
        }
    }

    /// Enable profiling in the VM
    pub fn enable_profiling(&mut self) {
        self.vm.profiler.enable();
    }

    /// Set JIT compile functions for forced compilation
    pub fn set_jit_compile_functions(&mut self, jit_functions: HashMap<String, usize>) {
        self.vm.set_jit_compile_functions(jit_functions);
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

    /// Enable output capture for testing
    pub fn enable_output_capture(&mut self) {
        self.vm.enable_output_capture();
    }

    /// Get captured output and clear the buffer
    pub fn take_captured_output(&mut self) -> Option<String> {
        self.vm.take_captured_output()
    }

    /// Get function call TOP5 ranking for --print-timings
    pub fn get_function_call_top5(&self) -> String {
        self.vm.get_function_call_top5()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Instruction;

    #[test]
    fn test_heap_memory_allocation() {
        let mut vm = VM::new();

        // Test GetHP/SetHP: allocate 3 values on heap
        vm.load_program(vec![
            // Simple allocation: get current HP, then advance it
            Instruction::GetHP,       // [current_hp]
            Instruction::GetLocal(0), // [current_hp, current_hp] (duplicate)
            Instruction::Push(3),     // [current_hp, current_hp, 3]
            Instruction::AddressAdd,  // [current_hp, new_hp]
            Instruction::SetHP,       // [current_hp] (HP = new_hp, heap extended)
        ]);

        // Execute step by step to check intermediate state
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        // Check that heap has been allocated
        assert_eq!(vm.hp, 3);
        // Stack should contain heap start address
        assert_eq!(vm.stack.len(), 1);
        if let Some(Value::Address(addr)) = vm.stack.get(0) {
            assert_eq!(addr, 0); // First allocation should start at 0
        } else {
            panic!("Expected Address on stack");
        }
    }

    #[test]
    fn test_heap_get_set() {
        let mut vm = VM::new();

        // Test Load/Store with allocated memory - use working pattern from successful test
        vm.load_program(vec![
            // Allocate heap space by advancing HP (same pattern as test_heap_memory_allocation)
            Instruction::GetHP,       // [current_hp]
            Instruction::GetLocal(0), // [current_hp, current_hp] (duplicate)
            Instruction::Push(2),     // [current_hp, current_hp, 2]
            Instruction::AddressAdd,  // [current_hp, new_hp]
            Instruction::SetHP,       // [heap_start_addr] (HP updated)
            // Set value at address: Store 42 at heap_start_addr
            Instruction::Push(42),    // [heap_start_addr, 42]
            Instruction::GetLocal(0), // [heap_start_addr, 42, heap_start_addr]
            Instruction::Store,       // [heap_start_addr] (store 42 at heap_start_addr)
            // Get value from address: Load from heap_start_addr
            Instruction::Load, // [42] (load from heap_start_addr)
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        let stack = vm.get_stack();
        // Stack should contain: [retrieved_value]
        assert_eq!(stack.len(), 1);

        // Check that we got the value back
        if let Value::Int(value) = &stack[0] {
            assert_eq!(*value, 42);
        } else {
            panic!("Expected Int(42), got {:?}", stack[0]);
        }
    }

    #[test]
    fn test_load_store_operations() {
        let mut vm = VM::new();

        // Test Load/Store with allocated memory using GetHP/SetHP
        vm.load_program(vec![
            // Allocate heap space by advancing HP (same pattern as test_heap_memory_allocation)
            Instruction::GetHP,       // [current_hp]
            Instruction::GetLocal(0), // [current_hp, current_hp] (duplicate)
            Instruction::Push(3),     // [current_hp, current_hp, 3]
            Instruction::AddressAdd,  // [current_hp, new_hp]
            Instruction::SetHP,       // [heap_start_addr] (HP updated)
            // Store value 42 at heap_start_addr
            Instruction::Push(42),    // [heap_start_addr, 42]
            Instruction::GetLocal(0), // [heap_start_addr, 42, heap_start_addr]
            Instruction::Store,       // [heap_start_addr]
            // Load value from heap_start_addr
            Instruction::Load, // [42]
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        // Check final result
        assert_eq!(vm.stack.len(), 1); // loaded value only
        if let Some(Value::Int(n)) = vm.stack.peek() {
            assert_eq!(n, 42);
        } else {
            panic!("Expected Int(42) on stack top");
        }
    }

    #[test]
    fn test_heap_offset_operations() {
        let mut vm = VM::new();

        // Test AddressAdd + Load/Store (replacement for HeapGetOffset/HeapSetOffset)
        vm.load_program(vec![
            // Allocate heap space by advancing HP (same pattern as test_heap_memory_allocation)
            Instruction::GetHP,       // [current_hp]
            Instruction::GetLocal(0), // [current_hp, current_hp] (duplicate)
            Instruction::Push(3),     // [current_hp, current_hp, 3]
            Instruction::AddressAdd,  // [current_hp, new_hp]
            Instruction::SetHP,       // [heap_start_addr] (HP updated)
            // Set value at offset 1: Store 100 at heap_start_addr + 1
            Instruction::Push(100),   // [heap_start_addr, 100]
            Instruction::GetLocal(0), // [heap_start_addr, 100, heap_start_addr]
            Instruction::Push(1),     // [heap_start_addr, 100, heap_start_addr, 1]
            Instruction::AddressAdd,  // [heap_start_addr, 100, target_addr]
            Instruction::Store,       // [heap_start_addr]
            // Get value from offset 1: Load from heap_start_addr + 1
            Instruction::Push(1),    // [heap_start_addr, 1]
            Instruction::AddressAdd, // [target_addr]
            Instruction::Load,       // [100]
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        let stack = vm.get_stack();
        // Stack should contain: [retrieved_value]
        assert_eq!(stack.len(), 1);

        // Check that we got the value back
        if let Value::Int(value) = &stack[0] {
            assert_eq!(*value, 100);
        } else {
            panic!("Expected Int(100), got {:?}", stack[0]);
        }
    }
}
