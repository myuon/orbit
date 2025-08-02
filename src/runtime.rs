use crate::codegen::CodeGenerator;
use crate::jit::ARM64JITCompiler;
use crate::label_resolution::LabelResolver;
use crate::profiler::InstructionTimer;
use crate::value_encoding::ValueEncoder;
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

/// Information about an external jump from a jump block
#[derive(Debug, Clone)]
pub struct ExternalJump {
    pub block_index: usize,   // Index within the block instructions
    pub absolute_addr: usize, // Absolute address in program
    pub target_addr: usize,   // Target address of the jump
    pub jump_type: ExternalJumpType,
}

#[derive(Debug, Clone)]
pub enum ExternalJumpType {
    JumpRel(i32),
    JumpIfZeroRel(i32),
    CallRel(i32),
}

#[derive(Debug)]
pub struct VM {
    stack: Box<[u64]>, // JIT-compatible encoded stack with direct u64 storage
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
    // Jump counts for JIT compilation decision (jump_target_address -> count)
    jump_counts: HashMap<usize, u64>,
    // Functions marked for JIT compilation (function_name -> label_address)
    jit_compile_functions: HashMap<String, usize>,
    // Reverse mapping for JIT functions (label_address -> function_name)
    jit_function_addresses: HashMap<usize, String>,
    // Print JIT compiled assembly as hexdump
    print_jit_asm: bool,
    // Set of function addresses that failed JIT compilation
    jit_failed_functions: std::collections::HashSet<usize>,
    // Whether JIT compilation is disabled
    jit_disabled: bool,
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
            stack: vec![0u64; crate::STACK_SIZE].into_boxed_slice(),
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
            jump_counts: HashMap::new(),
            jit_compile_functions: HashMap::new(),
            jit_function_addresses: HashMap::new(),
            print_jit_asm: false,
            jit_failed_functions: std::collections::HashSet::new(),
            jit_disabled: false,
        }
    }

    pub fn with_all_options(
        print_stacks: bool,
        print_heaps: bool,
        print_stacks_on_call: Option<String>,
        enable_profiling: bool,
    ) -> Self {
        Self {
            stack: vec![0u64; 1024].into_boxed_slice(),
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
            jump_counts: HashMap::new(),
            jit_compile_functions: HashMap::new(),
            jit_function_addresses: HashMap::new(),
            print_jit_asm: false,
            jit_failed_functions: std::collections::HashSet::new(),
            jit_disabled: false,
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

    pub fn set_print_jit_asm(&mut self, print_jit_asm: bool) {
        self.print_jit_asm = print_jit_asm;
    }

    /// Set JIT compile output file
    pub fn set_jit_compile_output(&mut self, output_file: Option<String>) {
        if let Some(ref mut jit_compiler) = self.jit_compiler {
            jit_compiler.set_jit_compile_output(output_file);
        }
    }

    /// Disable JIT compilation
    pub fn disable_jit(&mut self) {
        eprintln!("[INFO] JIT: JIT compilation disabled");
        self.jit_disabled = true;
    }

    /// Get the raw encoded stack for JIT operations
    pub fn get_encoded_stack(&self) -> &[u64] {
        &self.stack[..self.sp]
    }

    /// Get mutable access to the raw encoded stack for JIT operations
    pub fn get_encoded_stack_mut(&mut self) -> &mut [u64] {
        &mut self.stack[..self.sp]
    }

    /// Push a value onto the stack (encodes automatically)
    fn push_value(&mut self, value: Value) -> Result<(), String> {
        if self.sp >= self.stack.len() {
            return Err(format!(
                "Stack overflow: SP {} exceeds capacity {}",
                self.sp,
                self.stack.len()
            ));
        }

        let encoded = ValueEncoder::encode(&value);
        self.stack[self.sp] = encoded;
        self.sp += 1;
        Ok(())
    }

    /// Pop a value from the stack (decodes automatically)
    fn pop_value(&mut self) -> Result<Value, String> {
        if self.sp == 0 {
            return Err("Stack underflow: SP is 0".to_string());
        }

        self.sp -= 1;
        let encoded = self.stack[self.sp];
        Ok(ValueEncoder::decode(encoded))
    }

    /// Get a value at specific stack index (0-based from bottom)
    fn get_stack_value(&self, index: usize) -> Option<Value> {
        if index < self.sp {
            Some(ValueEncoder::decode(self.stack[index]))
        } else {
            None
        }
    }

    /// Set a value at specific stack index (0-based from bottom)
    fn set_stack_value(&mut self, index: usize, value: Value) -> Result<(), String> {
        if index >= self.stack.len() {
            return Err(format!(
                "Index {} out of bounds for stack capacity {}",
                index,
                self.stack.len()
            ));
        }

        let encoded = ValueEncoder::encode(&value);
        self.stack[index] = encoded;

        // Update SP if we're setting beyond current SP
        if index >= self.sp {
            self.sp = index + 1;
        }

        Ok(())
    }

    /// Check if stack is empty
    fn is_stack_empty(&self) -> bool {
        self.sp == 0
    }

    /// Clear the stack
    fn clear_stack(&mut self) {
        self.sp = 0;
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
        let instruction = self.program[self.pc].clone();
        let pc_before_execution = self.pc;

        // Start timing if profiling is enabled
        let timer = InstructionTimer::start(self.profiler.enabled);

        match instruction {
            Instruction::Label(_) => {
                // Labels are just markers, no action needed
            }

            Instruction::Push(value) => {
                if self.sp >= self.stack.len() {
                    let error_msg = format!(
                        "Stack overflow: SP {} exceeds capacity {}",
                        self.sp,
                        self.stack.len()
                    );
                    eprintln!("[ERROR] VM: {}", error_msg);
                    return Err(error_msg);
                }
                let encoded = ValueEncoder::encode(&Value::Int(value));
                self.stack[self.sp] = encoded;
                self.sp += 1;
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
                let encoded = ValueEncoder::encode(&Value::Address(start_index));
                self.stack[self.sp] = encoded;
                self.sp += 1;
                // Update HP to keep it in sync with heap length
                self.hp = self.heap.len();
            }

            Instruction::PushHeapRef(index) => {
                if index >= self.heap.len() {
                    return Err(format!("Invalid heap index: {}", index));
                }
                let encoded = ValueEncoder::encode(&Value::HeapRef(HeapIndex(index)));
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::PushAddress(addr) => {
                let encoded = ValueEncoder::encode(&Value::Address(addr));
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::Pop => {
                if self.sp == 0 {
                    return Err("Stack underflow: SP is 0".to_string());
                }
                self.sp -= 1;
            }

            Instruction::Add => {
                if self.sp < 2 {
                    return Err("Stack underflow for Add".to_string());
                }
                // Pop b
                self.sp -= 1;
                let b = ValueEncoder::decode(self.stack[self.sp]);
                // Pop a
                self.sp -= 1;
                let a = ValueEncoder::decode(self.stack[self.sp]);

                let result = match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    (Value::Int(a), Value::Byte(b)) => Value::Int(a + b as i64),
                    (Value::Byte(a), Value::Int(b)) => Value::Int(a as i64 + b),
                    (Value::Byte(a), Value::Byte(b)) => Value::Int(a as i64 + b as i64),
                    _ => return Err("Add operation requires numbers or bytes".to_string()),
                };

                // Push result
                let encoded = ValueEncoder::encode(&result);
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::Sub => {
                if self.sp < 2 {
                    return Err("Stack underflow for Sub".to_string());
                }
                // Pop b
                self.sp -= 1;
                let b = ValueEncoder::decode(self.stack[self.sp]);
                // Pop a
                self.sp -= 1;
                let a = ValueEncoder::decode(self.stack[self.sp]);

                let result = match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                    (Value::Int(a), Value::Byte(b)) => Value::Int(a - b as i64),
                    (Value::Byte(a), Value::Int(b)) => Value::Int(a as i64 - b),
                    (Value::Byte(a), Value::Byte(b)) => Value::Int(a as i64 - b as i64),
                    _ => return Err("Subtract operation requires numbers or bytes".to_string()),
                };

                // Push result
                let encoded = ValueEncoder::encode(&result);
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::Mul => {
                if self.sp < 2 {
                    return Err("Stack underflow for Mul".to_string());
                }
                // Pop b
                self.sp -= 1;
                let b = ValueEncoder::decode(self.stack[self.sp]);
                // Pop a
                self.sp -= 1;
                let a = ValueEncoder::decode(self.stack[self.sp]);

                let result = match (a, b) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                    (Value::Int(a), Value::Byte(b)) => Value::Int(a * b as i64),
                    (Value::Byte(a), Value::Int(b)) => Value::Int(a as i64 * b),
                    (Value::Byte(a), Value::Byte(b)) => Value::Int(a as i64 * b as i64),
                    _ => return Err("Multiply operation requires numbers or bytes".to_string()),
                };

                // Push result
                let encoded = ValueEncoder::encode(&result);
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::Div => {
                if self.sp < 2 {
                    return Err("Stack underflow for Div".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            eprintln!("[ERROR] VM: Division by zero attempted");
                            return Err("Division by zero".to_string());
                        }
                        self.push_value(Value::Int(a / b))?;
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.push_value(Value::Int(a / b as i64))?;
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.push_value(Value::Int(a as i64 / b))?;
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        if b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        self.push_value(Value::Int(a as i64 / b as i64))?;
                    }
                    _ => return Err("Divide operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Mod => {
                if self.sp < 2 {
                    return Err("Stack underflow for Mod".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Modulo by zero".to_string());
                        }
                        self.push_value(Value::Int(a % b))?;
                    }
                    _ => return Err("Modulo operation requires numbers".to_string()),
                }
            }

            Instruction::AddressAdd => {
                if self.sp < 2 {
                    return Err("Stack underflow for AddressAdd".to_string());
                }
                // Pop b (second operand - index)
                self.sp -= 1;
                let b = ValueEncoder::decode(self.stack[self.sp]);
                // Pop a (first operand - container)
                self.sp -= 1;
                let a = ValueEncoder::decode(self.stack[self.sp]);

                let result = match (&a, &b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        Value::Address(addr + *offset as usize)
                    }
                    (Value::HeapRef(heap_ref), Value::Int(offset)) => {
                        // HeapRef + offset = new HeapRef with adjusted index
                        Value::HeapRef(HeapIndex(heap_ref.0 + *offset as usize))
                    }
                    (Value::Int(offset), Value::HeapRef(heap_ref)) => {
                        // Handle reversed order: Int + HeapRef -> HeapRef
                        Value::HeapRef(HeapIndex(heap_ref.0 + *offset as usize))
                    }
                    _ => {
                        return Err(format!(
                            "AddressAdd requires Address/HeapRef + Number, got {:?} + {:?}",
                            a, b
                        ))
                    }
                };

                // Push result
                let encoded = ValueEncoder::encode(&result);
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::AddressSub => {
                if self.sp < 2 {
                    return Err("Stack underflow for AddressSub".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (&a, &b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        self.push_value(Value::Address(addr - *offset as usize))?;
                    }
                    (Value::HeapRef(heap_ref), Value::Int(offset)) => {
                        // HeapRef - offset = new HeapRef with adjusted index
                        self.push_value(Value::HeapRef(HeapIndex(heap_ref.0 - *offset as usize)))?;
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
                if self.sp < 2 {
                    return Err("Stack underflow for Eq".to_string());
                }
                // Pop b
                self.sp -= 1;
                let b = ValueEncoder::decode(self.stack[self.sp]);
                // Pop a
                self.sp -= 1;
                let a = ValueEncoder::decode(self.stack[self.sp]);

                // Push result
                let encoded = ValueEncoder::encode(&Value::Boolean(a == b));
                self.stack[self.sp] = encoded;
                self.sp += 1;
            }

            Instruction::Lt => {
                if self.sp < 2 {
                    return Err("Stack underflow for Lt".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.push_value(Value::Boolean(a < b))?;
                    }
                    (Value::Int(a), Value::Byte(b)) => {
                        self.push_value(Value::Boolean(a < b as i64))?;
                    }
                    (Value::Byte(a), Value::Int(b)) => {
                        self.push_value(Value::Boolean((a as i64) < b))?;
                    }
                    (Value::Byte(a), Value::Byte(b)) => {
                        self.push_value(Value::Boolean(a < b))?;
                    }
                    _ => return Err("Less than operation requires numbers or bytes".to_string()),
                }
            }

            Instruction::Lte => {
                if self.sp < 2 {
                    return Err("Stack underflow for Lte".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.push_value(Value::Boolean(a <= b))?;
                    }
                    _ => return Err("Less than or equal operation requires numbers".to_string()),
                }
            }

            Instruction::Gt => {
                if self.sp < 2 {
                    return Err("Stack underflow for Gt".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.push_value(Value::Boolean(a > b))?;
                    }
                    _ => return Err("Greater than operation requires numbers".to_string()),
                }
            }

            Instruction::Gte => {
                if self.sp < 2 {
                    return Err("Stack underflow for Gte".to_string());
                }
                let b = self.pop_value()?;
                let a = self.pop_value()?;
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.push_value(Value::Boolean(a >= b))?;
                    }
                    _ => return Err("Greater than or equal operation requires numbers".to_string()),
                }
            }

            Instruction::Not => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for Not".to_string());
                }
                let value = self.pop_value()?;
                match value {
                    Value::Boolean(b) => {
                        self.push_value(Value::Boolean(!b))?;
                    }
                    Value::Int(n) => {
                        self.push_value(Value::Boolean(n == 0))?;
                    }
                    Value::Byte(b) => {
                        self.push_value(Value::Boolean(b == 0))?;
                    }
                    _ => return Err("Not operation requires boolean, number, or byte".to_string()),
                }
            }

            Instruction::Jump(_label) => {
                return Err(
                    "Jump with label should have been resolved to JumpRel before execution"
                        .to_string(),
                );
            }

            Instruction::JumpIfZero(_label) => {
                return Err("JumpIfZero with label should have been resolved to JumpIfZeroRel before execution".to_string());
            }

            Instruction::JumpRel(offset) => {
                let new_pc = (self.pc as i32 + offset) as usize;

                // Track jump counts (for potential future JIT compilation)
                let count = self.jump_counts.entry(new_pc).or_insert(0);
                *count += 1;

                // Try to execute JIT compiled function first
                if let Some(ref mut jit_compiler) = self.jit_compiler {
                    if jit_compiler.is_compiled(new_pc) {
                        match jit_compiler.execute_function(
                            new_pc,
                            self.stack.as_mut_ptr(),
                            &mut self.pc,
                            &mut self.bp,
                            &mut self.sp,
                            &mut self.hp,
                            &mut self.heap,
                            &mut self.globals,
                        ) {
                            Ok(()) => {
                                return Ok(ControlFlow::Continue);
                            }
                            Err(e) => {
                                eprintln!("[INFO] JIT: JIT execution failed at address {} - falling back to interpreter: {:?}", new_pc, e);
                                self.jit_failed_functions.insert(new_pc);
                            }
                        }
                    }
                }

                // Enable jump block JIT compilation
                if *count == 10 {
                    // First time hitting threshold
                    if let Some((block, start_addr, end_addr, external_jumps)) =
                        self.extract_jump_block_with_exits(new_pc)
                    {
                        if let Some(jit_block) = self.prepare_jit_block(new_pc) {
                            // Try JIT compilation with exit path handling
                            if external_jumps.is_empty() {
                                // Simple case without external jumps - compile the original block
                                if self.jit_compiler.is_some()
                                    && !self.jit_disabled
                                    && !self.jit_failed_functions.contains(&new_pc)
                                {
                                    let print_jit_asm = self.print_jit_asm;
                                    if let Some(ref mut jit_compiler) = self.jit_compiler {
                                        match jit_compiler.compile_jump_block(
                                            new_pc,
                                            &block,
                                            None, // No function prologue offset needed for simple blocks
                                            print_jit_asm,
                                        ) {
                                            Ok(()) => {
                                                eprintln!("[INFO] JIT: Successfully compiled jump block at address {}", new_pc);
                                            }
                                            Err(e) => {
                                                eprintln!("[INFO] JIT: Failed to compile jump block at address {}: {}", new_pc, e);
                                                self.jit_failed_functions.insert(new_pc);
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Complex case with external jumps - compile with fallbacks
                                if self.jit_compiler.is_some()
                                    && !self.jit_disabled
                                    && !self.jit_failed_functions.contains(&new_pc)
                                {
                                    let print_jit_asm = self.print_jit_asm;
                                    if let Some(ref mut jit_compiler) = self.jit_compiler {
                                        match jit_compiler.compile_jump_block(
                                            new_pc,
                                            &jit_block,
                                            None, // No function prologue offset needed
                                            print_jit_asm,
                                        ) {
                                            Ok(()) => {
                                                eprintln!("[INFO] JIT: Successfully compiled jump block with external jumps at address {}", new_pc);
                                            }
                                            Err(e) => {
                                                eprintln!("[INFO] JIT: Failed to compile jump block with external jumps at address {}: {}", new_pc, e);
                                                self.jit_failed_functions.insert(new_pc);
                                            }
                                        }
                                    }
                                }
                            }

                            if let Some(ref mut jit_compiler) = self.jit_compiler {
                                if jit_compiler.is_compiled(new_pc) {
                                    match jit_compiler.execute_function(
                                        new_pc,
                                        self.stack.as_mut_ptr(),
                                        &mut self.pc,
                                        &mut self.bp,
                                        &mut self.sp,
                                        &mut self.hp,
                                        &mut self.heap,
                                        &mut self.globals,
                                    ) {
                                        Ok(()) => {
                                            eprintln!("[INFO] JIT: Successfully executed jump block at address {}", new_pc);
                                            return Ok(ControlFlow::Continue);
                                        }
                                        Err(e) => {
                                            eprintln!("[INFO] JIT: Jump block execution failed at address {} - falling back to interpreter: {:?}", new_pc, e);
                                            self.jit_failed_functions.insert(new_pc);
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                    }
                }

                self.pc = new_pc;

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, &instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::JumpIfZeroRel(offset) => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for JumpIfZeroRel".to_string());
                }
                let value = self.pop_value()?;
                let should_jump = match value {
                    Value::Int(n) => n == 0,
                    Value::Boolean(b) => !b,
                    Value::Byte(b) => b == 0,
                    _ => false,
                };
                if should_jump {
                    let new_pc = (self.pc as i32 + offset) as usize;

                    // Track jump counts (for potential future JIT compilation)
                    let count = self.jump_counts.entry(new_pc).or_insert(0);
                    *count += 1;

                    // Try to execute JIT compiled function first
                    if let Some(ref mut jit_compiler) = self.jit_compiler {
                        if jit_compiler.is_compiled(new_pc) {
                            match jit_compiler.execute_function(
                                new_pc,
                                self.stack.as_mut_ptr(),
                                &mut self.pc,
                                &mut self.bp,
                                &mut self.sp,
                                &mut self.hp,
                                &mut self.heap,
                                &mut self.globals,
                            ) {
                                Ok(()) => {
                                    return Ok(ControlFlow::Continue);
                                }
                                Err(e) => {
                                    eprintln!("[INFO] JIT: JIT execution failed at address {} - falling back to interpreter: {:?}", new_pc, e);
                                    self.jit_failed_functions.insert(new_pc);
                                }
                            }
                        }
                    }

                    // If jump count exceeds threshold, try JIT compilation with exit path handling
                    if *count == 10 {
                        // First time hitting threshold
                        if let Some((block, start_addr, end_addr, external_jumps)) =
                            self.extract_jump_block_with_exits(new_pc)
                        {
                            if let Some(jit_block) = self.prepare_jit_block(new_pc) {
                                // Try JIT compilation with exit path handling
                                if external_jumps.is_empty() {
                                    // Simple case without external jumps - compile the original block
                                    if self.jit_compiler.is_some()
                                        && !self.jit_disabled
                                        && !self.jit_failed_functions.contains(&new_pc)
                                    {
                                        let print_jit_asm = self.print_jit_asm;
                                        if let Some(ref mut jit_compiler) = self.jit_compiler {
                                            match jit_compiler.compile_jump_block(
                                                new_pc,
                                                &block,
                                                None, // No function prologue offset needed for simple blocks
                                                print_jit_asm,
                                            ) {
                                                Ok(()) => {
                                                    eprintln!("[INFO] JIT: Successfully compiled jump block at address {}", new_pc);
                                                }
                                                Err(e) => {
                                                    eprintln!("[INFO] JIT: Failed to compile jump block at address {}: {}", new_pc, e);
                                                    self.jit_failed_functions.insert(new_pc);
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    // Complex case with external jumps - compile with fallbacks
                                    if self.jit_compiler.is_some()
                                        && !self.jit_disabled
                                        && !self.jit_failed_functions.contains(&new_pc)
                                    {
                                        let print_jit_asm = self.print_jit_asm;
                                        if let Some(ref mut jit_compiler) = self.jit_compiler {
                                            match jit_compiler.compile_jump_block(
                                                new_pc,
                                                &jit_block,
                                                None, // No function prologue offset needed
                                                print_jit_asm,
                                            ) {
                                                Ok(()) => {
                                                    eprintln!("[INFO] JIT: Successfully compiled jump block with external jumps at address {}", new_pc);
                                                }
                                                Err(e) => {
                                                    eprintln!("[INFO] JIT: Failed to compile jump block with external jumps at address {}: {}", new_pc, e);
                                                    self.jit_failed_functions.insert(new_pc);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                        }
                    }

                    self.pc = new_pc;
                } else {
                    self.pc += 1;
                }

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, &instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::GetLocal(offset) => {
                let index = if offset < 0 {
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
                    self.bp + (offset as usize)
                };

                // Local variables are accessed relative to BP
                // No bounds checking needed as stack is pre-allocated
                // Parameter validation is done in caller
                let value = self
                    .get_stack_value(index)
                    .ok_or_else(|| format!("Invalid stack index: {}", index))?;
                self.push_value(value)?;
            }

            Instruction::SetLocal(offset) => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for SetLocal".to_string());
                }
                let value = self.pop_value()?;

                let index = if offset < 0 {
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
                    self.bp + (offset as usize)
                };

                // Extend stack if needed for positive offsets
                // Ensure stack has enough space - no dynamic resizing needed
                // Stack is pre-allocated with fixed capacity

                self.set_stack_value(index, value)?;
            }

            Instruction::GetGlobal(index) => {
                if index >= self.globals.len() {
                    return Err(format!("Global variable index out of bounds: {}", index));
                }
                self.push_value(self.globals[index].clone())?;
            }

            Instruction::SetGlobal(index) => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for SetGlobal".to_string());
                }
                let value = self.pop_value()?;
                // Extend globals vector if needed
                while self.globals.len() <= index {
                    self.globals.push(Value::Int(0)); // Default value
                }
                self.globals[index] = value;
            }

            Instruction::Call(func_name) => {
                return Err(format!(
                    "Call with function name '{}' should have been resolved to CallRel before execution",
                    func_name
                ));
            }

            Instruction::CallRel(offset) => {
                let new_pc = (self.pc as i32 + offset) as usize;

                // Record function call for profiling using the target address
                let call_name = format!("func_addr_{}", new_pc);
                self.profiler.record_function_call(call_name);

                // Handle JIT compilation - first increment call count and check if we should compile
                let should_jit_compile = if self.jit_compiler.is_some()
                    && !self.jit_disabled
                    && !self.jit_failed_functions.contains(&new_pc)
                {
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
                        match jit_compiler.compile_function(
                            new_pc,
                            &function_instructions,
                            self.print_jit_asm,
                        ) {
                            Ok(()) => {
                                jit_compilation_successful = true;
                                // Try to get function name for better logging
                                let func_name = self
                                    .jit_function_addresses
                                    .get(&new_pc)
                                    .map(|s| s.as_str())
                                    .unwrap_or("unknown");

                                eprintln!(
                                    "[INFO] JIT: Successfully compiled function '{}' at address {}",
                                    func_name, new_pc
                                );

                                if self.jit_function_addresses.contains_key(&new_pc) {
                                } else {
                                }
                            }
                            Err(e) => {
                                let func_name = self
                                    .jit_function_addresses
                                    .get(&new_pc)
                                    .map(|s| s.as_str())
                                    .unwrap_or("unknown");

                                eprintln!(
                                    "[INFO] JIT: Failed to compile function '{}' at address {}: {}",
                                    func_name, new_pc, e
                                );

                                // Mark this function as failed to prevent future compilation attempts
                                self.jit_failed_functions.insert(new_pc);
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

                            let is_function = self.jit_function_addresses.contains_key(&new_pc);

                            // Execute the JIT compiled function with error handling
                            // Get mutable pointers to VM state
                            let stack_ptr = self.stack.as_mut_ptr();
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
                                    if is_function {
                                        eprintln!("[INFO] JIT: Successfully executed function '{}' at address {}", func_name, new_pc);
                                    } else {
                                        eprintln!("[INFO] JIT: Successfully executed jump block at address {}", new_pc);
                                    }
                                }
                                Err(_) => {
                                    if is_function {
                                        eprintln!("[INFO] JIT: Failed to execute function '{}' at address {} - falling back to interpreter", func_name, new_pc);
                                    } else {
                                        eprintln!("[INFO] JIT: Failed to execute jump block at address {} - falling back to interpreter", new_pc);
                                    }
                                    // Fall through to interpreter execution
                                    self.pc = new_pc;
                                    self.print_debug_visualization(
                                        pc_before_execution,
                                        &instruction,
                                    );
                                    return Ok(ControlFlow::Continue);
                                }
                            }

                            self.pc += 1;

                            // Print debug visualization (heap and/or stack) if enabled
                            self.print_debug_visualization(pc_before_execution, &instruction);

                            // JIT execution completed, return immediately
                            return Ok(ControlFlow::Continue);
                        }
                    }
                    let func_name = self
                        .jit_function_addresses
                        .get(&new_pc)
                        .map(|s| s.as_str())
                        .unwrap_or("unknown");
                }

                // Fallback to interpreter execution
                eprintln!("[INFO] JIT: No JIT compilation available for address {} - falling back to interpreter", new_pc);
                self.pc = new_pc;

                // Print debug visualization (heap and/or stack) if enabled
                self.print_debug_visualization(pc_before_execution, &instruction);

                return Ok(ControlFlow::Continue);
            }

            Instruction::Ret => {
                let return_addr = if self.sp > 0 {
                    Some(self.pop_value()?)
                } else {
                    None
                };
                match return_addr {
                    Some(Value::Address(addr)) => {
                        self.pc = addr;
                    }
                    Some(Value::Int(n)) if n == -1 => {
                        let value = if self.sp > 0 {
                            Some(self.pop_value()?)
                        } else {
                            None
                        };
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
                self.push_value(Value::Address(self.bp))?;
            }

            Instruction::SetBP => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for SetBP".to_string());
                }
                let value = self.pop_value()?;
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
                self.push_value(Value::Address(self.sp))?;
            }

            Instruction::SetSP => {
                if self.sp == 0 {
                    return Err("Stack underflow for SetSP".to_string());
                }
                let value = self.pop_value()?;
                match value {
                    Value::Address(addr) => {
                        self.sp = addr;
                    }
                    Value::Int(n) => {
                        self.sp = n as usize;
                    }
                    _ => return Err("SetSP requires an address or number".to_string()),
                }
            }

            Instruction::GetHP => {
                self.push_value(Value::Address(self.hp))?;
            }

            Instruction::SetHP => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for SetHP".to_string());
                }
                let value = self.pop_value()?;
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
                self.push_value(Value::Address(self.pc))?;
            }

            Instruction::SetPC => {
                if self.is_stack_empty() {
                    return Err("Stack underflow for SetPC".to_string());
                }
                let value = self.pop_value()?;
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

            Instruction::BrkJit => {
                // JIT-only breakpoint: no-op in interpreter, breakpoint in JIT
                // This instruction is transparent to the interpreter
            }

            // (HeapAlloc removed - now handled via GetHP/SetHP)

            // (HeapGet removed - now handled via Load)

            // (HeapSet removed - now handled via Store)

            // (HeapGetOffset removed - now handled via AddressAdd + Load)

            // (HeapSetOffset removed - now handled via AddressAdd + Store)
            Instruction::Load => {
                // Stack: [heap_ref] -> [value]
                // Load value from heap at specified reference
                if self.is_stack_empty() {
                    return Err("Stack underflow for Load".to_string());
                }
                let heap_ref = self.pop_value()?;
                match heap_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            let error_msg = format!("Invalid heap index: {}", heap_index.0);
                            eprintln!("[ERROR] VM: Heap access out of bounds - {}", error_msg);
                            return Err(error_msg);
                        }
                        let value = &self.heap[heap_index.0];
                        self.push_value(value.clone())?;
                    }
                    Value::Address(addr) => {
                        if addr >= self.heap.len() {
                            return Err(format!("Invalid address: {}", addr));
                        }
                        let value = &self.heap[addr];
                        self.push_value(value.clone())?;
                    }
                    _ => return Err("Load requires a heap reference or address".to_string()),
                }
            }

            Instruction::Store => {
                // Stack: [value] [heap_ref] -> []
                // Store value to heap at specified reference
                if self.sp < 2 {
                    return Err("Stack underflow for Store".to_string());
                }
                let heap_ref = self.pop_value()?;
                let value = self.pop_value()?;
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
                if self.sp < 5 {
                    return Err("Stack underflow for Syscall".to_string());
                }

                // Pop the 4 arguments (length, buffer, fd, syscall_number) in reverse order
                let length = self.pop_value()?;
                let buffer_ref = self.pop_value()?;
                let fd = self.pop_value()?;
                let syscall_number = self.pop_value()?;

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
                                self.push_value(Value::Int(actual_length as i64))?;
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

                                self.push_value(Value::Int(actual_length as i64))?;
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
            // Need to clone the instruction again since it was moved by the match
            let instruction_for_profiling = self.program[pc_before_execution].clone();

            // Special handling for Call instructions first
            if let Instruction::Call(ref func_name) = instruction_for_profiling {
                self.profiler.record_function_call(func_name.clone());
            }

            let instruction_name = format!("{:?}", instruction_for_profiling)
                .split('(')
                .next()
                .unwrap_or("Unknown")
                .to_string();
            self.profiler.record_instruction(instruction_name, elapsed);
        }

        self.pc += 1;

        // Print debug visualization (heap and/or stack) if enabled
        let instruction_for_debug = self.program[pc_before_execution].clone();
        self.print_debug_visualization(pc_before_execution, &instruction_for_debug);

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
        if self.is_stack_empty() {
            Ok(0)
        } else {
            let value = self.pop_value()?;
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
        self.clear_stack();
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
        (0..self.sp)
            .map(|i| self.get_stack_value(i).unwrap())
            .collect()
    }

    /// Extract function instructions starting from the given address until function end
    fn extract_function_instructions(&self, start_addr: usize) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        let mut current_addr = start_addr;

        while current_addr < self.program.len() {
            let instruction = &self.program[current_addr];
            instructions.push(instruction.clone());

            // Stop at Ret instruction, but continue if we encounter jumps that might go beyond
            if matches!(instruction, Instruction::Ret) {
                // Look ahead for any jumps that target beyond the current position
                let mut max_target = current_addr;

                // Scan already collected instructions for jumps
                for (idx, inst) in instructions.iter().enumerate() {
                    match inst {
                        Instruction::JumpRel(offset) | Instruction::JumpIfZeroRel(offset) => {
                            let target = (current_addr as i32 + offset) as usize;
                            max_target = max_target.max(target);
                        }
                        _ => {}
                    }
                }

                // If no jumps go beyond current position, we can safely end here
                if max_target <= current_addr {
                    break;
                }
            }

            current_addr += 1;
        }

        instructions
    }

    /// Extract a jump block starting from target_addr and ending with a jump back to target_addr
    /// Returns (instructions, start_addr, end_addr) of the block, or None if no valid block found
    fn extract_jump_block(&self, target_addr: usize) -> Option<(Vec<Instruction>, usize, usize)> {
        if target_addr >= self.program.len() {
            return None;
        }

        let mut instructions = Vec::new();
        let mut current_addr = target_addr;
        let start_addr = target_addr;

        // Scan from target_addr looking for a jump back to target_addr
        while current_addr < self.program.len() {
            let instruction = &self.program[current_addr];
            instructions.push(instruction.clone());

            // Check if this is a jump back to our target address
            match instruction {
                Instruction::JumpRel(offset) => {
                    let jump_target = (current_addr as i32 + offset) as usize;
                    if jump_target == target_addr {
                        // Found the jump back to start - this completes the block
                        return Some((instructions, start_addr, current_addr));
                    }
                }
                Instruction::Ret => {
                    // If we hit a return, the block is incomplete
                    break;
                }
                _ => {}
            }

            current_addr += 1;

            // Safety check to avoid infinite loops
            if instructions.len() > 1000 {
                break;
            }
        }

        None
    }

    /// Extract a jump block with detailed external jump information
    /// Returns (instructions, start_addr, end_addr, external_jumps)
    fn extract_jump_block_with_exits(
        &self,
        target_addr: usize,
    ) -> Option<(Vec<Instruction>, usize, usize, Vec<ExternalJump>)> {
        if target_addr >= self.program.len() {
            return None;
        }

        let mut instructions = Vec::new();
        let mut current_addr = target_addr;
        let start_addr = target_addr;
        let mut end_addr = target_addr;

        // First pass: find the block boundaries
        while current_addr < self.program.len() {
            let instruction = &self.program[current_addr];
            instructions.push(instruction.clone());

            // Check if this is a jump back to our target address
            match instruction {
                Instruction::JumpRel(offset) => {
                    let jump_target = (current_addr as i32 + offset) as usize;
                    if jump_target == target_addr {
                        // Found the jump back to start - this completes the block
                        end_addr = current_addr;
                        break;
                    }
                }
                Instruction::Ret => {
                    // If we hit a return, the block is incomplete
                    return None;
                }
                _ => {}
            }

            current_addr += 1;

            // Safety check to avoid infinite loops
            if instructions.len() > 1000 {
                return None;
            }
        }

        // Second pass: identify external jumps now that we know the block boundaries
        let mut external_jumps = Vec::new();
        for (block_index, instruction) in instructions.iter().enumerate() {
            let instruction_addr = start_addr + block_index;

            match instruction {
                Instruction::JumpRel(offset) => {
                    let jump_target = (instruction_addr as i32 + offset) as usize;
                    // Skip the jump back to start (that's what defines our block)
                    if jump_target != target_addr {
                        external_jumps.push(ExternalJump {
                            block_index,
                            absolute_addr: instruction_addr,
                            target_addr: jump_target,
                            jump_type: ExternalJumpType::JumpRel(*offset),
                        });
                    }
                }
                Instruction::JumpIfZeroRel(offset) => {
                    let jump_target = (instruction_addr as i32 + offset) as usize;
                    // Check if this jumps outside the block boundaries
                    if jump_target < start_addr || jump_target > end_addr {
                        external_jumps.push(ExternalJump {
                            block_index,
                            absolute_addr: instruction_addr,
                            target_addr: jump_target,
                            jump_type: ExternalJumpType::JumpIfZeroRel(*offset),
                        });
                    }
                }
                Instruction::CallRel(offset) => {
                    let call_target = (instruction_addr as i32 + offset) as usize;
                    external_jumps.push(ExternalJump {
                        block_index,
                        absolute_addr: instruction_addr,
                        target_addr: call_target,
                        jump_type: ExternalJumpType::CallRel(*offset),
                    });
                }
                _ => {}
            }
        }

        Some((instructions, start_addr, end_addr, external_jumps))
    }

    /// Check if a block contains jumps to addresses outside the block range
    fn has_external_jumps(
        &self,
        block: &[Instruction],
        start_addr: usize,
        end_addr: usize,
    ) -> bool {
        for (idx, instruction) in block.iter().enumerate() {
            let current_addr = start_addr + idx;

            match instruction {
                Instruction::JumpRel(offset) => {
                    let jump_target = (current_addr as i32 + offset) as usize;
                    // Check if jump target is outside the block range
                    if jump_target < start_addr || jump_target > end_addr {
                        return true;
                    }
                }
                Instruction::JumpIfZeroRel(offset) => {
                    let jump_target = (current_addr as i32 + offset) as usize;
                    // Check if jump target is outside the block range
                    if jump_target < start_addr || jump_target > end_addr {
                        return true;
                    }
                }
                Instruction::CallRel(_) => {
                    // Function calls are considered external jumps
                    return true;
                }
                _ => {}
            }
        }

        false
    }

    /// Create fallback instructions for external jumps
    /// Returns (modified_block, fallback_instructions)
    fn create_jit_block_with_fallbacks(
        &self,
        mut block: Vec<Instruction>,
        external_jumps: Vec<ExternalJump>,
    ) -> (Vec<Instruction>, Vec<Instruction>) {
        let mut fallback_instructions = Vec::new();
        let original_block_size = block.len();

        // Create fallback instructions for each external jump
        for (fallback_index, external_jump) in external_jumps.iter().enumerate() {
            // Calculate offset from external jump position to fallback position
            let jump_position = external_jump.block_index;
            let fallback_position = original_block_size + fallback_index * 3; // Each fallback is 3 instructions
            let offset_to_fallback = (fallback_position as i32) - (jump_position as i32); // Jump is relative to current instruction

            // Replace the external jump with a jump to fallback
            match &external_jump.jump_type {
                ExternalJumpType::JumpIfZeroRel(_) => {
                    block[jump_position] = Instruction::JumpIfZeroRel(offset_to_fallback);
                }
                ExternalJumpType::JumpRel(_) => {
                    block[jump_position] = Instruction::JumpRel(offset_to_fallback);
                }
                ExternalJumpType::CallRel(_) => {
                    // For function calls, we still need to handle them specially
                    // For now, redirect to fallback
                    block[jump_position] = Instruction::JumpRel(offset_to_fallback);
                }
            }

            // Create fallback instructions: set_pc + ret
            fallback_instructions.push(Instruction::Push(external_jump.target_addr as i64));
            fallback_instructions.push(Instruction::SetPC);
            fallback_instructions.push(Instruction::Ret);
        }

        (block, fallback_instructions)
    }

    /// Prepare a jump block for JIT compilation by handling external jumps
    fn prepare_jit_block(&self, target_addr: usize) -> Option<Vec<Instruction>> {
        if let Some((block, _start_addr, _end_addr, external_jumps)) =
            self.extract_jump_block_with_exits(target_addr)
        {
            let (modified_block, fallback_instructions) =
                self.create_jit_block_with_fallbacks(block, external_jumps);

            // Combine modified block with fallback instructions
            let mut jit_block = modified_block;
            jit_block.extend(fallback_instructions);

            Some(jit_block)
        } else {
            None
        }
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
            // Only show stack contents from index 0 to sp
            let visible_stack: Vec<String> = (0..self.sp)
                .filter_map(|i| self.get_stack_value(i))
                .map(|x| x.to_string())
                .collect();
            print!(" [{}]", visible_stack.join(", "));
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

    /// Disable JIT compilation
    pub fn disable_jit(&mut self) {
        self.vm.disable_jit();
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

    /// Set JIT assembly printing option
    pub fn set_print_jit_asm(&mut self, print_jit_asm: bool) {
        self.vm.set_print_jit_asm(print_jit_asm);
    }

    /// Set JIT compile output file
    pub fn set_jit_compile_output(&mut self, output_file: Option<String>) {
        self.vm.set_jit_compile_output(output_file);
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
            Instruction::GetHP,      // [current_hp]
            Instruction::Push(3),    // [current_hp, 3]
            Instruction::AddressAdd, // [new_hp]
            Instruction::SetHP,      // [] (HP = new_hp, heap extended)
        ]);

        // Execute GetHP
        vm.step().unwrap();
        println!("After GetHP: {:?}", vm.get_stack());

        // Execute Push(3)
        vm.step().unwrap();
        println!("After Push(3): {:?}", vm.get_stack());

        // Execute the rest
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        // Check that heap has been allocated
        assert_eq!(vm.hp, 3);
        // Stack should be empty after SetHP
        assert_eq!(vm.sp, 0);
    }

    #[test]
    fn test_heap_get_set() {
        let mut vm = VM::new();

        // Test Load/Store with allocated memory - use simple approach
        vm.load_program(vec![
            // Allocate heap space by advancing HP
            Instruction::GetHP,      // [current_hp = 0]
            Instruction::Push(2),    // [current_hp, 2]
            Instruction::AddressAdd, // [new_hp = 2]
            Instruction::SetHP,      // [] (HP = 2, heap extended)
            // Set value at address 0: Store 42 at heap[0]
            Instruction::Push(42),       // [42]
            Instruction::PushAddress(0), // [42, addr(0)]
            Instruction::Store,          // [] (store 42 at heap[0])
            // Get value from address 0: Load from heap[0]
            Instruction::PushAddress(0), // [addr(0)]
            Instruction::Load,           // [42] (load from heap[0])
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        let stack = vm.get_stack();
        // Stack should contain: [retrieved_value]
        assert_eq!(vm.sp, 1);

        // Check that we got the value back
        if let Value::Int(value) = &stack[0] {
            assert_eq!(*value, 42);
        } else {
            panic!("Expected Int(42), got {:?}", stack[0]);
        }
    }

    #[test]
    fn test_load_store_operations() {
        let mut vm = VM::new_with_stack_printing(true);

        // Test Load/Store with allocated memory using GetHP/SetHP
        vm.load_program(vec![
            // Allocate heap space by advancing HP (same pattern as test_heap_memory_allocation)
            Instruction::GetHP,      // [current_hp = 0]
            Instruction::Push(3),    // [current_hp, 3]
            Instruction::AddressAdd, // [new_hp = 3]
            Instruction::SetHP,      // [] (HP = 3, heap extended)
            // Store value 42 at heap_start_addr
            Instruction::Push(42),       // [42]
            Instruction::PushAddress(0), // [42, heap_start_addr]
            Instruction::Store,          // [] (store 42 at heap_start_addr)
            // Load value from heap_start_addr
            Instruction::PushAddress(0), // [heap_start_addr]
            Instruction::Load,           // [42]
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        // Check final result
        assert_eq!(vm.sp, 1); // loaded value only
        match if vm.sp > 0 {
            Some(ValueEncoder::decode(vm.stack[vm.sp - 1]))
        } else {
            None
        } {
            Some(value) => {
                println!("Actual value on stack top: {:?}", value);
                if let Value::Int(n) = value {
                    assert_eq!(n, 42);
                } else {
                    panic!("Expected Int(42), got {:?}", value);
                }
            }
            None => panic!("Stack is empty, expected Int(42)"),
        }
    }

    #[test]
    fn test_heap_offset_operations() {
        let mut vm = VM::new();

        // Test AddressAdd + Load/Store (replacement for HeapGetOffset/HeapSetOffset)
        vm.load_program(vec![
            // Allocate heap space by advancing HP (same pattern as test_heap_memory_allocation)
            Instruction::GetHP,      // [current_hp = 0]
            Instruction::Push(3),    // [current_hp, 3]
            Instruction::AddressAdd, // [new_hp = 3]
            Instruction::SetHP,      // [] (HP = 3, heap extended)
            // Set value at offset 1: Store 100 at heap_start_addr + 1
            Instruction::Push(100),      // [100]
            Instruction::PushAddress(0), // [100, heap_start_addr]
            Instruction::Push(1),        // [100, heap_start_addr, 1]
            Instruction::AddressAdd,     // [100, target_addr]
            Instruction::Store,          // [] (store 100 at target_addr)
            // Get value from offset 1: Load from heap_start_addr + 1
            Instruction::PushAddress(0), // [heap_start_addr]
            Instruction::Push(1),        // [heap_start_addr, 1]
            Instruction::AddressAdd,     // [target_addr]
            Instruction::Load,           // [100]
        ]);

        // Execute step by step
        while vm.pc < vm.program.len() {
            vm.step().unwrap();
        }

        let stack = vm.get_stack();
        // Stack should contain: [retrieved_value]
        assert_eq!(vm.sp, 1);

        // Check that we got the value back
        if let Value::Int(value) = &stack[0] {
            assert_eq!(*value, 100);
        } else {
            panic!("Expected Int(100), got {:?}", stack[0]);
        }
    }

    #[test]
    #[cfg(target_os = "macos")]
    fn test_jit_failed_compilation_tracking() {
        let mut vm = VM::new();

        // Initially no failed functions
        assert!(vm.jit_failed_functions.is_empty());

        // Simulate adding a failed function
        vm.jit_failed_functions.insert(100);

        // Check that the function is marked as failed
        assert!(vm.jit_failed_functions.contains(&100));
        assert_eq!(vm.jit_failed_functions.len(), 1);

        // Other functions should not be affected
        assert!(!vm.jit_failed_functions.contains(&200));
    }
}
