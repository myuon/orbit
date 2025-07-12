use crate::codegen::CodeGenerator;
use crate::profiler::InstructionTimer;
use crate::vm::Instruction;
use crate::{ast::Program, profiler::Profiler};
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
    Int(i64),
    Boolean(bool),
    Address(usize),
    HeapRef(HeapIndex),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
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

pub enum ControlFlow {
    Exit(i64),
    Continue,
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    pc: usize, // program counter
    bp: usize, // base pointer for stack frame
    sp: usize, // stack pointer
    program: Vec<Instruction>,
    pub print_stacks: bool, // whether to print stack state during execution
    print_stacks_on_call: Option<String>, // print stacks only when calling this function
    heap: Vec<HeapObject>,  // unified heap storage
    globals: HashMap<String, Value>, // global variables
    // Output capture for testing
    pub captured_output: Option<String>,
    // Profiling
    pub profiler: Profiler,
}

impl VM {
    pub fn new() -> Self {
        Self::with_options(false, false)
    }

    pub fn new_with_stack_printing(print_stacks: bool) -> Self {
        Self::with_options(print_stacks, false)
    }

    pub fn new_with_profiling(enable_profiling: bool) -> Self {
        Self::with_options(false, enable_profiling)
    }

    pub fn with_options(print_stacks: bool, enable_profiling: bool) -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            program: Vec::new(),
            print_stacks,
            print_stacks_on_call: None,
            heap: Vec::new(),
            globals: HashMap::new(),
            captured_output: None,
            profiler: Profiler::new_with_enabled(enable_profiling),
        }
    }

    pub fn with_all_options(
        print_stacks: bool,
        print_stacks_on_call: Option<String>,
        enable_profiling: bool,
    ) -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            program: Vec::new(),
            print_stacks,
            print_stacks_on_call,
            heap: Vec::new(),
            globals: HashMap::new(),
            captured_output: None,
            profiler: Profiler::new_with_enabled(enable_profiling),
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.pc = 0;
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
                // Allocate string on heap and push heap reference
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::String(s.clone()));
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::PushHeapRef(index) => {
                if *index >= self.heap.len() {
                    return Err(format!("Invalid heap index: {}", index));
                }
                self.stack.push(Value::HeapRef(HeapIndex(*index)));
            }

            Instruction::Pop => {
                if self.stack.is_empty() {
                    return Err("Stack underflow".to_string());
                }
                self.stack.pop();
            }

            Instruction::Add => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for Add".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a + b));
                    }
                    _ => return Err("Add operation requires numbers".to_string()),
                }
            }

            Instruction::Sub => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for Sub".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a - b));
                    }
                    _ => return Err("Subtract operation requires numbers".to_string()),
                }
            }

            Instruction::Mul => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for Mul".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a * b));
                    }
                    _ => return Err("Multiply operation requires numbers".to_string()),
                }
            }

            Instruction::Div => {
                if self.stack.len() < 2 {
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
                    _ => return Err("Divide operation requires numbers".to_string()),
                }
            }

            Instruction::Mod => {
                if self.stack.len() < 2 {
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
                if self.stack.len() < 2 {
                    return Err("Stack underflow for AddressAdd".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        self.stack.push(Value::Address(addr + offset as usize));
                    }
                    _ => return Err("AddressAdd requires Address + Number".to_string()),
                }
            }

            Instruction::AddressSub => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for AddressSub".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Address(addr), Value::Int(offset)) => {
                        self.stack.push(Value::Address(addr - offset as usize));
                    }
                    _ => return Err("AddressSub requires Address - Number".to_string()),
                }
            }

            Instruction::Eq => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for Eq".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(Value::Boolean(a == b));
            }

            Instruction::Lt => {
                if self.stack.len() < 2 {
                    return Err("Stack underflow for Lt".to_string());
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Boolean(a < b));
                    }
                    _ => return Err("Less than operation requires numbers".to_string()),
                }
            }

            Instruction::Lte => {
                if self.stack.len() < 2 {
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
                if self.stack.len() < 2 {
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
                if self.stack.len() < 2 {
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
                    _ => return Err("Not operation requires boolean or number".to_string()),
                }
            }

            Instruction::Jump(addr) => {
                self.pc = *addr;

                // Print instruction and stack state after execution if enabled
                if self.print_stacks {
                    println!(
                        "{:04} {:20} [{}]",
                        pc_before_execution,
                        format!("{}", instruction),
                        self.stack
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }

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
                    _ => false,
                };
                if should_jump {
                    self.pc = *addr;
                } else {
                    self.pc += 1;
                }

                // Print instruction and stack state after execution if enabled
                if self.print_stacks {
                    println!(
                        "{:04} {:20} [{}]",
                        pc_before_execution,
                        format!("{}", instruction),
                        self.stack
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }

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
                self.stack.push(self.stack[index].clone());
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

                self.stack[index] = value;
            }

            Instruction::GetGlobal(name) => match self.globals.get(name) {
                Some(value) => self.stack.push(value.clone()),
                None => return Err(format!("Undefined global variable: {}", name)),
            },

            Instruction::SetGlobal(name) => {
                if self.stack.is_empty() {
                    return Err("Stack underflow for SetGlobal".to_string());
                }
                let value = self.stack.pop().unwrap();
                self.globals.insert(name.clone(), value);
            }

            Instruction::Call(func_name) => {
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

                // Call instruction now only performs jump to function
                // All other operations (PC/BP setup) are handled by compiler-generated instructions
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
                    self.pc = addr;

                    // Print instruction and stack state after execution if enabled
                    if self.print_stacks {
                        println!(
                            "{:04} {:20} [{}]",
                            pc_before_execution,
                            format!("{}", instruction),
                            self.stack
                                .iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                    }

                    return Ok(ControlFlow::Continue);
                } else {
                    return Err(format!("Function not found: {}", func_name));
                }
            }

            Instruction::Ret => {
                let return_addr = self.stack.pop();
                match return_addr {
                    Some(Value::Address(addr)) => {
                        self.pc = addr;
                    }
                    Some(Value::Int(n)) if n == -1 => {
                        let value = self.stack.pop();
                        if let Some(Value::Int(n)) = value {
                            return Ok(ControlFlow::Exit(n));
                        } else {
                            return Err("Stack underflow for Ret".to_string());
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

            Instruction::HeapAlloc => {
                // Allocate a new heap object (type determined by stack content)
                // For now, this is a placeholder - specific allocation is handled by typed instructions
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::String(String::new())); // Default placeholder
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::HeapGet => {
                // Get value from heap object - implementation depends on object type
                if self.stack.is_empty() {
                    return Err("Stack underflow for HeapGet".to_string());
                }
                let heap_ref = self.stack.pop().unwrap();
                match heap_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        // For now, just push the heap ref back - specific get operations handled by typed instructions
                        self.stack.push(Value::HeapRef(heap_index));
                    }
                    _ => return Err("HeapGet requires a heap reference".to_string()),
                }
            }

            Instruction::HeapSet => {
                // Set value in heap object - implementation depends on object type
                if self.stack.len() < 2 {
                    return Err("Stack underflow for HeapSet".to_string());
                }
                let heap_ref = self.stack.pop().unwrap();
                let _value = self.stack.pop().unwrap();
                match heap_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        // For now, just discard - specific set operations handled by typed instructions
                    }
                    _ => return Err("HeapSet requires a heap reference".to_string()),
                }
            }

            Instruction::StringNew => {
                // Create a new string from stack value
                if self.stack.is_empty() {
                    return Err("Stack underflow for StringNew".to_string());
                }
                let value = self.stack.pop().unwrap();
                let string_value = match value {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &self.heap[heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => return Err("StringNew requires a string heap object".to_string()),
                        }
                    }
                    _ => return Err("StringNew requires a heap reference".to_string()),
                };
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::String(string_value));
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::VectorNew => {
                // Create a new empty vector and push its heap index
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::Vector(Vec::new()));
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::VectorPush => {
                // Stack: [value] [vector_heap_ref]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for VectorPush".to_string());
                }
                let vector_ref = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();
                match vector_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &mut self.heap[heap_index.0] {
                            HeapObject::Vector(vec) => {
                                vec.push(value);
                            }
                            _ => return Err("VectorPush requires a vector heap object".to_string()),
                        }
                    }
                    _ => return Err("VectorPush requires a heap reference".to_string()),
                }
            }

            Instruction::VectorIndex => {
                // Stack: [vector_heap_ref] [element_index]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for VectorIndex".to_string());
                }
                let element_index_value = self.stack.pop().unwrap();
                let vector_ref = self.stack.pop().unwrap();
                match (vector_ref, element_index_value) {
                    (Value::HeapRef(heap_index), Value::Int(element_index)) => {
                        let element_index = element_index as usize;
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &self.heap[heap_index.0] {
                            HeapObject::Vector(vec) => {
                                if element_index >= vec.len() {
                                    return Err(format!(
                                        "Invalid element index: {}",
                                        element_index
                                    ));
                                }
                                let value = vec[element_index].clone();
                                self.stack.push(value);
                            }
                            _ => {
                                return Err("VectorIndex requires a vector heap object".to_string())
                            }
                        }
                    }
                    _ => {
                        return Err(
                            "VectorIndex requires heap reference and element index (number)"
                                .to_string(),
                        )
                    }
                }
            }

            Instruction::VectorSet => {
                // Stack: [value] [element_index] [vector_heap_ref]
                if self.stack.len() < 3 {
                    return Err("Stack underflow for VectorSet".to_string());
                }
                let vector_ref = self.stack.pop().unwrap();
                let element_index_value = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();
                match (vector_ref, element_index_value) {
                    (Value::HeapRef(heap_index), Value::Int(element_index)) => {
                        let element_index = element_index as usize;
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &mut self.heap[heap_index.0] {
                            HeapObject::Vector(vec) => {
                                if element_index >= vec.len() {
                                    return Err(format!(
                                        "Invalid element index: {}",
                                        element_index
                                    ));
                                }
                                vec[element_index] = value;
                            }
                            _ => return Err("VectorSet requires a vector heap object".to_string()),
                        }
                    }
                    _ => {
                        return Err(
                            "VectorSet requires heap reference and element index (number)"
                                .to_string(),
                        )
                    }
                }
            }

            Instruction::MapNew => {
                // Create a new empty map and push its heap index
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::Map(HashMap::new()));
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::MapIndex => {
                // Stack: [map_heap_ref] [key]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for MapIndex".to_string());
                }
                let key_value = self.stack.pop().unwrap();
                let map_ref = self.stack.pop().unwrap();
                match (map_ref, key_value) {
                    (Value::HeapRef(heap_index), Value::HeapRef(key_heap_index)) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        if key_heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid key heap index: {}", key_heap_index.0));
                        }

                        let key = match &self.heap[key_heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => return Err("MapIndex requires a string key".to_string()),
                        };

                        match &self.heap[heap_index.0] {
                            HeapObject::Map(map) => match map.get(&key) {
                                Some(value) => {
                                    self.stack.push(value.clone());
                                }
                                None => {
                                    return Err(format!("Key '{}' not found in map", key));
                                }
                            },
                            _ => return Err("MapIndex requires a map heap object".to_string()),
                        }
                    }
                    _ => {
                        return Err(
                            "MapIndex requires a heap reference and key (string)".to_string()
                        )
                    }
                }
            }

            Instruction::MapSet => {
                // Stack: [value] [key] [map_heap_ref]
                if self.stack.len() < 3 {
                    return Err("Stack underflow for MapSet".to_string());
                }
                let map_ref = self.stack.pop().unwrap();
                let key_value = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();
                match (map_ref, key_value) {
                    (Value::HeapRef(heap_index), Value::HeapRef(key_heap_index)) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        if key_heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid key heap index: {}", key_heap_index.0));
                        }

                        let key = match &self.heap[key_heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => return Err("MapSet requires a string key".to_string()),
                        };

                        match &mut self.heap[heap_index.0] {
                            HeapObject::Map(map) => {
                                map.insert(key, value);
                            }
                            _ => return Err("MapSet requires a map heap object".to_string()),
                        }
                    }
                    _ => {
                        return Err("MapSet requires a heap reference and key (string)".to_string())
                    }
                }
            }

            Instruction::PointerAlloc => {
                // Stack: [element_type_string] [size]
                // Allocate pointer with specified type and size
                if self.stack.len() < 2 {
                    return Err("Stack underflow for PointerAlloc".to_string());
                }

                let element_type_ref = self.stack.pop().unwrap();
                let size = match self.stack.pop().unwrap() {
                    Value::Int(n) => n as usize,
                    _ => return Err("PointerAlloc requires a number for size".to_string()),
                };

                // Extract element type name from heap string
                let element_type =
                    match element_type_ref {
                        Value::HeapRef(heap_index) => {
                            if heap_index.0 >= self.heap.len() {
                                return Err(format!("Invalid heap index: {}", heap_index.0));
                            }
                            match &self.heap[heap_index.0] {
                                HeapObject::String(s) => s.clone(),
                                _ => {
                                    return Err("PointerAlloc requires a string for element type"
                                        .to_string())
                                }
                            }
                        }
                        _ => {
                            return Err("PointerAlloc requires a heap reference for element type"
                                .to_string())
                        }
                    };

                // Calculate sizeof(element_type) * size
                let element_size = self.sizeof_type(&element_type);
                let total_size = element_size * size;

                // Create pointer with total_size bytes, initialized to zero
                let values = vec![Value::Int(0); total_size];
                let heap_index = self.heap.len();
                self.heap.push(HeapObject::Pointer(values));
                self.stack.push(Value::HeapRef(HeapIndex(heap_index)));
            }

            Instruction::PointerIndex => {
                // Stack: [pointer_heap_ref] [element_index]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for PointerIndex".to_string());
                }
                let element_index_value = self.stack.pop().unwrap();
                let pointer_ref = self.stack.pop().unwrap();
                match (pointer_ref, element_index_value) {
                    (Value::HeapRef(heap_index), Value::Int(element_index)) => {
                        let element_index = element_index as usize;
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &self.heap[heap_index.0] {
                            HeapObject::Pointer(arr) => {
                                if element_index >= arr.len() {
                                    return Err(format!(
                                        "Pointer index out of bounds: {} >= {}",
                                        element_index,
                                        arr.len()
                                    ));
                                }
                                self.stack.push(arr[element_index].clone());
                            }
                            _ => {
                                return Err(
                                    "PointerIndex requires a pointer heap object".to_string()
                                )
                            }
                        }
                    }
                    _ => {
                        return Err("PointerIndex requires a heap reference and number".to_string())
                    }
                }
            }

            Instruction::PointerSet => {
                // Stack: [value] [element_index] [pointer_heap_ref]
                if self.stack.len() < 3 {
                    return Err("Stack underflow for PointerSet".to_string());
                }
                let pointer_ref = self.stack.pop().unwrap();
                let element_index_value = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();
                match (pointer_ref, element_index_value) {
                    (Value::HeapRef(heap_index), Value::Int(element_index)) => {
                        let element_index = element_index as usize;
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &mut self.heap[heap_index.0] {
                            HeapObject::Pointer(arr) => {
                                if element_index >= arr.len() {
                                    // Extend the array if needed
                                    arr.resize(element_index + 1, Value::Int(0));
                                }
                                arr[element_index] = value;
                            }
                            _ => {
                                return Err("PointerSet requires a pointer heap object".to_string())
                            }
                        }
                    }
                    _ => return Err("PointerSet requires a heap reference and number".to_string()),
                }
            }

            Instruction::StringIndex => {
                // Stack: [string_heap_ref] [element_index]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for StringIndex".to_string());
                }
                let element_index_value = self.stack.pop().unwrap();
                let string_ref = self.stack.pop().unwrap();
                match (string_ref, element_index_value) {
                    (Value::HeapRef(heap_index), Value::Int(element_index)) => {
                        let element_index = element_index as usize;
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &self.heap[heap_index.0] {
                            HeapObject::String(s) => {
                                let bytes = s.as_bytes();
                                if element_index >= bytes.len() {
                                    return Err(format!(
                                        "String index out of bounds: {} >= {}",
                                        element_index,
                                        bytes.len()
                                    ));
                                }
                                // Return the byte value as a number
                                self.stack.push(Value::Int(bytes[element_index] as i64));
                            }
                            _ => {
                                return Err("StringIndex requires a string heap object".to_string())
                            }
                        }
                    }
                    _ => return Err("StringIndex requires a heap reference and number".to_string()),
                }
            }
            Instruction::StructNew => {
                // Stack: [field_count] [field_name_1] [field_value_1] ... [field_name_n] [field_value_n]
                if self.stack.is_empty() {
                    return Err("Stack underflow for StructNew".to_string());
                }
                let field_count = match self.stack.pop().unwrap() {
                    Value::Int(n) => n as usize,
                    _ => return Err("StructNew requires a number for field count".to_string()),
                };

                if self.stack.len() < field_count * 2 {
                    return Err("Stack underflow for StructNew fields".to_string());
                }

                let mut fields = HashMap::new();
                for _ in 0..field_count {
                    let field_value = self.stack.pop().unwrap();
                    let field_name = match self.stack.pop().unwrap() {
                        Value::HeapRef(heap_index) => {
                            if heap_index.0 >= self.heap.len() {
                                return Err(format!("Invalid heap index: {}", heap_index.0));
                            }
                            match &self.heap[heap_index.0] {
                                HeapObject::String(s) => s.clone(),
                                _ => {
                                    return Err("StructNew requires string field names".to_string())
                                }
                            }
                        }
                        _ => {
                            return Err(
                                "StructNew requires heap reference for field names".to_string()
                            )
                        }
                    };
                    fields.insert(field_name, field_value);
                }

                let heap_index = HeapIndex(self.heap.len());
                self.heap.push(HeapObject::Struct(fields));
                self.stack.push(Value::HeapRef(heap_index));
            }
            Instruction::StructFieldGet => {
                // Stack: [struct_heap_ref] [field_name]
                if self.stack.len() < 2 {
                    return Err("Stack underflow for StructFieldGet".to_string());
                }
                let field_name_ref = self.stack.pop().unwrap();
                let struct_ref = self.stack.pop().unwrap();

                match (struct_ref, field_name_ref) {
                    (Value::HeapRef(heap_index), Value::HeapRef(name_heap_index)) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        if name_heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid name heap index: {}", name_heap_index.0));
                        }

                        let field_name = match &self.heap[name_heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => {
                                return Err(
                                    "StructFieldGet requires a string field name".to_string()
                                )
                            }
                        };

                        match &self.heap[heap_index.0] {
                            HeapObject::Struct(fields) => match fields.get(&field_name) {
                                Some(value) => self.stack.push(value.clone()),
                                None => {
                                    return Err(format!(
                                        "Field '{}' not found in struct",
                                        field_name
                                    ))
                                }
                            },
                            _ => {
                                return Err(
                                    "StructFieldGet requires a struct heap object".to_string()
                                )
                            }
                        }
                    }
                    _ => return Err("StructFieldGet requires heap references".to_string()),
                }
            }
            Instruction::StructFieldSet => {
                // Stack: [value] [field_name] [struct_heap_ref]
                if self.stack.len() < 3 {
                    return Err("Stack underflow for StructFieldSet".to_string());
                }
                let struct_ref = self.stack.pop().unwrap();
                let field_name_ref = self.stack.pop().unwrap();
                let value = self.stack.pop().unwrap();

                match (struct_ref, field_name_ref) {
                    (Value::HeapRef(heap_index), Value::HeapRef(name_heap_index)) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        if name_heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid name heap index: {}", name_heap_index.0));
                        }

                        let field_name = match &self.heap[name_heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => {
                                return Err(
                                    "StructFieldSet requires a string field name".to_string()
                                )
                            }
                        };

                        match &mut self.heap[heap_index.0] {
                            HeapObject::Struct(fields) => {
                                fields.insert(field_name, value);
                            }
                            _ => {
                                return Err(
                                    "StructFieldSet requires a struct heap object".to_string()
                                )
                            }
                        }
                    }
                    _ => return Err("StructFieldSet requires heap references".to_string()),
                }
            }

            Instruction::Syscall => {
                // Stack: [return_placeholder] [syscall_number] [fd] [buffer] [length] (pushed left-to-right)
                // For syscall(1): write(fd, buffer, length)
                if self.stack.len() < 5 {
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

                        // Get buffer content
                        match buffer_ref {
                            Value::HeapRef(heap_index) => {
                                if heap_index.0 >= self.heap.len() {
                                    return Err(format!("Invalid heap index: {}", heap_index.0));
                                }
                                match &self.heap[heap_index.0] {
                                    HeapObject::String(s) => {
                                        // Use the specified length or the string length, whichever is smaller
                                        let actual_length = length_num.min(s.len());
                                        let output = &s[..actual_length];

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
                                    _ => {
                                        return Err(
                                            "Write syscall: buffer must be a string".to_string()
                                        )
                                    }
                                }
                            }
                            _ => {
                                return Err(
                                    "Write syscall: buffer must be a string reference".to_string()
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

        // Print instruction and stack state after execution if enabled
        if self.print_stacks {
            println!(
                "{:04} {:20} [{}]",
                pc_before_execution,
                format!("{}", instruction),
                self.stack
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

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

    /// Get a reference to the stack for debugging
    pub fn get_stack(&self) -> &Vec<Value> {
        &self.stack
    }

    /// Calculate the size in bytes for a given type
    fn sizeof_type(&self, type_name: &str) -> usize {
        match type_name {
            "bool" | "boolean" => 1,
            "byte" => 1,
            "int" | "number" => 8, // Using 8 bytes for numbers
            "string" => 8,         // String is a pointer, so 8 bytes
            _ => {
                if type_name.starts_with("[*]") {
                    8 // Pointer types are 8 bytes
                } else {
                    // For struct types or unknown types, default to 8 bytes
                    // In a full implementation, this would look up the struct size
                    8
                }
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
        let mut compiler = CodeGenerator::new();
        let instructions = compiler.compile_program(program);

        // Execute on VM
        self.vm.reset();
        self.vm.load_program(instructions);

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

        // Execute on VM with stack printing option
        self.vm.print_stacks = print_stacks;
        self.vm.reset();
        self.vm.load_program(instructions);

        match self.vm.execute() {
            Ok(result) => Ok(Some(Value::Int(result))),
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

    /// Enable output capture for testing
    pub fn enable_output_capture(&mut self) {
        self.vm.enable_output_capture();
    }

    /// Get captured output and clear the buffer
    pub fn take_captured_output(&mut self) -> Option<String> {
        self.vm.take_captured_output()
    }
}
