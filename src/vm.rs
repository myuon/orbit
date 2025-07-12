use crate::ast::{
    BinaryOp, Decl, Expr, Function, GlobalVariable, IndexContainerType, Program, Stmt, StructDecl,
};
use crate::profiler::{InstructionTimer, Profiler};
use crate::runtime::{HeapIndex, HeapObject, Value};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Stack operations
    Push(i64),
    PushString(String), // Only used for global initialization
    PushHeapRef(usize), // Push heap reference by index
    Pop,

    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Address arithmetic operations
    AddressAdd, // Address + Number -> Address
    AddressSub, // Address - Number -> Address

    // Comparison operations
    Eq,
    Lt,
    Lte,
    Gt,
    Gte,

    // Logical operations
    Not,

    // Control flow
    Jump(usize),
    JumpIfZero(usize),

    // Local variables
    GetLocal(i32),
    SetLocal(i32),

    // Global variables
    GetGlobal(String),
    SetGlobal(String),

    // Function calls
    Call(String),
    Ret,

    // Frame management
    GetBP,
    SetBP,
    GetSP,
    SetSP,
    GetPC,
    SetPC,

    // Labels
    Label(String),

    // No operation
    Nop,

    // Heap operations
    HeapAlloc, // Allocate new heap object
    HeapGet,   // Get value from heap object
    HeapSet,   // Set value in heap object

    // String operations
    StringNew,

    // Vector operations
    VectorNew,
    VectorPush,
    VectorIndex,
    VectorSet,

    // Map operations
    MapNew,
    MapIndex,
    MapSet,

    // Pointer operations
    PointerAlloc,
    PointerIndex,
    PointerSet,

    // String operations
    StringIndex,

    // Struct operations
    StructNew,
    StructFieldGet,
    StructFieldSet,
    MethodCall(usize), // Method call with argument count

    // System calls
    Syscall,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Push(value) => write!(f, "push {}", value),
            Instruction::PushString(s) => write!(f, "push_string \"{}\"", s),
            Instruction::PushHeapRef(index) => write!(f, "push_heap_ref {}", index),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Add => write!(f, "add"),
            Instruction::Sub => write!(f, "sub"),
            Instruction::AddressAdd => write!(f, "address_add"),
            Instruction::AddressSub => write!(f, "address_sub"),
            Instruction::Mul => write!(f, "mul"),
            Instruction::Div => write!(f, "div"),
            Instruction::Mod => write!(f, "mod"),
            Instruction::Eq => write!(f, "eq"),
            Instruction::Lt => write!(f, "lt"),
            Instruction::Lte => write!(f, "lte"),
            Instruction::Gt => write!(f, "gt"),
            Instruction::Gte => write!(f, "gte"),
            Instruction::Not => write!(f, "not"),
            Instruction::Jump(addr) => write!(f, "jump {}", addr),
            Instruction::JumpIfZero(addr) => write!(f, "jump_if_zero {}", addr),
            Instruction::GetLocal(offset) => write!(f, "get_local {}", offset),
            Instruction::SetLocal(offset) => write!(f, "set_local {}", offset),
            Instruction::GetGlobal(name) => write!(f, "get_global {}", name),
            Instruction::SetGlobal(name) => write!(f, "set_global {}", name),
            Instruction::Call(func_name) => write!(f, "call {}", func_name),
            Instruction::Ret => write!(f, "ret"),
            Instruction::GetBP => write!(f, "get_bp"),
            Instruction::SetBP => write!(f, "set_bp"),
            Instruction::GetSP => write!(f, "get_sp"),
            Instruction::SetSP => write!(f, "set_sp"),
            Instruction::GetPC => write!(f, "get_pc"),
            Instruction::SetPC => write!(f, "set_pc"),
            Instruction::Label(name) => write!(f, "{}:", name),
            Instruction::Nop => write!(f, "nop"),
            Instruction::HeapAlloc => write!(f, "heap_alloc"),
            Instruction::HeapGet => write!(f, "heap_get"),
            Instruction::HeapSet => write!(f, "heap_set"),
            Instruction::StringNew => write!(f, "string_new"),
            Instruction::VectorNew => write!(f, "vector_new"),
            Instruction::VectorPush => write!(f, "vector_push"),
            Instruction::VectorIndex => write!(f, "vector_index"),
            Instruction::VectorSet => write!(f, "vector_set"),
            Instruction::MapNew => write!(f, "map_new"),
            Instruction::MapIndex => write!(f, "map_index"),
            Instruction::MapSet => write!(f, "map_set"),
            Instruction::PointerAlloc => write!(f, "pointer_new"),
            Instruction::PointerIndex => write!(f, "pointer_index"),
            Instruction::PointerSet => write!(f, "pointer_set"),
            Instruction::StringIndex => write!(f, "string_index"),
            Instruction::StructNew => write!(f, "struct_new"),
            Instruction::StructFieldGet => write!(f, "struct_field_get"),
            Instruction::StructFieldSet => write!(f, "struct_field_set"),
            Instruction::MethodCall(argc) => write!(f, "method_call {}", argc),
            Instruction::Syscall => write!(f, "syscall"),
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
                    return Ok(ControlFlow::Continue);
                } else {
                    return Err(format!("Function not found: {}", func_name));
                }
            }

            Instruction::Ret => {
                // Determine the return type based on current stack state and BP

                // For main function: BP should be at initial value and stack should be simple
                if self.bp <= 3 {
                    // Initial BP setup makes BP around 3
                    // Main function return
                    if !self.stack.is_empty() {
                        let return_value = self.stack.pop().unwrap();
                        return match return_value {
                            Value::Int(v) => Ok(ControlFlow::Exit(v as i64)),
                            _ => Err("Program must return a number".to_string()),
                        };
                    } else {
                        return Err("Stack underflow for main function return".to_string());
                    }
                } else {
                    // Regular function return - need to do full stack frame restoration
                    // Stack currently has: [...frame...] [return_value]
                    // BP points to: [old_bp]
                    // BP-1 points to: [return_addr]
                    // BP-2 points to: [last_arg]
                    // ...
                    // We need to restore stack to put return_value in place of the placeholder

                    if self.stack.is_empty() {
                        return Err("Stack underflow for function return".to_string());
                    }

                    let return_value = self.stack.pop().unwrap();

                    // Get return address from BP-2
                    if self.bp >= 2 && self.bp < self.stack.len() + 1 {
                        let return_addr = self.stack[self.bp - 2].clone();

                        // Get old BP from BP-1
                        let old_bp = match self.stack[self.bp - 1] {
                            Value::Address(addr) => addr,
                            Value::Int(n) => n as usize,
                            _ => return Err("Invalid old BP type".to_string()),
                        };

                        // Restore stack to caller frame and put return value on top
                        self.stack.truncate(self.bp - 1); // Remove current frame
                        self.stack.push(return_value); // Place return value

                        // Restore BP
                        self.bp = old_bp;

                        // Jump to return address
                        match return_addr {
                            Value::Address(addr) => {
                                self.pc = addr;
                            }
                            _ => return Err("Return address must be an address".to_string()),
                        }
                    } else {
                        return Err("Invalid BP for function return".to_string());
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

            Instruction::MethodCall(argc) => {
                // For now, convert method calls to regular function calls with name mangling
                // This is a simplified implementation that assumes we have type information
                // Stack: [args...] [object] [method_name]
                if self.stack.len() < argc + 2 {
                    return Err("Stack underflow for MethodCall".to_string());
                }

                // Get method name
                let method_name_ref = self.stack.pop().unwrap();
                let method_name = match method_name_ref {
                    Value::HeapRef(heap_index) => {
                        if heap_index.0 >= self.heap.len() {
                            return Err(format!("Invalid heap index: {}", heap_index.0));
                        }
                        match &self.heap[heap_index.0] {
                            HeapObject::String(s) => s.clone(),
                            _ => return Err("MethodCall requires a string method name".to_string()),
                        }
                    }
                    _ => {
                        return Err(
                            "MethodCall requires a heap reference for method name".to_string()
                        )
                    }
                };

                // Get object - we need to determine its struct type
                // For now, we'll hardcode this for the Point example
                // In a real implementation, we'd store type information with the object
                let object = self.stack.pop().unwrap();

                // Push object back as first argument for method call
                self.stack.push(object);

                // For the Point example, assume it's a Point struct
                // In a real implementation, we'd look up the type information
                let mangled_method_name = format!("Point_{}", method_name);

                // Create a function call instruction
                let call_instruction = Instruction::Call(mangled_method_name);

                // Set up stack frame and call the function
                // Push return address, old BP, etc.
                let old_bp = self.bp;
                let return_address = self.pc + 1;

                // Set up new frame
                self.stack.push(Value::Int(-1)); // placeholder for return value
                self.stack.push(Value::Int(return_address as i64));
                self.stack.push(Value::Int(old_bp as i64));
                self.stack
                    .push(Value::Int((self.stack.len() - argc - 1) as i64));

                self.bp = self.stack.len() - 1;

                // Find and jump to function
                if let Instruction::Call(func_name) = &call_instruction {
                    for (i, instr) in self.program.iter().enumerate() {
                        if let Instruction::Label(label) = instr {
                            if label == func_name {
                                self.pc = i + 1;
                                break;
                            }
                        }
                    }
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

                                        // Replace the return placeholder with the number of bytes written
                                        if let Some(top) = self.stack.last_mut() {
                                            *top = Value::Int(actual_length as i64);
                                        }
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

/// Compiler for generating VM bytecode from AST
pub struct VMCompiler {
    instructions: Vec<Instruction>,
    local_vars: HashMap<String, i32>,
    local_offset: i32,
    current_function_param_count: usize,
    current_function_name: String,
    functions: HashMap<String, Function>,
    structs: HashMap<String, StructDecl>,
    string_constants: HashMap<String, usize>, // string -> heap index mapping
    string_constant_list: Vec<String>,        // ordered list of string constants
}

impl VMCompiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            local_vars: HashMap::new(),
            local_offset: 0,
            current_function_param_count: 0,
            current_function_name: String::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            string_constants: HashMap::new(),
            string_constant_list: Vec::new(),
        }
    }

    /// Get or create a string constant, returning its heap index
    fn get_or_create_string_constant(&mut self, s: &str) -> usize {
        if let Some(&index) = self.string_constants.get(s) {
            index
        } else {
            let index = self.string_constant_list.len();
            self.string_constants.insert(s.to_string(), index);
            self.string_constant_list.push(s.to_string());
            index
        }
    }

    /// Get the list of string constants for VM initialization
    pub fn get_string_constants(&self) -> &[String] {
        &self.string_constant_list
    }

    /// Collect string constants from a declaration
    fn collect_string_constants_from_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Function(func) => {
                for stmt in &func.body {
                    self.collect_string_constants_from_stmt(stmt);
                }
            }
            Decl::GlobalVariable(global_var) => {
                self.collect_string_constants_from_expr(&global_var.value);
            }
            Decl::Struct(struct_decl) => {
                for method in &struct_decl.methods {
                    for stmt in &method.body {
                        self.collect_string_constants_from_stmt(stmt);
                    }
                }
            }
        }
    }

    /// Collect string constants from a statement
    fn collect_string_constants_from_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { value, .. } => {
                self.collect_string_constants_from_expr(value);
            }
            Stmt::Assign { value, .. } => {
                self.collect_string_constants_from_expr(value);
            }
            Stmt::Return(expr) => {
                self.collect_string_constants_from_expr(expr);
            }
            Stmt::Expression(expr) => {
                self.collect_string_constants_from_expr(expr);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_string_constants_from_expr(condition);
                for stmt in then_branch {
                    self.collect_string_constants_from_stmt(stmt);
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.collect_string_constants_from_stmt(stmt);
                    }
                }
            }
            Stmt::While { condition, body } => {
                self.collect_string_constants_from_expr(condition);
                for stmt in body {
                    self.collect_string_constants_from_stmt(stmt);
                }
            }
            Stmt::VectorPush { value, .. } => {
                self.collect_string_constants_from_expr(value);
            }
            Stmt::IndexAssign { index, value, .. } => {
                self.collect_string_constants_from_expr(index);
                self.collect_string_constants_from_expr(value);
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.collect_string_constants_from_expr(object);
                self.collect_string_constants_from_expr(value);
            }
        }
    }

    /// Collect string constants from an expression
    fn collect_string_constants_from_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::String(s) => {
                self.get_or_create_string_constant(s);
            }
            Expr::Binary { left, right, .. } => {
                self.collect_string_constants_from_expr(left);
                self.collect_string_constants_from_expr(right);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.collect_string_constants_from_expr(arg);
                }
            }
            Expr::MethodCall { object, args, .. } => {
                self.collect_string_constants_from_expr(object);
                for arg in args {
                    self.collect_string_constants_from_expr(arg);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.collect_string_constants_from_expr(object);
            }
            Expr::Index {
                container, index, ..
            } => {
                self.collect_string_constants_from_expr(container);
                self.collect_string_constants_from_expr(index);
            }
            Expr::StructNew { fields, .. } => {
                for (_, field_value) in fields {
                    self.collect_string_constants_from_expr(field_value);
                }
            }
            Expr::VectorNew { initial_values, .. } => {
                for value in initial_values {
                    self.collect_string_constants_from_expr(value);
                }
            }
            Expr::PointerAlloc { initial_values, .. } => {
                for value in initial_values {
                    self.collect_string_constants_from_expr(value);
                }
            }
            Expr::MapNew { initial_pairs, .. } => {
                for (key, value) in initial_pairs {
                    self.collect_string_constants_from_expr(key);
                    self.collect_string_constants_from_expr(value);
                }
            }
            Expr::TypeExpr { type_name } => {
                self.get_or_create_string_constant(type_name);
            }
            _ => {} // Other expressions don't contain strings
        }
    }

    /// Dump compiled IR to a string
    pub fn dump_ir(&self) -> String {
        let mut output = String::new();
        output.push_str("; Orbit VM IR Dump\n");
        output.push_str("; Generated by Orbit Compiler\n\n");

        // Add instructions with line numbers
        for (i, instruction) in self.instructions.iter().enumerate() {
            match instruction {
                Instruction::Label(name) => {
                    output.push_str(&format!("\n{}:\n", name));
                }
                _ => {
                    output.push_str(&format!("{:4}: {}\n", i, instruction));
                }
            }
        }

        output
    }

    /// Dump compiled IR to a file
    pub fn dump_ir_to_file(&self, filename: &str) -> Result<(), std::io::Error> {
        use std::fs::File;
        use std::io::Write;

        let ir_content = self.dump_ir();
        let mut file = File::create(filename)?;
        file.write_all(ir_content.as_bytes())?;
        Ok(())
    }

    /// Compile a complete program to VM bytecode
    pub fn compile_program(&mut self, program: &Program) -> Vec<Instruction> {
        // 0. First pass: register all struct types, collect methods, and collect string constants
        self.structs.clear();
        self.string_constants.clear();
        self.string_constant_list.clear();
        let mut all_methods: Vec<Function> = Vec::new();

        // Pre-scan for string constants
        for decl in &program.declarations {
            self.collect_string_constants_from_decl(decl);
        }

        for decl in &program.declarations {
            if let Decl::Struct(struct_decl) = decl {
                self.structs
                    .insert(struct_decl.name.clone(), struct_decl.clone());

                // Collect struct methods with name mangling
                for method in &struct_decl.methods {
                    let mut mangled_method = method.clone();
                    mangled_method.name = format!("{}_{}", struct_decl.name, method.name);
                    all_methods.push(mangled_method);
                }
            }
        }

        // 1. 関数を定義順に収集し、関数レジストリに登録
        let mut functions: Vec<Function> = Vec::new();
        let mut global_variables: Vec<GlobalVariable> = Vec::new();
        self.functions.clear();
        for decl in &program.declarations {
            match decl {
                Decl::Function(func) => {
                    functions.push(func.clone());
                    self.functions.insert(func.name.clone(), func.clone());
                }
                Decl::Struct(_) => {
                    // Already handled in first pass
                }
                Decl::GlobalVariable(global_var) => {
                    // Collect global variables for later initialization
                    global_variables.push(global_var.clone());
                }
            }
        }

        // Register struct methods in the function registry
        for method in &all_methods {
            functions.push(method.clone());
            self.functions.insert(method.name.clone(), method.clone());
        }

        // 2. プログラムの先頭に初期化とcall main; ret を挿入
        self.instructions.clear();
        self.local_vars.clear();
        self.local_offset = 0;
        self.current_function_param_count = 0;

        // Initialize string constants in heap first
        for string_constant in &self.string_constant_list {
            self.instructions
                .push(Instruction::PushString(string_constant.clone()));
            self.instructions.push(Instruction::Pop); // Discard the HeapRef from stack
        }

        // Initialize global variables second
        for global_var in &global_variables {
            self.compile_expression(&global_var.value);
            self.instructions
                .push(Instruction::SetGlobal(global_var.name.clone()));
        }

        self.instructions.push(Instruction::Push(-1)); // placeholder for return value
        self.instructions.push(Instruction::Push(0)); // placeholder for return address
        self.instructions.push(Instruction::Push(-1)); // placeholder for old BP
        self.instructions.push(Instruction::Push(3));
        self.instructions.push(Instruction::SetBP);
        self.instructions
            .push(Instruction::Call("main".to_string()));
        self.instructions.push(Instruction::Ret);

        // 3. 各関数をコンパイル（ラベルと本体を一緒に）
        for func in &functions {
            self.compile_function(func);
        }

        self.instructions.clone()
    }

    /// Compile a function to VM bytecode
    pub fn compile_function(&mut self, func: &Function) -> usize {
        let func_start = self.instructions.len();

        // Add function label
        self.instructions
            .push(Instruction::Label(func.name.clone()));

        // Set current function name for return handling
        self.current_function_name = func.name.clone();

        // Function prologue
        self.emit_function_prologue(func);

        // Compile function body
        for stmt in &func.body {
            self.compile_statement(stmt);
        }

        // Default return value if no explicit return
        // For main function, default return should also be handled specially
        if func.name == "main" {
            self.instructions.push(Instruction::Push(-1));
            self.instructions.push(Instruction::Ret);
        } else {
            self.instructions.push(Instruction::Push(-1));
            self.emit_return_sequence();
        }

        func_start
    }

    /// Emit function prologue - sets up stack frame and parameter mappings
    fn emit_function_prologue(&mut self, func: &Function) {
        // Reserve space for local variables
        // (This is simplified - in a real implementation we'd analyze the function first)
        self.local_offset = 0;
        self.local_vars.clear();
        self.current_function_param_count = func.params.len();

        // Map parameters to stack positions (negative offsets from BP)
        // Stack layout: [return_placeholder] [arg0] [arg1] ... [return_addr] [old_bp] <- BP points here
        // Parameters are accessed with negative offsets from BP
        // First param is at BP-(num_params+2), second at BP-(num_params+1), etc.
        for (i, param) in func.params.iter().enumerate() {
            let param_offset = -(func.params.len() as i32 - i as i32 + 2);
            self.local_vars.insert(param.name.clone(), param_offset);
        }
    }

    /// Emit return sequence for non-main functions  
    fn emit_return_sequence(&mut self) {
        // Leave return value on stack and just call Ret
        // Let Ret handle the complex stack frame restoration
        self.instructions.push(Instruction::Ret);
    }

    /// Compile a statement to VM bytecode
    fn compile_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { name, value } => {
                // Compile the value expression
                self.compile_expression(value);

                // Assign to local variable
                let offset = self.local_offset;
                self.local_vars.insert(name.clone(), offset);
                self.instructions.push(Instruction::SetLocal(offset));
                self.local_offset += 1;
            }

            Stmt::Expression(expr) => {
                self.compile_expression(expr);
                self.instructions.push(Instruction::Pop); // Discard result
            }

            Stmt::Return(expr) => {
                // Compile return value expression
                self.compile_expression(expr);

                // Check if this is the main function
                if self.current_function_name == "main" {
                    // For main function, just return without complex frame handling
                    self.instructions.push(Instruction::Ret);
                } else {
                    self.emit_return_sequence();
                }
            }

            Stmt::Assign { name, value } => {
                self.compile_expression(value);
                if let Some(&offset) = self.local_vars.get(name) {
                    self.instructions.push(Instruction::SetLocal(offset));
                } else {
                    // Try to assign to global variable
                    self.instructions.push(Instruction::SetGlobal(name.clone()));
                }
            }

            Stmt::While { condition, body } => {
                // While loop structure:
                // loop_start:
                //   condition
                //   jump_if_zero loop_end
                //   body
                //   jump loop_start
                // loop_end:

                let loop_start = self.instructions.len();

                // Compile condition
                self.compile_expression(condition);

                // Jump to end if condition is false
                let jump_to_end = self.instructions.len();
                self.instructions.push(Instruction::JumpIfZero(0)); // Placeholder

                // Compile body
                for stmt in body {
                    self.compile_statement(stmt);
                }

                // Jump back to start
                self.instructions.push(Instruction::Jump(loop_start));

                // Set the jump target for the end
                let loop_end = self.instructions.len();
                self.instructions[jump_to_end] = Instruction::JumpIfZero(loop_end);
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // If statement structure:
                //   condition
                //   jump_if_zero else_branch
                //   then_branch
                //   jump end
                // else_branch:
                //   else_branch (if exists)
                // end:

                // Compile condition
                self.compile_expression(condition);

                // Jump to else if condition is false
                let jump_to_else = self.instructions.len();
                self.instructions.push(Instruction::JumpIfZero(0)); // Placeholder

                // Compile then branch
                for stmt in then_branch {
                    self.compile_statement(stmt);
                }

                // Jump to end (skip else branch)
                let jump_to_end = self.instructions.len();
                self.instructions.push(Instruction::Jump(0)); // Placeholder

                // Set jump target for else branch
                let else_start = self.instructions.len();
                self.instructions[jump_to_else] = Instruction::JumpIfZero(else_start);

                // Compile else branch if it exists
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.compile_statement(stmt);
                    }
                }

                // Set jump target for end
                let end = self.instructions.len();
                self.instructions[jump_to_end] = Instruction::Jump(end);
            }

            Stmt::VectorPush { vector, value } => {
                // vector: 変数名, value: 式
                // valueをpushし、vector変数の位置をpushし、VectorPush命令
                self.compile_expression(value);
                if let Some(&offset) = self.local_vars.get(vector) {
                    self.instructions.push(Instruction::GetLocal(offset));
                } else {
                    panic!("Undefined vector variable: {}", vector);
                }
                self.instructions.push(Instruction::VectorPush);
            }

            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type,
            } => {
                // container[index] = value
                // Compile different instructions based on container type
                self.compile_expression(value);
                self.compile_expression(index);
                if let Some(&offset) = self.local_vars.get(container) {
                    self.instructions.push(Instruction::GetLocal(offset));
                } else {
                    panic!("Undefined container variable: {}", container);
                }

                match container_type {
                    Some(IndexContainerType::Vector) => {
                        self.instructions.push(Instruction::VectorSet);
                    }
                    Some(IndexContainerType::Map) => {
                        self.instructions.push(Instruction::MapSet);
                    }
                    Some(IndexContainerType::Pointer) => {
                        self.instructions.push(Instruction::PointerSet);
                    }
                    Some(IndexContainerType::String) => {
                        // Strings are immutable, this should not happen
                        panic!("Cannot assign to string index - strings are immutable");
                    }
                    None => {
                        // Type checking should have resolved this
                        panic!("Container type not resolved during type checking");
                    }
                }
            }

            Stmt::FieldAssign {
                object,
                field,
                value,
            } => {
                // obj.field = value
                self.compile_expression(value);
                self.instructions
                    .push(Instruction::PushString(field.clone()));
                self.compile_expression(object);
                self.instructions.push(Instruction::StructFieldSet);
            }
        }
    }

    /// Compile an expression to VM bytecode
    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(value) => {
                self.instructions.push(Instruction::Push(*value as i64));
            }

            Expr::Boolean(value) => {
                self.instructions
                    .push(Instruction::Push(if *value { 1 } else { 0 }));
            }

            Expr::String(value) => {
                let heap_index = self.get_or_create_string_constant(value);
                self.instructions.push(Instruction::PushHeapRef(heap_index));
            }

            Expr::Identifier(name) => {
                if let Some(&offset) = self.local_vars.get(name) {
                    self.instructions.push(Instruction::GetLocal(offset));
                } else if self.functions.contains_key(name) {
                    // Function identifier used as expression (function pointer/name)
                    // For now, just push a placeholder value - functions should be called, not used as values
                    // This could be enhanced later to support function pointers
                    panic!(
                        "Function '{}' cannot be used as a value (use function calls instead)",
                        name
                    );
                } else {
                    // Try to access as global variable
                    self.instructions.push(Instruction::GetGlobal(name.clone()));
                }
            }

            Expr::Binary { left, op, right } => {
                self.compile_expression(left);
                self.compile_expression(right);

                match op {
                    BinaryOp::Add => self.instructions.push(Instruction::Add),
                    BinaryOp::Subtract => self.instructions.push(Instruction::Sub),
                    BinaryOp::Multiply => self.instructions.push(Instruction::Mul),
                    BinaryOp::Divide => self.instructions.push(Instruction::Div),
                    BinaryOp::Equal => self.instructions.push(Instruction::Eq),
                    BinaryOp::NotEqual => {
                        self.instructions.push(Instruction::Eq);
                        self.instructions.push(Instruction::Push(1));
                        self.instructions.push(Instruction::Sub);
                    }
                    BinaryOp::Less => self.instructions.push(Instruction::Lt),
                    BinaryOp::LessEqual => self.instructions.push(Instruction::Lte),
                    BinaryOp::Greater => self.instructions.push(Instruction::Gt),
                    BinaryOp::GreaterEqual => self.instructions.push(Instruction::Gte),
                }
            }

            Expr::Call { callee, args } => {
                if let Expr::Identifier(func_name) = callee.as_ref() {
                    // Handle syscall as a special built-in function
                    if func_name == "syscall" {
                        // For syscall, push arguments directly onto stack and call syscall instruction
                        for arg in args {
                            self.compile_expression(arg);
                        }
                        self.instructions.push(Instruction::Syscall);
                        return;
                    }

                    // Check if function exists
                    if !self.functions.contains_key(func_name) {
                        panic!("Undefined function: {}", func_name);
                    }
                }

                // Caller responsibilities according to VM spec:
                // 1. Push placeholder for return value
                self.instructions.push(Instruction::Push(-1));

                // 2. Push function arguments onto stack (left to right)
                for arg in args {
                    self.compile_expression(arg);
                }

                if let Expr::Identifier(func_name) = callee.as_ref() {
                    // 3. Push current PC + offset (return address)
                    // PC will be at Call instruction, so return address is PC + 1
                    self.instructions.push(Instruction::GetPC);
                    self.instructions.push(Instruction::Push(6)); // Offset to return address (after Call)
                    self.instructions.push(Instruction::AddressAdd);

                    // 4. Push current BP (old base pointer)
                    self.instructions.push(Instruction::GetBP);

                    // 5. Set new BP to current SP
                    self.instructions.push(Instruction::GetSP);
                    self.instructions.push(Instruction::SetBP);

                    // 6. Jump to function
                    self.instructions.push(Instruction::Call(func_name.clone()));

                    // After function return, the return value is on top of stack
                    // No need to pop arguments - they are handled by the function's stack frame
                } else {
                    panic!("Function calls with complex callees not supported yet");
                }
            }

            Expr::VectorNew { .. } => {
                // 現状型情報は無視し、空ベクターをpush
                self.instructions.push(Instruction::VectorNew);
            }

            Expr::PointerAlloc { initial_values, .. } => {
                // Push initial values onto stack first
                for value in initial_values {
                    self.compile_expression(value);
                }
                // Push the count of initial values
                self.instructions
                    .push(Instruction::Push(initial_values.len() as i64));
                self.instructions.push(Instruction::PointerAlloc);
            }

            Expr::Index {
                container,
                index,
                container_type,
            } => {
                // container, indexの順でpushし、適切なIndex命令
                self.compile_expression(container);
                self.compile_expression(index);

                match container_type {
                    Some(IndexContainerType::Vector) => {
                        self.instructions.push(Instruction::VectorIndex);
                    }
                    Some(IndexContainerType::Map) => {
                        self.instructions.push(Instruction::MapIndex);
                    }
                    Some(IndexContainerType::Pointer) => {
                        self.instructions.push(Instruction::PointerIndex);
                    }
                    Some(IndexContainerType::String) => {
                        self.instructions.push(Instruction::StringIndex);
                    }
                    None => {
                        // Type checking should have resolved this
                        panic!("Container type not resolved during type checking");
                    }
                }
            }

            Expr::MapNew { .. } => {
                self.instructions.push(Instruction::MapNew);
            }

            Expr::StructNew { type_name, fields } => {
                // Verify struct exists
                if !self.structs.contains_key(type_name) {
                    panic!("Unknown struct type: {}", type_name);
                }

                // Push field values and names onto stack
                for (field_name, field_value) in fields {
                    self.instructions
                        .push(Instruction::PushString(field_name.clone()));
                    self.compile_expression(field_value);
                }
                // Push field count
                self.instructions
                    .push(Instruction::Push(fields.len() as i64));
                self.instructions.push(Instruction::StructNew);
            }

            Expr::StructNewPattern { type_name, fields } => {
                // Verify struct exists
                if !self.structs.contains_key(type_name) {
                    panic!("Unknown struct type: {}", type_name);
                }

                // Push field values and names onto stack
                for (field_name, field_value) in fields {
                    self.instructions
                        .push(Instruction::PushString(field_name.clone()));
                    self.compile_expression(field_value);
                }
                // Push field count
                self.instructions
                    .push(Instruction::Push(fields.len() as i64));
                self.instructions.push(Instruction::StructNew);
            }

            Expr::FieldAccess { object, field } => {
                self.compile_expression(object);
                self.instructions
                    .push(Instruction::PushString(field.clone()));
                self.instructions.push(Instruction::StructFieldGet);
            }

            Expr::MethodCall {
                object,
                method,
                args,
                ..
            } => {
                // Method calls are compiled as function calls with name mangling
                // First, determine the object type to construct the method name
                // For now, we'll need to get the struct type at runtime
                for arg in args {
                    self.compile_expression(arg);
                }
                self.compile_expression(object);

                // Push the method name - the actual mangled name will be resolved at runtime
                self.instructions
                    .push(Instruction::PushString(method.clone()));
                self.instructions.push(Instruction::MethodCall(args.len()));
            }

            Expr::Alloc { element_type, size } => {
                // Compile the size expression
                self.compile_expression(size);
                // Push element type as string for VM to calculate sizeof
                self.instructions
                    .push(Instruction::PushString(element_type.clone()));
                // Allocate pointer with specified type and size
                self.instructions.push(Instruction::PointerAlloc);
            }

            Expr::TypeExpr { type_name } => {
                // For now, type expressions are treated as string constants
                // This is a placeholder - in a full implementation, we'd need proper type system integration
                let heap_index = self.get_or_create_string_constant(type_name);
                self.instructions.push(Instruction::PushHeapRef(heap_index));
            }
        }
    }
}

/// Compile an AST expression to VM bytecode
pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    compile_expr_recursive(expr, &mut instructions);
    instructions
}

fn compile_expr_recursive(expr: &Expr, instructions: &mut Vec<Instruction>) {
    match expr {
        Expr::Int(value) => {
            instructions.push(Instruction::Push(*value as i64));
        }

        Expr::Boolean(value) => {
            instructions.push(Instruction::Push(if *value { 1 } else { 0 }));
        }

        Expr::String(value) => {
            instructions.push(Instruction::PushString(value.clone()));
        }

        Expr::Binary { left, op, right } => {
            // Compile operands in order (left first, then right)
            compile_expr_recursive(left, instructions);
            compile_expr_recursive(right, instructions);

            // Add the operation instruction
            match op {
                BinaryOp::Add => instructions.push(Instruction::Add),
                BinaryOp::Subtract => instructions.push(Instruction::Sub),
                BinaryOp::Multiply => instructions.push(Instruction::Mul),
                BinaryOp::Divide => instructions.push(Instruction::Div),
                BinaryOp::Equal => instructions.push(Instruction::Eq),
                BinaryOp::NotEqual => {
                    instructions.push(Instruction::Eq);
                    instructions.push(Instruction::Not);
                }
                BinaryOp::Less => instructions.push(Instruction::Lt),
                BinaryOp::LessEqual => instructions.push(Instruction::Lte),
                BinaryOp::Greater => instructions.push(Instruction::Gt),
                BinaryOp::GreaterEqual => instructions.push(Instruction::Gte),
            }
        }

        Expr::Identifier(_name) => {
            // For now, just push 0 - will implement proper variable handling later
            instructions.push(Instruction::Push(0));
        }

        Expr::Call { callee, args } => {
            if let Expr::Identifier(func_name) = callee.as_ref() {
                if func_name == "syscall" {
                    // Handle syscall in standalone compilation
                    for arg in args {
                        compile_expr_recursive(arg, instructions);
                    }
                    instructions.push(Instruction::Syscall);
                    return;
                }
            }
            // For other function calls, just push 0 - need more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::VectorNew { .. } => {
            // For now, just push 0 - vectors need more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::PointerAlloc { .. } => {
            // For now, just push 0 - pointers need more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::Index { .. } => {
            // For now, just push 0 - indexing needs more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::MapNew { .. } => {
            instructions.push(Instruction::MapNew);
        }

        Expr::StructNew {
            type_name: _,
            fields,
        } => {
            // Note: struct type checking should be done at type check time
            // For standalone compilation, we assume struct is valid

            // Push field values and names onto stack
            for (field_name, field_value) in fields {
                instructions.push(Instruction::PushString(field_name.clone()));
                compile_expr_recursive(field_value, instructions);
            }
            // Push field count
            instructions.push(Instruction::Push(fields.len() as i64));
            instructions.push(Instruction::StructNew);
        }

        Expr::StructNewPattern {
            type_name: _,
            fields,
        } => {
            // Note: struct type checking should be done at type check time
            // For standalone compilation, we assume struct is valid

            // Push field values and names onto stack
            for (field_name, field_value) in fields {
                instructions.push(Instruction::PushString(field_name.clone()));
                compile_expr_recursive(field_value, instructions);
            }
            // Push field count
            instructions.push(Instruction::Push(fields.len() as i64));
            instructions.push(Instruction::StructNew);
        }

        Expr::FieldAccess { object, field } => {
            compile_expr_recursive(object, instructions);
            instructions.push(Instruction::PushString(field.clone()));
            instructions.push(Instruction::StructFieldGet);
        }

        Expr::MethodCall {
            object,
            method,
            args,
            object_type: _,
        } => {
            // Compile arguments first
            for arg in args {
                compile_expr_recursive(arg, instructions);
            }
            // Compile object
            compile_expr_recursive(object, instructions);
            // Push method name
            instructions.push(Instruction::PushString(method.clone()));
            // Method call with argument count
            instructions.push(Instruction::MethodCall(args.len()));
        }

        Expr::Alloc { element_type, size } => {
            // Compile the size expression
            compile_expr_recursive(size, instructions);
            // Push element type as string for VM to calculate sizeof
            instructions.push(Instruction::PushString(element_type.clone()));
            // Allocate pointer with specified type and size
            instructions.push(Instruction::PointerAlloc);
        }

        Expr::TypeExpr { type_name } => {
            // For now, type expressions are treated as string constants
            instructions.push(Instruction::PushString(type_name.clone()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper function to run VM test cases
    fn run_vm_test_cases(test_cases: Vec<(Vec<Instruction>, i64)>) {
        for (program, expected) in test_cases {
            let mut vm = VM::new();
            vm.load_program(program);
            let result = vm.execute().unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_simple_arithmetic() {
        let test_cases = vec![
            (
                vec![Instruction::Push(2), Instruction::Push(3), Instruction::Add],
                5,
            ),
            (
                vec![
                    Instruction::Push(10),
                    Instruction::Push(4),
                    Instruction::Sub,
                ],
                6,
            ),
            (
                vec![Instruction::Push(3), Instruction::Push(4), Instruction::Mul],
                12,
            ),
            (
                vec![
                    Instruction::Push(15),
                    Instruction::Push(3),
                    Instruction::Div,
                ],
                5,
            ),
            (
                vec![
                    Instruction::Push(10),
                    Instruction::Push(3),
                    Instruction::Mod,
                ],
                1,
            ),
        ];

        run_vm_test_cases(test_cases);
    }

    #[test]
    fn test_comparison_operations() {
        let test_cases = vec![
            (
                vec![Instruction::Push(5), Instruction::Push(5), Instruction::Eq],
                1,
            ),
            (
                vec![Instruction::Push(3), Instruction::Push(7), Instruction::Eq],
                0,
            ),
            (
                vec![Instruction::Push(3), Instruction::Push(7), Instruction::Lt],
                1,
            ),
            (
                vec![Instruction::Push(7), Instruction::Push(3), Instruction::Lt],
                0,
            ),
            (
                vec![Instruction::Push(3), Instruction::Push(7), Instruction::Lte],
                1,
            ),
            (
                vec![Instruction::Push(7), Instruction::Push(7), Instruction::Lte],
                1,
            ),
            (
                vec![Instruction::Push(7), Instruction::Push(3), Instruction::Gt],
                1,
            ),
            (
                vec![Instruction::Push(3), Instruction::Push(7), Instruction::Gt],
                0,
            ),
            (
                vec![Instruction::Push(7), Instruction::Push(3), Instruction::Gte],
                1,
            ),
            (
                vec![Instruction::Push(7), Instruction::Push(7), Instruction::Gte],
                1,
            ),
        ];

        run_vm_test_cases(test_cases);
    }

    #[test]
    fn test_local_variables() {
        let mut vm = VM::new();
        vm.load_program(vec![
            Instruction::Push(42),
            Instruction::SetLocal(0),
            Instruction::GetLocal(0),
        ]);
        let result = vm.execute().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_jump_operations() {
        let mut vm = VM::new();
        vm.load_program(vec![
            Instruction::Push(0),
            Instruction::JumpIfZero(4), // Jump to instruction 4 (Push(99))
            Instruction::Push(1),       // This should be skipped
            Instruction::Add,           // This should be skipped
            Instruction::Push(99),      // Jump target
        ]);
        let result = vm.execute().unwrap();
        assert_eq!(result, 99);
    }

    #[test]
    fn test_ast_compilation() {
        use crate::ast::{BinaryOp, Expr};

        // Test simple literal
        let expr = Expr::Int(42);
        let instructions = compile_expression(&expr);
        assert_eq!(instructions, vec![Instruction::Push(42)]);

        // Test boolean
        let expr = Expr::Boolean(true);
        let instructions = compile_expression(&expr);
        assert_eq!(instructions, vec![Instruction::Push(1)]);

        // Test binary expression: 2 + 3
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(2)),
            op: BinaryOp::Add,
            right: Box::new(Expr::Int(3)),
        };
        let instructions = compile_expression(&expr);
        assert_eq!(
            instructions,
            vec![Instruction::Push(2), Instruction::Push(3), Instruction::Add,]
        );

        // Test complex expression: (2 + 3) * 4
        let expr = Expr::Binary {
            left: Box::new(Expr::Binary {
                left: Box::new(Expr::Int(2)),
                op: BinaryOp::Add,
                right: Box::new(Expr::Int(3)),
            }),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Int(4)),
        };
        let instructions = compile_expression(&expr);
        assert_eq!(
            instructions,
            vec![
                Instruction::Push(2),
                Instruction::Push(3),
                Instruction::Add,
                Instruction::Push(4),
                Instruction::Mul,
            ]
        );
    }

    #[test]
    fn test_alloc_expression_compilation() {
        use crate::ast::Expr;

        // Test alloc with constant size: alloc int 10
        let expr = Expr::Alloc {
            element_type: "int".to_string(),
            size: Box::new(Expr::Int(10)),
        };
        let instructions = compile_expression(&expr);
        assert_eq!(
            instructions,
            vec![
                Instruction::Push(10),
                Instruction::PushString("int".to_string()),
                Instruction::PointerAlloc
            ]
        );

        // Test alloc with expression size: alloc byte (5 + 3)
        let expr = Expr::Alloc {
            element_type: "byte".to_string(),
            size: Box::new(Expr::Binary {
                left: Box::new(Expr::Int(5)),
                op: crate::ast::BinaryOp::Add,
                right: Box::new(Expr::Int(3)),
            }),
        };
        let instructions = compile_expression(&expr);
        assert_eq!(
            instructions,
            vec![
                Instruction::Push(5),
                Instruction::Push(3),
                Instruction::Add,
                Instruction::PushString("byte".to_string()),
                Instruction::PointerAlloc
            ]
        );
    }

    #[test]
    fn test_ast_to_vm_execution() {
        use crate::ast::{BinaryOp, Expr};

        // Test 2 + 3 * 4 (should be 14 with proper precedence)
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(2)),
            op: BinaryOp::Add,
            right: Box::new(Expr::Binary {
                left: Box::new(Expr::Int(3)),
                op: BinaryOp::Multiply,
                right: Box::new(Expr::Int(4)),
            }),
        };

        let instructions = compile_expression(&expr);
        let mut vm = VM::new();
        vm.load_program(instructions);
        let result = vm.execute().unwrap();
        assert_eq!(result, 14);
    }

    #[test]
    fn test_vm_simple_function() {
        use crate::ast::{Decl, Function, Program};

        // Create main function: fun main() do return 42; end
        let main_func = Function {
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![Stmt::Return(Expr::Int(42))],
        };

        let program = Program {
            declarations: vec![Decl::Function(main_func)],
        };

        // Compile and execute
        let mut compiler = VMCompiler::new();
        let instructions = compiler.compile_program(&program);

        // Print IR for debugging
        println!("Generated IR:");
        println!("{}", compiler.dump_ir());

        let mut vm = VM::new();
        vm.load_program(instructions);
        let result = vm.execute().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_vm_function_with_parameters() {
        use crate::ast::{Decl, FunParam, Function, Program};

        // Create a simple function: fun add(x, y) do return x + y; end
        let add_func = Function {
            name: "add".to_string(),
            type_params: vec![],
            params: vec![
                FunParam {
                    name: "x".to_string(),
                    type_name: None,
                },
                FunParam {
                    name: "y".to_string(),
                    type_name: None,
                },
            ],
            body: vec![Stmt::Return(Expr::Binary {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: BinaryOp::Add,
                right: Box::new(Expr::Identifier("y".to_string())),
            })],
        };

        // Create main function: fun main() do return add(2, 3); end
        let main_func = Function {
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![Stmt::Return(Expr::Call {
                callee: Box::new(Expr::Identifier("add".to_string())),
                args: vec![Expr::Int(2), Expr::Int(3)],
            })],
        };

        let program = Program {
            declarations: vec![Decl::Function(add_func), Decl::Function(main_func)],
        };

        // Compile and execute
        let mut compiler = VMCompiler::new();
        let instructions = compiler.compile_program(&program);

        // Print IR for debugging
        println!("Generated IR:");
        println!("{}", compiler.dump_ir());

        let mut vm = VM::new_with_stack_printing(true);
        vm.load_program(instructions);
        let result = vm.execute().unwrap();
        println!("Final result: {}", result);
        assert_eq!(result, 5);
    }

    #[test]
    fn dump_ir_for_vector_program() {
        use crate::lexer::Lexer;
        use crate::parser::Parser;
        use crate::typecheck::TypeChecker;
        use crate::vm::VMCompiler;
        use std::fs;

        let path = "tests/testcase/vector_program.ob";
        let ir_path = "target/vector_program.ir";
        let code = fs::read_to_string(path).expect("failed to read vector_program.ob");
        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize().expect("tokenize failed");
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program().expect("parse_program failed");

        // Perform type checking and inference
        let mut type_checker = TypeChecker::new();
        type_checker
            .infer_types(&mut program)
            .expect("type inference failed");
        type_checker
            .check_program(&program)
            .expect("type checking failed");

        let mut compiler = VMCompiler::new();
        compiler.compile_program(&program);
        compiler.dump_ir_to_file(ir_path).expect("dump_ir failed");
        println!("IR dumped to {}", ir_path);
    }
}
