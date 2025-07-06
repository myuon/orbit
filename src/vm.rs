use crate::ast::{BinaryOp, Decl, Expr, Function, Program, Stmt};
use crate::profiler::{InstructionTimer, Profiler};
use crate::runtime::Value;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Stack operations
    Push(i64),
    Pop,

    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,

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

    // Function calls
    Call(String),
    Ret,

    // Frame management
    GetBP,
    SetBP,
    GetSP,
    SetSP,

    // Labels
    Label(String),

    // No operation
    Nop,

    // Vector operations
    VectorNew,
    VectorPush,
    VectorIndex,
    VectorSet,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Push(value) => write!(f, "push {}", value),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Add => write!(f, "add"),
            Instruction::Sub => write!(f, "sub"),
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
            Instruction::Call(func_name) => write!(f, "call {}", func_name),
            Instruction::Ret => write!(f, "ret"),
            Instruction::GetBP => write!(f, "get_bp"),
            Instruction::SetBP => write!(f, "set_bp"),
            Instruction::GetSP => write!(f, "get_sp"),
            Instruction::SetSP => write!(f, "set_sp"),
            Instruction::Label(name) => write!(f, "{}:", name),
            Instruction::Nop => write!(f, "nop"),
            Instruction::VectorNew => write!(f, "vector_new"),
            Instruction::VectorPush => write!(f, "vector_push"),
            Instruction::VectorIndex => write!(f, "vector_index"),
            Instruction::VectorSet => write!(f, "vector_set"),
        }
    }
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    pc: usize, // program counter
    bp: usize, // base pointer for stack frame
    sp: usize, // stack pointer
    program: Vec<Instruction>,
    pub print_stacks: bool,   // whether to print stack state during execution
    print_stacks_on_call: Option<String>, // print stacks only when calling this function
    vectors: Vec<Vec<Value>>, // vector storage
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
            vectors: Vec::new(),
            profiler: Profiler::new_with_enabled(enable_profiling),
        }
    }
    
    pub fn with_all_options(print_stacks: bool, print_stacks_on_call: Option<String>, enable_profiling: bool) -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            program: Vec::new(),
            print_stacks,
            print_stacks_on_call,
            vectors: Vec::new(),
            profiler: Profiler::new_with_enabled(enable_profiling),
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.pc = 0;
    }

    pub fn execute(&mut self) -> Result<i64, String> {
        // Initialize BP to point to the start of the stack
        self.bp = 0;

        while self.pc < self.program.len() {
            let instruction = &self.program[self.pc];

            // Print instruction and stack state if enabled
            if self.print_stacks {
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
            
            // Start timing if profiling is enabled
            let timer = InstructionTimer::start(self.profiler.enabled);

            match instruction {
                Instruction::Label(_) => {
                    // Labels are just markers, no action needed
                }

                Instruction::Push(value) => {
                    self.stack.push(Value::Number(*value as f64));
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(a + b));
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(a - b));
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Number(a * b));
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
                        (Value::Number(a), Value::Number(b)) => {
                            if b == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            self.stack.push(Value::Number(a / b));
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
                        (Value::Number(a), Value::Number(b)) => {
                            if b == 0.0 {
                                return Err("Modulo by zero".to_string());
                            }
                            self.stack.push(Value::Number(a % b));
                        }
                        _ => return Err("Modulo operation requires numbers".to_string()),
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
                        (Value::Number(a), Value::Number(b)) => {
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Boolean(a <= b));
                        }
                        _ => {
                            return Err("Less than or equal operation requires numbers".to_string())
                        }
                    }
                }

                Instruction::Gt => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Gt".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
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
                        (Value::Number(a), Value::Number(b)) => {
                            self.stack.push(Value::Boolean(a >= b));
                        }
                        _ => {
                            return Err(
                                "Greater than or equal operation requires numbers".to_string()
                            )
                        }
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
                        Value::Number(n) => {
                            self.stack.push(Value::Boolean(n == 0.0));
                        }
                        _ => return Err("Not operation requires boolean or number".to_string()),
                    }
                }

                Instruction::Jump(addr) => {
                    self.pc = *addr;
                    continue;
                }

                Instruction::JumpIfZero(addr) => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for JumpIfZero".to_string());
                    }
                    let value = self.stack.pop().unwrap();
                    let should_jump = match value {
                        Value::Number(n) => n == 0.0,
                        Value::Boolean(b) => !b,
                        _ => false,
                    };
                    if should_jump {
                        self.pc = *addr;
                        continue;
                    }
                }

                Instruction::GetLocal(offset) => {
                    let index = if *offset < 0 {
                        // Negative offset: access parameters (before BP)
                        let abs_offset = (-offset) as usize;
                        if self.bp + 1 < abs_offset {
                            return Err(format!(
                                "Parameter access out of bounds: BP={}, offset={}",
                                self.bp, offset
                            ));
                        }
                        self.bp + 1 - abs_offset
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
                        if self.bp + 1 < abs_offset {
                            return Err(format!(
                                "Parameter access out of bounds: BP={}, offset={}",
                                self.bp, offset
                            ));
                        }
                        self.bp + 1 - abs_offset
                    } else {
                        // Positive offset: access local variables (after BP)
                        self.bp + (*offset as usize)
                    };

                    // Extend stack if needed for positive offsets
                    while self.stack.len() <= index {
                        self.stack.push(Value::Number(0.0));
                    }

                    self.stack[index] = value;
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
                    
                    // Function calling convention:
                    // 1. Arguments are already on stack
                    // 2. Push return address (PC + 1)
                    // 3. Push old BP
                    // 4. Set new BP to (stack.len() - 2)
                    // 5. Jump to function

                    // Find function address by name
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
                        self.stack.push(Value::Number((self.pc + 1) as f64)); // Return address
                        self.stack.push(Value::Number(self.bp as f64)); // Old BP
                                                                        // BPは[引数...][return_addr][old_bp]のold_bpを指す
                        self.bp = self.stack.len() - 1;
                        self.pc = addr;
                        continue;
                    } else {
                        return Err(format!("Function not found: {}", func_name));
                    }
                }

                Instruction::Ret => {
                    // 関数リターン時のスタックレイアウト:
                    // [args...] [return_addr] [old_bp] <- BP
                    //                         ^BP

                    if self.bp < 1 || self.stack.len() < self.bp + 1 {
                        return Err("Stack underflow in function return".to_string());
                    }

                    // Get return value from top of stack
                    let return_value = if self.stack.len() > self.bp + 1 {
                        self.stack.pop().unwrap_or(Value::Number(0.0))
                    } else {
                        Value::Number(0.0)
                    };

                    // Get old BP and return address from stack frame
                    let old_bp = match self.stack[self.bp] {
                        Value::Number(n) => n as usize,
                        _ => return Err("Invalid BP value in stack frame".to_string()),
                    };
                    let return_addr = match self.stack[self.bp - 1] {
                        Value::Number(n) => n as usize,
                        _ => return Err("Invalid return address in stack frame".to_string()),
                    };

                    // Check if this is main function return (return address is 1)
                    if return_addr == 1 {
                        // Main function returned, exit VM and return the value
                        match return_value {
                            Value::Number(n) => return Ok(n as i64),
                            Value::Boolean(b) => return Ok(if b { 1 } else { 0 }),
                            _ => return Ok(0),
                        }
                    }


                    // Restore stack to before function call (but keep arguments for now)
                    self.stack.truncate(self.bp - 1);
                    self.bp = old_bp;

                    // Push return value back onto stack
                    self.stack.push(return_value);

                    // Jump to return address
                    self.pc = return_addr;
                    continue;
                }

                Instruction::GetBP => {
                    self.stack.push(Value::Number(self.bp as f64));
                }

                Instruction::SetBP => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for SetBP".to_string());
                    }
                    let value = self.stack.pop().unwrap();
                    match value {
                        Value::Number(n) => {
                            self.bp = n as usize;
                        }
                        _ => return Err("SetBP requires a number".to_string()),
                    }
                }

                Instruction::GetSP => {
                    self.stack.push(Value::Number(self.stack.len() as f64));
                }

                Instruction::SetSP => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for SetSP".to_string());
                    }
                    let value = self.stack.pop().unwrap();
                    match value {
                        Value::Number(n) => {
                            let new_sp = n as usize;
                            if new_sp > self.stack.len() {
                                // Extend stack
                                self.stack.resize(new_sp, Value::Number(0.0));
                            } else {
                                // Truncate stack
                                self.stack.truncate(new_sp);
                            }
                        }
                        _ => return Err("SetSP requires a number".to_string()),
                    }
                }

                Instruction::Nop => {
                    // Do nothing
                }

                Instruction::VectorNew => {
                    // Create a new empty vector and push its index
                    let vector_index = self.vectors.len();
                    self.vectors.push(Vec::new());
                    self.stack.push(Value::Number(vector_index as f64));
                }

                Instruction::VectorPush => {
                    // Stack: [value] [vector_index]
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for VectorPush".to_string());
                    }
                    let vector_index_value = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    match vector_index_value {
                        Value::Number(vector_index) => {
                            let vector_index = vector_index as usize;
                            if vector_index >= self.vectors.len() {
                                return Err(format!("Invalid vector index: {}", vector_index));
                            }
                            self.vectors[vector_index].push(value);
                        }
                        _ => {
                            return Err("VectorPush requires a number for vector index".to_string())
                        }
                    }
                }

                Instruction::VectorIndex => {
                    // Stack: [vector_index] [element_index]
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for VectorIndex".to_string());
                    }
                    let element_index_value = self.stack.pop().unwrap();
                    let vector_index_value = self.stack.pop().unwrap();
                    match (vector_index_value, element_index_value) {
                        (Value::Number(vector_index), Value::Number(element_index)) => {
                            let vector_index = vector_index as usize;
                            let element_index = element_index as usize;
                            if vector_index >= self.vectors.len() {
                                return Err(format!("Invalid vector index: {}", vector_index));
                            }
                            if element_index >= self.vectors[vector_index].len() {
                                return Err(format!("Invalid element index: {}", element_index));
                            }
                            let value = self.vectors[vector_index][element_index].clone();
                            self.stack.push(value);
                        }
                        _ => {
                            return Err("VectorIndex requires numbers for both indices".to_string())
                        }
                    }
                }

                Instruction::VectorSet => {
                    // Stack: [value] [element_index] [vector_index]
                    if self.stack.len() < 3 {
                        return Err("Stack underflow for VectorSet".to_string());
                    }
                    let vector_index_value = self.stack.pop().unwrap();
                    let element_index_value = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    match (vector_index_value, element_index_value) {
                        (Value::Number(vector_index), Value::Number(element_index)) => {
                            let vector_index = vector_index as usize;
                            let element_index = element_index as usize;
                            if vector_index >= self.vectors.len() {
                                return Err(format!("Invalid vector index: {}", vector_index));
                            }
                            if element_index >= self.vectors[vector_index].len() {
                                return Err(format!("Invalid element index: {}", element_index));
                            }
                            self.vectors[vector_index][element_index] = value;
                        }
                        _ => return Err("VectorSet requires numbers for both indices".to_string()),
                    }
                }
            }
            
            // Record profiling data if enabled
            if let Some(elapsed) = timer.finish() {
                let instruction_name = format!("{:?}", instruction).split('(').next().unwrap_or("Unknown").to_string();
                self.profiler.record_instruction(instruction_name, elapsed);
                
                // Special handling for Call instructions
                if let Instruction::Call(func_name) = instruction {
                    self.profiler.record_function_call(func_name.clone());
                }
            }

            self.pc += 1;
        }

        // Program ended, return top of stack or 0
        if self.stack.is_empty() {
            Ok(0)
        } else {
            let value = self.stack.pop().unwrap();
            match value {
                Value::Number(n) => Ok(n as i64),
                Value::Boolean(b) => Ok(if b { 1 } else { 0 }),
                _ => Ok(0),
            }
        }
    }

    /// Reset the VM state for a fresh execution
    pub fn reset(&mut self) {
        self.stack.clear();
        self.pc = 0;
        self.bp = 0;
        self.sp = 0;
        // Keep print_stacks and profiler settings unchanged
        
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
}

/// Compiler for generating VM bytecode from AST
pub struct VMCompiler {
    instructions: Vec<Instruction>,
    local_vars: HashMap<String, i32>,
    local_offset: i32,
}

impl VMCompiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            local_vars: HashMap::new(),
            local_offset: 0,
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
        // 1. 関数を定義順に収集
        let mut functions: Vec<Function> = Vec::new();
        for decl in &program.declarations {
            match decl {
                Decl::Function(func) => {
                    functions.push(func.clone());
                }
            }
        }

        // 2. プログラムの先頭に call main; ret を挿入
        self.instructions.clear();
        self.local_vars.clear();
        self.local_offset = 0;
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

        // Function prologue - reserve space for local variables
        // (This is simplified - in a real implementation we'd analyze the function first)
        self.local_offset = 0;
        self.local_vars.clear();

        // Map parameters to stack positions (negative offsets from BP)
        // Stack layout: [arg0] [arg1] ... [return_addr] [old_bp] <- BP points here
        // Parameters are accessed with negative offsets from BP
        // First param is at BP-4, second at BP-3, etc.
        for (i, param) in func.params.iter().enumerate() {
            let param_offset = -(func.params.len() as i32 - i as i32 + 2);
            self.local_vars.insert(param.name.clone(), param_offset);
        }

        // Compile function body
        for stmt in &func.body {
            self.compile_statement(stmt);
        }

        // Compile return expression if present
        if let Some(return_expr) = &func.return_expr {
            self.compile_expression(return_expr);
        } else {
            // Default return value
            self.instructions.push(Instruction::Push(0));
        }

        // Function epilogue
        if func.name == "main" {
            // For main function, just return the value without using function return convention
            // The VM will naturally exit when it reaches the end of the program
        } else {
            self.instructions.push(Instruction::Ret);
        }

        func_start
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
                self.compile_expression(expr);
                self.instructions.push(Instruction::Ret);
            }

            Stmt::Assign { name, value } => {
                self.compile_expression(value);
                if let Some(&offset) = self.local_vars.get(name) {
                    self.instructions.push(Instruction::SetLocal(offset));
                } else {
                    // This should be caught by type checker
                    panic!("Undefined variable: {}", name);
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

            Stmt::VectorAssign {
                vector,
                index,
                value,
            } => {
                // vector[index] = value
                // value, index, vectorの順でpushし、VectorSet命令
                self.compile_expression(value);
                self.compile_expression(index);
                if let Some(&offset) = self.local_vars.get(vector) {
                    self.instructions.push(Instruction::GetLocal(offset));
                } else {
                    panic!("Undefined vector variable: {}", vector);
                }
                self.instructions.push(Instruction::VectorSet);
            }
        }
    }

    /// Compile an expression to VM bytecode
    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Number(value) => {
                self.instructions.push(Instruction::Push(*value as i64));
            }

            Expr::Boolean(value) => {
                self.instructions
                    .push(Instruction::Push(if *value { 1 } else { 0 }));
            }

            Expr::Identifier(name) => {
                if let Some(&offset) = self.local_vars.get(name) {
                    self.instructions.push(Instruction::GetLocal(offset));
                } else {
                    // This should be caught by type checker
                    panic!("Undefined variable: {}", name);
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
                // Push arguments onto stack (left to right)
                for arg in args {
                    self.compile_expression(arg);
                }

                // Generate call instruction
                if let Expr::Identifier(func_name) = callee.as_ref() {
                    self.instructions.push(Instruction::Call(func_name.clone()));

                    // Note: arguments will be cleaned up by the Call instruction implementation
                    // The return value will be left on the stack
                } else {
                    panic!("Function calls with complex callees not supported yet");
                }
            }

            Expr::VectorNew { .. } => {
                // 現状型情報は無視し、空ベクターをpush
                self.instructions.push(Instruction::VectorNew);
            }

            Expr::VectorIndex { vector, index } => {
                // vector, indexの順でpushし、VectorIndex命令
                self.compile_expression(vector);
                self.compile_expression(index);
                self.instructions.push(Instruction::VectorIndex);
            }

            // TODO: Implement other expression types
            _ => {
                self.instructions.push(Instruction::Push(0)); // Placeholder
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
        Expr::Number(value) => {
            instructions.push(Instruction::Push(*value as i64));
        }

        Expr::Boolean(value) => {
            instructions.push(Instruction::Push(if *value { 1 } else { 0 }));
        }

        Expr::String(_value) => {
            // For now, just push 0 - strings need more complex handling
            instructions.push(Instruction::Push(0));
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

        Expr::Call { .. } => {
            // For now, just push 0 - function calls need more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::VectorNew { .. } => {
            // For now, just push 0 - vectors need more complex handling
            instructions.push(Instruction::Push(0));
        }

        Expr::VectorIndex { .. } => {
            // For now, just push 0 - vector indexing needs more complex handling
            instructions.push(Instruction::Push(0));
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
        let expr = Expr::Number(42.0);
        let instructions = compile_expression(&expr);
        assert_eq!(instructions, vec![Instruction::Push(42)]);

        // Test boolean
        let expr = Expr::Boolean(true);
        let instructions = compile_expression(&expr);
        assert_eq!(instructions, vec![Instruction::Push(1)]);

        // Test binary expression: 2 + 3
        let expr = Expr::Binary {
            left: Box::new(Expr::Number(2.0)),
            op: BinaryOp::Add,
            right: Box::new(Expr::Number(3.0)),
        };
        let instructions = compile_expression(&expr);
        assert_eq!(
            instructions,
            vec![Instruction::Push(2), Instruction::Push(3), Instruction::Add,]
        );

        // Test complex expression: (2 + 3) * 4
        let expr = Expr::Binary {
            left: Box::new(Expr::Binary {
                left: Box::new(Expr::Number(2.0)),
                op: BinaryOp::Add,
                right: Box::new(Expr::Number(3.0)),
            }),
            op: BinaryOp::Multiply,
            right: Box::new(Expr::Number(4.0)),
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
    fn test_ast_to_vm_execution() {
        use crate::ast::{BinaryOp, Expr};

        // Test 2 + 3 * 4 (should be 14 with proper precedence)
        let expr = Expr::Binary {
            left: Box::new(Expr::Number(2.0)),
            op: BinaryOp::Add,
            right: Box::new(Expr::Binary {
                left: Box::new(Expr::Number(3.0)),
                op: BinaryOp::Multiply,
                right: Box::new(Expr::Number(4.0)),
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
            params: vec![],
            body: vec![],
            return_expr: Some(Box::new(Expr::Number(42.0))),
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
            body: vec![],
            return_expr: Some(Box::new(Expr::Binary {
                left: Box::new(Expr::Identifier("x".to_string())),
                op: BinaryOp::Add,
                right: Box::new(Expr::Identifier("y".to_string())),
            })),
        };

        // Create main function: fun main() do return add(2, 3); end
        let main_func = Function {
            name: "main".to_string(),
            params: vec![],
            body: vec![],
            return_expr: Some(Box::new(Expr::Call {
                callee: Box::new(Expr::Identifier("add".to_string())),
                args: vec![Expr::Number(2.0), Expr::Number(3.0)],
            })),
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

        let mut vm = VM::new();
        vm.load_program(instructions);
        let result = vm.execute().unwrap();
        assert_eq!(result, 5);
    }

    #[test]
    fn dump_ir_for_vector_program() {
        use crate::lexer::Lexer;
        use crate::parser::Parser;
        use crate::vm::VMCompiler;
        use std::fs;

        let path = "tests/testcase/program/vector_program.ob";
        let ir_path = "target/vector_program.ir";
        let code = fs::read_to_string(path).expect("failed to read vector_program.ob");
        let mut lexer = Lexer::new(&code);
        let tokens = lexer.tokenize().expect("tokenize failed");
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().expect("parse_program failed");
        let mut compiler = VMCompiler::new();
        compiler.compile_program(&program);
        compiler.dump_ir_to_file(ir_path).expect("dump_ir failed");
        println!("IR dumped to {}", ir_path);
    }
}
