use crate::ast::{BinaryOp, Decl, Expr, Function, Program, Stmt};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Stack operations
    Push(i32),
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

    // Control flow
    Jump(usize),
    JumpIfZero(usize),

    // Local variables
    GetLocal(i32),
    SetLocal(i32),

    // Function calls
    Call(usize),
    Ret,

    // Frame management
    GetBP,
    SetBP,
    GetSP,
    SetSP,

    // No operation
    Nop,
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
            Instruction::Jump(addr) => write!(f, "jump {}", addr),
            Instruction::JumpIfZero(addr) => write!(f, "jump_if_zero {}", addr),
            Instruction::GetLocal(offset) => write!(f, "get_local {}", offset),
            Instruction::SetLocal(offset) => write!(f, "set_local {}", offset),
            Instruction::Call(addr) => write!(f, "call {}", addr),
            Instruction::Ret => write!(f, "ret"),
            Instruction::GetBP => write!(f, "get_bp"),
            Instruction::SetBP => write!(f, "set_bp"),
            Instruction::GetSP => write!(f, "get_sp"),
            Instruction::SetSP => write!(f, "set_sp"),
            Instruction::Nop => write!(f, "nop"),
        }
    }
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<i32>,
    pc: usize, // program counter
    bp: usize, // base pointer for stack frame
    sp: usize, // stack pointer
    program: Vec<Instruction>,
    pub print_stacks: bool, // whether to print stack state during execution
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            program: Vec::new(),
            print_stacks: false,
        }
    }

    pub fn new_with_stack_printing(print_stacks: bool) -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            bp: 0,
            sp: 0,
            program: Vec::new(),
            print_stacks,
        }
    }

    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.pc = 0;
    }

    pub fn execute(&mut self) -> Result<i32, String> {
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

            match instruction {
                Instruction::Push(value) => {
                    self.stack.push(*value);
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
                    self.stack.push(a + b);
                }

                Instruction::Sub => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Sub".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }

                Instruction::Mul => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Mul".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a * b);
                }

                Instruction::Div => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Div".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    if b == 0 {
                        return Err("Division by zero".to_string());
                    }
                    self.stack.push(a / b);
                }

                Instruction::Mod => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Mod".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    if b == 0 {
                        return Err("Modulo by zero".to_string());
                    }
                    self.stack.push(a % b);
                }

                Instruction::Eq => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Eq".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a == b { 1 } else { 0 });
                }

                Instruction::Lt => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Lt".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a < b { 1 } else { 0 });
                }

                Instruction::Lte => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Lte".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a <= b { 1 } else { 0 });
                }

                Instruction::Gt => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Gt".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a > b { 1 } else { 0 });
                }

                Instruction::Gte => {
                    if self.stack.len() < 2 {
                        return Err("Stack underflow for Gte".to_string());
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a >= b { 1 } else { 0 });
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
                    if value == 0 {
                        self.pc = *addr;
                        continue;
                    }
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
                    self.stack.push(self.stack[index]);
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
                        self.stack.push(0);
                    }

                    self.stack[index] = value;
                }

                Instruction::Call(addr) => {
                    // Function calling convention:
                    // 1. Arguments are already on stack
                    // 2. Push return address (PC + 1)
                    // 3. Push old BP
                    // 4. Set new BP to current SP
                    // 5. Jump to function

                    self.stack.push((self.pc + 1) as i32); // Return address
                    self.stack.push(self.bp as i32); // Old BP
                    self.bp = self.stack.len(); // New BP
                    self.pc = *addr;
                    continue;
                }

                Instruction::Ret => {
                    // Function return convention:
                    // Stack layout at function entry:
                    // [args...] [return_addr] [old_bp] [locals...] [return_value]
                    //                         ^BP

                    if self.bp < 2 || self.stack.len() < self.bp {
                        return Err("Stack underflow in function return".to_string());
                    }

                    // Get return value from top of stack
                    let return_value = if self.stack.len() > self.bp {
                        self.stack.pop().unwrap_or(0)
                    } else {
                        0
                    };

                    // Get old BP and return address from stack frame
                    let old_bp = self.stack[self.bp - 1] as usize;
                    let return_addr = self.stack[self.bp - 2] as usize;

                    // Restore stack to before function call (but keep arguments for now)
                    self.stack.truncate(self.bp - 2);
                    self.bp = old_bp;

                    // Push return value back onto stack
                    self.stack.push(return_value);

                    // Jump to return address
                    self.pc = return_addr;
                    continue;
                }

                Instruction::GetBP => {
                    self.stack.push(self.bp as i32);
                }

                Instruction::SetBP => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for SetBP".to_string());
                    }
                    self.bp = self.stack.pop().unwrap() as usize;
                }

                Instruction::GetSP => {
                    self.stack.push(self.stack.len() as i32);
                }

                Instruction::SetSP => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for SetSP".to_string());
                    }
                    let new_sp = self.stack.pop().unwrap() as usize;
                    if new_sp > self.stack.len() {
                        // Extend stack
                        self.stack.resize(new_sp, 0);
                    } else {
                        // Truncate stack
                        self.stack.truncate(new_sp);
                    }
                }

                Instruction::Nop => {
                    // Do nothing
                }
            }

            self.pc += 1;
        }

        // Program ended, return top of stack or 0
        if self.stack.is_empty() {
            Ok(0)
        } else {
            Ok(self.stack.pop().unwrap())
        }
    }

    /// Reset the VM state for a fresh execution
    pub fn reset(&mut self) {
        self.stack.clear();
        self.pc = 0;
        self.bp = 0;
        self.sp = 0;
        // Keep print_stacks setting unchanged
    }
}

/// Compiler for generating VM bytecode from AST
pub struct VMCompiler {
    instructions: Vec<Instruction>,
    function_addresses: HashMap<String, usize>,
    local_vars: HashMap<String, i32>,
    local_offset: i32,
}

impl VMCompiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            function_addresses: HashMap::new(),
            local_vars: HashMap::new(),
            local_offset: 0,
        }
    }

    /// Dump compiled IR to a string
    pub fn dump_ir(&self) -> String {
        let mut output = String::new();
        output.push_str("; Orbit VM IR Dump\n");
        output.push_str("; Generated by Orbit Compiler\n\n");

        // Add function labels as comments
        if !self.function_addresses.is_empty() {
            output.push_str("; Function addresses:\n");
            let mut functions: Vec<_> = self.function_addresses.iter().collect();
            functions.sort_by_key(|(_, &addr)| addr);
            for (name, &addr) in functions {
                output.push_str(&format!("; {}: {}\n", name, addr));
            }
            output.push('\n');
        }

        // Add instructions with line numbers
        for (i, instruction) in self.instructions.iter().enumerate() {
            // Add function labels
            for (name, &addr) in &self.function_addresses {
                if addr == i {
                    output.push_str(&format!("\n{}:\n", name));
                }
            }

            output.push_str(&format!("{:4}: {}\n", i, instruction));
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

        // 2. 関数を定義順にコンパイル
        self.instructions.clear();
        self.function_addresses.clear();
        self.local_vars.clear();
        self.local_offset = 0;

        for func in &functions {
            let func_addr = self.compile_function(func);
            self.function_addresses.insert(func.name.clone(), func_addr);
        }

        self.instructions.clone()
    }

    /// Compile a function to VM bytecode
    pub fn compile_function(&mut self, func: &Function) -> usize {
        let func_start = self.instructions.len();

        // Function prologue - reserve space for local variables
        // (This is simplified - in a real implementation we'd analyze the function first)
        self.local_offset = 0;
        self.local_vars.clear();

        // Map parameters to stack positions (negative offsets from BP)
        // Stack layout: [arg0] [arg1] [return_addr] [old_bp] <- BP points here
        for (i, param) in func.params.iter().enumerate() {
            let param_offset = -2 - (func.params.len() as i32) + (i as i32); // Parameters are before return_addr and old_bp
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

            // TODO: Implement other statement types
            _ => {
                // For now, just ignore unsupported statements
            }
        }
    }

    /// Compile an expression to VM bytecode
    fn compile_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Number(value) => {
                self.instructions.push(Instruction::Push(*value as i32));
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

                // Get function address
                if let Expr::Identifier(func_name) = callee.as_ref() {
                    if let Some(&addr) = self.function_addresses.get(func_name) {
                        self.instructions.push(Instruction::Call(addr));

                        // Note: arguments will be cleaned up by the Call instruction implementation
                        // The return value will be left on the stack
                    } else {
                        panic!("Undefined function: {}", func_name);
                    }
                } else {
                    panic!("Function calls with complex callees not supported yet");
                }
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
            instructions.push(Instruction::Push(*value as i32));
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
                    instructions.push(Instruction::Push(1));
                    instructions.push(Instruction::Sub);
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

        for (program, expected) in test_cases {
            let mut vm = VM::new();
            vm.load_program(program);
            let result = vm.execute().unwrap();
            assert_eq!(result, expected);
        }
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

        for (program, expected) in test_cases {
            let mut vm = VM::new();
            vm.load_program(program);
            let result = vm.execute().unwrap();
            assert_eq!(result, expected);
        }
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

        for inst in &instructions {
            println!("{}", inst);
        }

        let mut vm = VM::new();
        vm.load_program(instructions);
        let result = vm.execute().unwrap();
        assert_eq!(result, 5);
    }
}
