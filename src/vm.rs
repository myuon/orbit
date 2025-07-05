use crate::ast::{Expr, BinaryOp};

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
    
    // No operation
    Nop,
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<i32>,
    locals: Vec<i32>,
    pc: usize,           // program counter
    bp: usize,           // base pointer for locals
    program: Vec<Instruction>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            pc: 0,
            bp: 0,
            program: Vec::new(),
        }
    }
    
    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.pc = 0;
    }
    
    pub fn execute(&mut self) -> Result<i32, String> {
        while self.pc < self.program.len() {
            match &self.program[self.pc] {
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
                    let index = (self.bp as i32 + offset) as usize;
                    if index >= self.locals.len() {
                        return Err(format!("Local variable access out of bounds: {}", index));
                    }
                    self.stack.push(self.locals[index]);
                }
                
                Instruction::SetLocal(offset) => {
                    if self.stack.is_empty() {
                        return Err("Stack underflow for SetLocal".to_string());
                    }
                    let value = self.stack.pop().unwrap();
                    let index = (self.bp as i32 + offset) as usize;
                    
                    // Extend locals vector if needed
                    while self.locals.len() <= index {
                        self.locals.push(0);
                    }
                    
                    self.locals[index] = value;
                }
                
                Instruction::Call(_addr) => {
                    // For now, just ignore function calls - will implement later
                    return Err("Function calls not yet implemented".to_string());
                }
                
                Instruction::Ret => {
                    // Return top of stack as result
                    if self.stack.is_empty() {
                        return Ok(0);
                    }
                    return Ok(self.stack.pop().unwrap());
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
        self.locals.clear();
        self.pc = 0;
        self.bp = 0;
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
            (vec![Instruction::Push(2), Instruction::Push(3), Instruction::Add], 5),
            (vec![Instruction::Push(10), Instruction::Push(4), Instruction::Sub], 6),
            (vec![Instruction::Push(3), Instruction::Push(4), Instruction::Mul], 12),
            (vec![Instruction::Push(15), Instruction::Push(3), Instruction::Div], 5),
            (vec![Instruction::Push(10), Instruction::Push(3), Instruction::Mod], 1),
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
            (vec![Instruction::Push(5), Instruction::Push(5), Instruction::Eq], 1),
            (vec![Instruction::Push(3), Instruction::Push(7), Instruction::Eq], 0),
            (vec![Instruction::Push(3), Instruction::Push(7), Instruction::Lt], 1),
            (vec![Instruction::Push(7), Instruction::Push(3), Instruction::Lt], 0),
            (vec![Instruction::Push(3), Instruction::Push(7), Instruction::Lte], 1),
            (vec![Instruction::Push(7), Instruction::Push(7), Instruction::Lte], 1),
            (vec![Instruction::Push(7), Instruction::Push(3), Instruction::Gt], 1),
            (vec![Instruction::Push(3), Instruction::Push(7), Instruction::Gt], 0),
            (vec![Instruction::Push(7), Instruction::Push(3), Instruction::Gte], 1),
            (vec![Instruction::Push(7), Instruction::Push(7), Instruction::Gte], 1),
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
        use crate::ast::{Expr, BinaryOp};
        
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
        assert_eq!(instructions, vec![
            Instruction::Push(2),
            Instruction::Push(3),
            Instruction::Add,
        ]);
        
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
        assert_eq!(instructions, vec![
            Instruction::Push(2),
            Instruction::Push(3),
            Instruction::Add,
            Instruction::Push(4),
            Instruction::Mul,
        ]);
    }

    #[test]
    fn test_ast_to_vm_execution() {
        use crate::ast::{Expr, BinaryOp};
        
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
}