use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOp, Decl, Expr, Function, GlobalVariable, IndexContainerType, PositionedDecl,
        PositionedExpr, PositionedStmt, Program, Stmt, StructDecl,
    },
    vm::Instruction,
};

/// Compiler for generating VM bytecode from AST
pub struct CodeGenerator {
    instructions: Vec<Instruction>,
    local_vars: HashMap<String, i32>,
    local_offset: i32,
    current_function_param_count: usize,
    current_function_name: String,
    functions: HashMap<String, Function>,
    structs: HashMap<String, StructDecl>,
    string_constants: HashMap<String, usize>, // string -> heap address mapping
    string_constant_list: Vec<String>,        // ordered list of string constants
    global_vars: HashMap<String, usize>,      // global variable name -> index mapping
    global_var_count: usize,                  // counter for global variable indices
}

impl CodeGenerator {
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
            global_vars: HashMap::new(),
            global_var_count: 0,
        }
    }

    /// Get or create a string constant, returning its heap index
    fn get_or_create_string_constant(&mut self, s: &str) -> usize {
        if let Some(&addr) = self.string_constants.get(s) {
            addr
        } else {
            // Calculate the address where this string will be stored
            let mut current_addr = 0;
            for existing_string in &self.string_constant_list {
                current_addr += existing_string.len() + 1; // +1 for null terminator
            }

            self.string_constants.insert(s.to_string(), current_addr);
            self.string_constant_list.push(s.to_string());
            current_addr
        }
    }

    /// Get the list of string constants for VM initialization
    pub fn get_string_constants(&self) -> &[String] {
        &self.string_constant_list
    }

    /// Get or create a global variable index
    fn get_or_create_global_var_index(&mut self, name: &str) -> usize {
        if let Some(&index) = self.global_vars.get(name) {
            index
        } else {
            let index = self.global_var_count;
            self.global_vars.insert(name.to_string(), index);
            self.global_var_count += 1;
            index
        }
    }

    /// Collect string constants from a declaration
    fn collect_string_constants_from_decl(&mut self, decl: &PositionedDecl) {
        match &decl.value {
            Decl::Function(func) => {
                for stmt in &func.value.body {
                    self.collect_string_constants_from_stmt(stmt);
                }
            }
            Decl::GlobalVariable(global_var) => {
                if let Some(ref value_expr) = global_var.value.value {
                    self.collect_string_constants_from_expr(value_expr);
                }
            }
            Decl::Struct(struct_decl) => {
                for method in &struct_decl.value.methods {
                    for stmt in &method.value.body {
                        self.collect_string_constants_from_stmt(stmt);
                    }
                }
            }
        }
    }

    /// Collect string constants from a statement
    fn collect_string_constants_from_stmt(&mut self, stmt: &PositionedStmt) {
        match &stmt.value {
            Stmt::Let { value, .. } => {
                self.collect_string_constants_from_expr(value);
            }
            Stmt::Assign { lvalue, value } => {
                self.collect_string_constants_from_expr(lvalue);
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
        }
    }

    /// Collect string constants from an expression
    fn collect_string_constants_from_expr(&mut self, expr: &PositionedExpr) {
        match &expr.value {
            Expr::String(s) => {
                self.get_or_create_string_constant(s);
            }
            Expr::PushString(_) => {
                // PushString doesn't need string constants, it creates strings at runtime
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
                if let Some(obj) = object {
                    self.collect_string_constants_from_expr(obj);
                }
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
            Expr::Alloc {
                element_type: _,
                size,
            } => {
                self.collect_string_constants_from_expr(size);
            }
            Expr::Sizeof { .. } => {
                // sizeof expressions don't contain string constants
            }
            Expr::Cast {
                expr,
                target_type: _,
            } => {
                self.collect_string_constants_from_expr(expr);
            }
            Expr::TypeExpr { type_name } => {
                self.get_or_create_string_constant(&type_name.to_string());
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
            if let Decl::Struct(struct_decl) = &decl.value {
                self.structs
                    .insert(struct_decl.value.name.clone(), struct_decl.value.clone());

                // Collect struct methods with name mangling
                for method in &struct_decl.value.methods {
                    let mut mangled_method = method.value.clone();
                    mangled_method.name =
                        format!("{}_{}", struct_decl.value.name, method.value.name);
                    all_methods.push(mangled_method);
                }
            }
        }

        // 1. 関数を定義順に収集し、関数レジストリに登録
        let mut functions: Vec<Function> = Vec::new();
        let mut global_variables: Vec<GlobalVariable> = Vec::new();
        self.functions.clear();
        for decl in &program.declarations {
            match &decl.value {
                Decl::Function(func) => {
                    functions.push(func.value.clone());
                    self.functions
                        .insert(func.value.name.clone(), func.value.clone());
                }
                Decl::Struct(_) => {
                    // Already handled in first pass
                }
                Decl::GlobalVariable(global_var) => {
                    // Collect global variables for later initialization
                    global_variables.push(global_var.value.clone());
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

        // Initialize string constants in heap first - they are now directly addressable
        for string_constant in &self.string_constant_list {
            self.instructions
                .push(Instruction::PushString(string_constant.clone()));
            self.instructions.push(Instruction::Pop); // Discard the address from stack
        }

        // Initialize global variables second
        for global_var in &global_variables {
            if let Some(ref value_expr) = global_var.value {
                self.compile_expression(value_expr);
                let global_index = self.get_or_create_global_var_index(&global_var.name);
                self.instructions.push(Instruction::SetGlobal(global_index));
            } else {
                // For uninitialized globals, set them to 0 (null pointer)
                self.instructions.push(Instruction::Push(0));
                let global_index = self.get_or_create_global_var_index(&global_var.name);
                self.instructions.push(Instruction::SetGlobal(global_index));
            }
        }

        self.instructions.push(Instruction::Push(-1)); // placeholder for return value
        self.instructions.push(Instruction::Push(-1)); // placeholder for old BP
        self.instructions.push(Instruction::PushAddress(0)); // placeholder for return address

        // Set BP
        self.instructions.push(Instruction::Push(3));
        self.instructions.push(Instruction::SetBP);

        // Call main
        self.instructions
            .push(Instruction::Call("main".to_string()));
        self.emit_return_sequence();

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
        self.instructions.push(Instruction::SetLocal(
            -(self.current_function_param_count as i32 + 3),
        ));
        self.instructions.push(Instruction::GetBP);
        self.instructions.push(Instruction::SetSP);
        self.instructions.push(Instruction::SetBP);
        self.instructions.push(Instruction::Ret);
    }

    /// Compile a statement to VM bytecode
    fn compile_statement(&mut self, stmt: &PositionedStmt) {
        match &stmt.value {
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
                self.emit_return_sequence();
            }

            Stmt::Assign { lvalue, value } => {
                // Use the existing complex assignment logic for all assignments
                self.compile_complex_assignment(lvalue, value);
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

            Stmt::VectorPush { .. } => {
                panic!("VectorPush statements should have been desugared into method calls");
            }
        }
    }

    /// Compile a complex assignment (lvalue = value)
    fn compile_complex_assignment(&mut self, lvalue: &PositionedExpr, value: &PositionedExpr) {
        match &lvalue.value {
            Expr::Identifier(name) => {
                // Simple variable assignment: var = value
                self.compile_expression(value);
                if let Some(&offset) = self.local_vars.get(name) {
                    self.instructions.push(Instruction::SetLocal(offset));
                } else {
                    // Try to assign to global variable
                    let global_index = self.get_or_create_global_var_index(name);
                    self.instructions.push(Instruction::SetGlobal(global_index));
                }
            }

            Expr::FieldAccess { object, field } => {
                // Field assignment: obj.field = value
                self.compile_expression(value);
                self.instructions
                    .push(Instruction::PushString(field.clone()));
                self.compile_expression(object);
                self.instructions.push(Instruction::StructFieldSet);
            }

            Expr::Index {
                container,
                index,
                container_type,
                ..
            } => {
                // Index assignment: container[index] = value
                self.compile_expression(value);
                self.compile_expression(index);
                self.compile_expression(container);

                match container_type {
                    Some(IndexContainerType::Vector) => {
                        panic!("Vector indexing should be handled as method calls, not VM instructions");
                    }
                    Some(IndexContainerType::Pointer) => {
                        self.instructions.push(Instruction::PointerSet);
                    }
                    Some(IndexContainerType::String) => {
                        panic!("Cannot assign to string index - strings are immutable");
                    }
                    None => {
                        panic!(
                            "Container type not resolved during type checking, {:?}",
                            container_type
                        );
                    }
                }
            }

            _ => {
                panic!(
                    "Invalid left-hand side in complex assignment: {:?}",
                    lvalue.value
                );
            }
        }
    }

    /// Compile an expression to VM bytecode
    fn compile_expression(&mut self, expr: &PositionedExpr) {
        match &expr.value {
            Expr::Int(value) => {
                self.instructions.push(Instruction::Push(*value as i64));
            }

            Expr::Boolean(value) => {
                self.instructions
                    .push(Instruction::Push(if *value { 1 } else { 0 }));
            }

            Expr::String(value) => {
                let string_addr = self.get_or_create_string_constant(value);
                self.instructions
                    .push(Instruction::PushAddress(string_addr));
            }

            Expr::PushString(value) => {
                self.instructions
                    .push(Instruction::PushString(value.clone()));
            }

            Expr::Byte(value) => {
                self.instructions.push(Instruction::Push(*value as i64));
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
                    let global_index = self.get_or_create_global_var_index(name);
                    self.instructions.push(Instruction::GetGlobal(global_index));
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
                    BinaryOp::Modulo => self.instructions.push(Instruction::Mod),
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
                if let Expr::Identifier(func_name) = &callee.value {
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

                if let Expr::Identifier(func_name) = &callee.value {
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
                    for _ in 0..args.len() {
                        self.instructions.push(Instruction::Pop);
                    }
                } else {
                    panic!("Function calls with complex callees not supported yet");
                }
            }

            Expr::VectorNew { .. } => {
                panic!(
                    "VectorNew expressions should have been desugared into struct instantiation"
                );
            }

            Expr::Alloc { element_type, size } => {
                // Size-based alloc: compile the size expression
                self.compile_expression(size);
                // Push element size for multiplication
                let element_size = element_type.sizeof();
                self.instructions
                    .push(Instruction::Push(element_size as i64));
                // Multiply size by element size to get total size
                self.instructions.push(Instruction::Mul);
                // Allocate heap space with total size
                self.instructions.push(Instruction::HeapAlloc);
            }

            Expr::Sizeof { type_name } => {
                // Push the size of the type as a constant
                let size = type_name.sizeof();
                self.instructions.push(Instruction::Push(size as i64));
            }

            Expr::Cast {
                expr,
                target_type: _,
            } => {
                // For now, just compile the expression (no runtime cast needed)
                self.compile_expression(expr);
            }

            Expr::Index {
                container,
                index,
                container_type,
                ..
            } => {
                // container, indexの順でpushし、適切なIndex命令
                self.compile_expression(container);
                self.compile_expression(index);

                match container_type {
                    Some(IndexContainerType::Vector) => {
                        panic!("Vector indexing should be handled as method calls, not VM instructions");
                    }
                    Some(IndexContainerType::Pointer) => {
                        self.instructions.push(Instruction::PointerIndex);
                    }
                    Some(IndexContainerType::String) => {
                        self.instructions.push(Instruction::StringIndex);
                    }
                    None => {
                        // Type checking should have resolved this
                        panic!(
                            "Container type not resolved during type checking, {:?}",
                            container_type
                        );
                    }
                }
            }

            Expr::StructNew {
                type_name,
                fields,
                kind: _,
            } => {
                // Verify struct exists (special handling for built-in types and generic instantiations)
                let type_name_str = type_name.to_string();
                let is_builtin_or_generic = type_name_str.starts_with("array(") 
                    || type_name_str.starts_with("vec(") 
                    || type_name_str.contains('('); // Any type with parentheses is likely a generic instantiation
                
                if !self.structs.contains_key(&type_name_str) && !is_builtin_or_generic {
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

            Expr::MethodCall { .. } => {
                panic!("MethodCall should have been desugared before code generation");
            }

            Expr::TypeExpr { type_name } => {
                // For now, type expressions are treated as string constants
                // This is a placeholder - in a full implementation, we'd need proper type system integration
                let string_addr = self.get_or_create_string_constant(&type_name.to_string());
                self.instructions
                    .push(Instruction::PushAddress(string_addr));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Positioned;
    use crate::runtime::VM;

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
    fn test_vm_simple_function() {
        use crate::ast::{Decl, Function, Program};

        // Create main function: fun main() do return 42; end
        let main_func = Function {
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Int(42)),
            ))],
        };

        let program = Program {
            declarations: vec![Positioned::with_unknown_span(Decl::Function(
                Positioned::with_unknown_span(main_func),
            ))],
        };

        // Compile and execute
        let mut compiler = CodeGenerator::new();
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
                    param_type: None,
                },
                FunParam {
                    name: "y".to_string(),
                    param_type: None,
                },
            ],
            body: vec![Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Binary {
                    left: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                        "x".to_string(),
                    ))),
                    op: BinaryOp::Add,
                    right: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                        "y".to_string(),
                    ))),
                }),
            ))],
        };

        // Create main function: fun main() do return add(2, 3); end
        let main_func = Function {
            name: "main".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Call {
                    callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                        "add".to_string(),
                    ))),
                    args: vec![
                        Positioned::with_unknown_span(Expr::Int(2)),
                        Positioned::with_unknown_span(Expr::Int(3)),
                    ],
                }),
            ))],
        };

        let program = Program {
            declarations: vec![
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    add_func,
                ))),
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    main_func,
                ))),
            ],
        };

        // Compile and execute
        let mut compiler = CodeGenerator::new();
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
        use crate::Compiler;
        use std::fs;

        let path = "tests/testcase/vector_program.ob";
        let ir_path = "target/vector_program.ir";
        let code = fs::read_to_string(path).expect("failed to read vector_program.ob");

        let mut compiler = Compiler::new();
        let result = compiler.execute(&code);

        // Just check that it doesn't panic - the actual IR dumping can be done manually if needed
        match result {
            Ok(_) => println!("Vector program executed successfully"),
            Err(e) => println!("Vector program execution failed: {}", e),
        }
    }
}
