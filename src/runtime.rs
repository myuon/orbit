use crate::ast::{BinaryOp, Expr, FunParam, Program, Stmt};
use crate::vm::{self, VMCompiler, VM};
use anyhow::{bail, Result};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Address(usize),
    Function {
        params: Vec<FunParam>,
        body: Vec<Stmt>,
        return_expr: Option<Box<Expr>>,
    },
    Vector {
        element_type: String,
        elements: Vec<Value>,
    },
    Map {
        key_type: String,
        value_type: String,
        entries: HashMap<String, Value>,
    },
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
            Value::Function { .. } => write!(f, "<function>"),
            Value::Vector { elements, .. } => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Value::Map { entries, .. } => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

pub struct Runtime {
    variables: HashMap<String, Value>,
    call_stack: Vec<HashMap<String, Value>>,
    vm: VM,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            variables: HashMap::new(),
            call_stack: Vec::new(),
            vm: VM::new(),
        }
    }

    pub fn new_with_call_tracing(print_stacks: bool, print_stacks_on_call: Option<String>) -> Self {
        Runtime {
            variables: HashMap::new(),
            call_stack: Vec::new(),
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

    /// Evaluate an expression by compiling to IR and executing on VM (for simple expressions)
    /// or handling complex constructs (functions, vectors) at runtime level
    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            // Handle complex constructs at runtime level
            Expr::Call { callee, args } => {
                let function = self.evaluate(callee)?;

                match function {
                    Value::Function {
                        params,
                        body,
                        return_expr,
                    } => {
                        if args.len() != params.len() {
                            bail!(
                                "Function expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            );
                        }

                        // Create new scope for function execution
                        let mut local_scope = HashMap::new();

                        // Bind arguments to parameters
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_value = self.evaluate(arg)?;
                            local_scope.insert(param.name.clone(), arg_value);
                        }

                        // Execute function body
                        self.call_stack.push(local_scope);

                        let mut result = Value::Number(0.0); // Default return value

                        // Execute statements in function body
                        for stmt in &body {
                            if let Some(val) = self.execute_stmt(stmt)? {
                                result = val;
                            }
                        }

                        // Execute return expression if present
                        if let Some(return_expr) = return_expr {
                            result = self.evaluate(&return_expr)?;
                        }

                        self.call_stack.pop();
                        Ok(result)
                    }
                    _ => bail!("Cannot call non-function value: {:?}", function),
                }
            }

            Expr::Identifier(name) => {
                // Check call stack first (local variables)
                if let Some(locals) = self.call_stack.last() {
                    if let Some(value) = locals.get(name) {
                        return Ok(value.clone());
                    }
                }

                // Then check global variables
                self.variables
                    .get(name)
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))
            }

            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let mut elements = Vec::new();
                for value_expr in initial_values {
                    elements.push(self.evaluate(value_expr)?);
                }
                Ok(Value::Vector {
                    element_type: element_type.clone(),
                    elements,
                })
            }

            Expr::VectorIndex { vector, index } => {
                let vector_value = self.evaluate(vector)?;
                let index_value = self.evaluate(index)?;

                match (&vector_value, &index_value) {
                    (Value::Vector { elements, .. }, Value::Number(idx)) => {
                        let idx = *idx as usize;
                        if idx < elements.len() {
                            Ok(elements[idx].clone())
                        } else {
                            bail!(
                                "Vector index {} out of bounds (length: {})",
                                idx,
                                elements.len()
                            )
                        }
                    }
                    (Value::Vector { .. }, _) => {
                        bail!("Vector index must be a number")
                    }
                    (Value::Map { entries, .. }, Value::String(key)) => {
                        entries.get(key).cloned().ok_or_else(|| anyhow::anyhow!("Key '{}' not found in map", key))
                    }
                    (Value::Map { .. }, _) => {
                        bail!("Map key must be a string")
                    }
                    _ => bail!("Cannot index non-vector/map value: {:?}", vector_value),
                }
            }

            Expr::MapNew { key_type, value_type, initial_pairs } => {
                let mut entries = HashMap::new();
                for (key_expr, value_expr) in initial_pairs {
                    let key_value = self.evaluate(key_expr)?;
                    let value_value = self.evaluate(value_expr)?;
                    
                    match key_value {
                        Value::String(key) => {
                            entries.insert(key, value_value);
                        }
                        _ => bail!("Map key must be a string"),
                    }
                }
                Ok(Value::Map {
                    key_type: key_type.clone(),
                    value_type: value_type.clone(),
                    entries,
                })
            }

            Expr::String(value) => Ok(Value::String(value.clone())),

            // Binary expressions with variables need to be evaluated at runtime
            Expr::Binary { left, op, right } if self.contains_variables(expr) => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                match (left_val, right_val, op) {
                    (Value::Number(a), Value::Number(b), BinaryOp::Add) => Ok(Value::Number(a + b)),
                    (Value::Number(a), Value::Number(b), BinaryOp::Subtract) => {
                        Ok(Value::Number(a - b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::Multiply) => {
                        Ok(Value::Number(a * b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::Divide) => {
                        if b == 0.0 {
                            bail!("Division by zero")
                        } else {
                            Ok(Value::Number(a / b))
                        }
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::Equal) => {
                        Ok(Value::Boolean(a == b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::NotEqual) => {
                        Ok(Value::Boolean(a != b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::Less) => {
                        Ok(Value::Boolean(a < b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::Greater) => {
                        Ok(Value::Boolean(a > b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::LessEqual) => {
                        Ok(Value::Boolean(a <= b))
                    }
                    (Value::Number(a), Value::Number(b), BinaryOp::GreaterEqual) => {
                        Ok(Value::Boolean(a >= b))
                    }
                    (Value::String(a), Value::String(b), BinaryOp::Add) => {
                        Ok(Value::String(a + &b))
                    }
                    (Value::String(a), Value::String(b), BinaryOp::Equal) => {
                        Ok(Value::Boolean(a == b))
                    }
                    (Value::String(a), Value::String(b), BinaryOp::NotEqual) => {
                        Ok(Value::Boolean(a != b))
                    }
                    (Value::Boolean(a), Value::Boolean(b), BinaryOp::Equal) => {
                        Ok(Value::Boolean(a == b))
                    }
                    (Value::Boolean(a), Value::Boolean(b), BinaryOp::NotEqual) => {
                        Ok(Value::Boolean(a != b))
                    }
                    // Address comparison operations only
                    (Value::Address(a), Value::Address(b), BinaryOp::Equal) => {
                        Ok(Value::Boolean(a == b))
                    }
                    (Value::Address(a), Value::Address(b), BinaryOp::NotEqual) => {
                        Ok(Value::Boolean(a != b))
                    }
                    (Value::Address(a), Value::Address(b), BinaryOp::Less) => {
                        Ok(Value::Boolean(a < b))
                    }
                    (Value::Address(a), Value::Address(b), BinaryOp::Greater) => {
                        Ok(Value::Boolean(a > b))
                    }
                    (Value::Address(a), Value::Address(b), BinaryOp::LessEqual) => {
                        Ok(Value::Boolean(a <= b))
                    }
                    (Value::Address(a), Value::Address(b), BinaryOp::GreaterEqual) => {
                        Ok(Value::Boolean(a >= b))
                    }
                    _ => bail!("Invalid operation for types"),
                }
            }

            // Simple expressions: compile to IR and execute on VM
            _ => {
                // Phase 1: Compile AST to IR (bytecode)
                let instructions = vm::compile_expression(expr);

                // Phase 2: Execute IR on VM
                self.vm.reset();
                self.vm.load_program(instructions);

                match self.vm.execute() {
                    Ok(result) => {
                        // Convert VM result to appropriate Value type
                        if self.is_boolean_expr(expr) {
                            Ok(Value::Boolean(result != 0))
                        } else {
                            Ok(Value::Number(result as f64))
                        }
                    }
                    Err(err) => bail!("VM execution error: {}", err),
                }
            }
        }
    }

    /// Check if an expression should return a boolean value
    fn is_boolean_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Boolean(_) => true,
            Expr::Binary { op, .. } => matches!(
                op,
                BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::Greater
                    | BinaryOp::LessEqual
                    | BinaryOp::GreaterEqual
            ),
            _ => false,
        }
    }

    /// Check if an expression contains variables (identifiers)
    fn contains_variables(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(_) => true,
            Expr::Binary { left, right, .. } => {
                self.contains_variables(left) || self.contains_variables(right)
            }
            Expr::Call { callee, args } => {
                self.contains_variables(callee)
                    || args.iter().any(|arg| self.contains_variables(arg))
            }
            Expr::VectorIndex { vector, index } => {
                self.contains_variables(vector) || self.contains_variables(index)
            }
            Expr::MapNew { initial_pairs, .. } => {
                initial_pairs.iter().any(|(key, value)| {
                    self.contains_variables(key) || self.contains_variables(value)
                })
            }
            _ => false,
        }
    }

    pub fn execute_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>> {
        match stmt {
            Stmt::Let { name, value } => {
                let val = self.evaluate(value)?;
                if let Some(locals) = self.call_stack.last_mut() {
                    locals.insert(name.clone(), val);
                } else {
                    self.variables.insert(name.clone(), val);
                }
                Ok(None)
            }
            Stmt::Expression(expr) => {
                let val = self.evaluate(expr)?;
                Ok(Some(val))
            }
            Stmt::Return(expr) => {
                let val = self.evaluate(expr)?;
                Ok(Some(val))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_val = self.evaluate(condition)?;

                match condition_val {
                    Value::Boolean(true) => {
                        let mut last_value = None;
                        for stmt in then_branch {
                            last_value = self.execute_stmt(stmt)?;
                        }
                        Ok(last_value)
                    }
                    Value::Boolean(false) => {
                        if let Some(else_stmts) = else_branch {
                            let mut last_value = None;
                            for stmt in else_stmts {
                                last_value = self.execute_stmt(stmt)?;
                            }
                            Ok(last_value)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => bail!("Condition must evaluate to a boolean value"),
                }
            }
            Stmt::While { condition, body } => {
                let mut last_value = None;
                loop {
                    let condition_val = self.evaluate(condition)?;

                    match condition_val {
                        Value::Boolean(true) => {
                            for stmt in body {
                                last_value = self.execute_stmt(stmt)?;
                            }
                        }
                        Value::Boolean(false) => break,
                        _ => bail!("While condition must evaluate to a boolean value"),
                    }
                }
                Ok(last_value)
            }
            Stmt::Assign { name, value } => {
                let val = self.evaluate(value)?;

                // Check if variable exists in local scope first
                if let Some(locals) = self.call_stack.last_mut() {
                    if locals.contains_key(name) {
                        locals.insert(name.clone(), val);
                        return Ok(None);
                    }
                }

                // Then check global scope
                if self.variables.contains_key(name) {
                    self.variables.insert(name.clone(), val);
                    Ok(None)
                } else {
                    bail!("Cannot assign to undefined variable: {}", name)
                }
            }
            Stmt::VectorPush { vector, value } => {
                let val = self.evaluate(value)?;

                // Find the vector variable and push to it
                if let Some(locals) = self.call_stack.last_mut() {
                    if let Some(vector_value) = locals.get_mut(vector) {
                        match vector_value {
                            Value::Vector { elements, .. } => {
                                elements.push(val);
                                return Ok(None);
                            }
                            _ => bail!("Cannot push to non-vector value: {}", vector),
                        }
                    }
                }

                // Check global scope
                if let Some(vector_value) = self.variables.get_mut(vector) {
                    match vector_value {
                        Value::Vector { elements, .. } => {
                            elements.push(val);
                            Ok(None)
                        }
                        _ => bail!("Cannot push to non-vector value: {}", vector),
                    }
                } else {
                    bail!("Undefined vector: {}", vector)
                }
            }
            Stmt::VectorAssign {
                vector,
                index,
                value,
            } => {
                let idx_val = self.evaluate(index)?;
                let new_val = self.evaluate(value)?;

                // Find the variable and update element (could be vector or map)
                if let Some(locals) = self.call_stack.last_mut() {
                    if let Some(collection_value) = locals.get_mut(vector) {
                        match (collection_value, &idx_val) {
                            (Value::Vector { elements, .. }, Value::Number(idx)) => {
                                let idx = *idx as usize;
                                if idx < elements.len() {
                                    elements[idx] = new_val;
                                    return Ok(None);
                                } else {
                                    bail!(
                                        "Vector index {} out of bounds (length: {})",
                                        idx,
                                        elements.len()
                                    );
                                }
                            }
                            (Value::Map { entries, .. }, Value::String(key)) => {
                                entries.insert(key.clone(), new_val);
                                return Ok(None);
                            }
                            (Value::Vector { .. }, _) => {
                                bail!("Vector index must be a number")
                            }
                            (Value::Map { .. }, _) => {
                                bail!("Map key must be a string")
                            }
                            _ => bail!("Cannot index non-vector/map value: {}", vector),
                        }
                    }
                }

                // Check global scope
                if let Some(collection_value) = self.variables.get_mut(vector) {
                    match (collection_value, &idx_val) {
                        (Value::Vector { elements, .. }, Value::Number(idx)) => {
                            let idx = *idx as usize;
                            if idx < elements.len() {
                                elements[idx] = new_val;
                                Ok(None)
                            } else {
                                bail!(
                                    "Vector index {} out of bounds (length: {})",
                                    idx,
                                    elements.len()
                                );
                            }
                        }
                        (Value::Map { entries, .. }, Value::String(key)) => {
                            entries.insert(key.clone(), new_val);
                            Ok(None)
                        }
                        (Value::Vector { .. }, _) => {
                            bail!("Vector index must be a number")
                        }
                        (Value::Map { .. }, _) => {
                            bail!("Map key must be a string")
                        }
                        _ => bail!("Cannot index non-vector/map value: {}", vector),
                    }
                } else {
                    bail!("Undefined variable: {}", vector)
                }
            }
            Stmt::MapAssign { map, key, value } => {
                let key_val = self.evaluate(key)?;
                let new_val = self.evaluate(value)?;

                let key_str = match key_val {
                    Value::String(s) => s,
                    _ => bail!("Map key must be a string"),
                };

                // Find the map variable and update element
                if let Some(locals) = self.call_stack.last_mut() {
                    if let Some(map_value) = locals.get_mut(map) {
                        match map_value {
                            Value::Map { entries, .. } => {
                                entries.insert(key_str, new_val);
                                return Ok(None);
                            }
                            _ => bail!("Cannot assign to non-map value: {}", map),
                        }
                    }
                }

                // Check global scope
                if let Some(map_value) = self.variables.get_mut(map) {
                    match map_value {
                        Value::Map { entries, .. } => {
                            entries.insert(key_str, new_val);
                            Ok(None)
                        }
                        _ => bail!("Cannot assign to non-map value: {}", map),
                    }
                } else {
                    bail!("Undefined map: {}", map)
                }
            }
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
