use crate::ast::{BinaryOp, Decl, Expr, FunParam, Function, Program, Stmt};
use anyhow::{bail, Result};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Function {
        params: Vec<FunParam>,
        body: Vec<Stmt>,
        return_expr: Option<Box<Expr>>,
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
            Value::Function { .. } => write!(f, "<function>"),
        }
    }
}

pub struct Runtime {
    variables: HashMap<String, Value>,
    call_stack: Vec<HashMap<String, Value>>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            variables: HashMap::new(),
            call_stack: Vec::new(),
        }
    }

    /// Execute a complete program
    pub fn execute_program(&mut self, program: &Program) -> Result<Option<Value>> {
        // First pass: register all function declarations
        for decl in &program.declarations {
            match decl {
                Decl::Function(function) => {
                    self.register_function(function)?;
                }
            }
        }

        // Look for a main function and execute it if found
        if let Some(Value::Function {
            params,
            body,
            return_expr,
        }) = self.variables.get("main").cloned()
        {
            if !params.is_empty() {
                bail!("Main function should not have parameters");
            }

            // Execute main function
            self.call_stack.push(HashMap::new());

            let mut result = None;

            // Execute statements in main function body
            for stmt in &body {
                if let Some(val) = self.execute_stmt(stmt)? {
                    result = Some(val);
                }
            }

            // Execute return expression if present
            if let Some(return_expr) = return_expr {
                result = Some(self.evaluate(&return_expr)?);
            }

            self.call_stack.pop();
            Ok(result)
        } else {
            bail!("No main function found");
        }
    }

    /// Register a function declaration in the global scope
    fn register_function(&mut self, function: &Function) -> Result<()> {
        let function_value = Value::Function {
            params: function.params.clone(),
            body: function.body.clone(),
            return_expr: function.return_expr.clone(),
        };
        self.variables.insert(function.name.clone(), function_value);
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Boolean(value) => Ok(Value::Boolean(*value)),
            Expr::String(value) => Ok(Value::String(value.clone())),
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
            Expr::Binary { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                match (&left_val, &right_val) {
                    (Value::Number(l), Value::Number(r)) => match op {
                        BinaryOp::Add => Ok(Value::Number(l + r)),
                        BinaryOp::Subtract => Ok(Value::Number(l - r)),
                        BinaryOp::Multiply => Ok(Value::Number(l * r)),
                        BinaryOp::Divide => {
                            if *r == 0.0 {
                                bail!("Division by zero");
                            }
                            Ok(Value::Number(l / r))
                        }
                        BinaryOp::Equal => Ok(Value::Boolean(l == r)),
                        BinaryOp::NotEqual => Ok(Value::Boolean(l != r)),
                        BinaryOp::Less => Ok(Value::Boolean(l < r)),
                        BinaryOp::Greater => Ok(Value::Boolean(l > r)),
                        BinaryOp::LessEqual => Ok(Value::Boolean(l <= r)),
                        BinaryOp::GreaterEqual => Ok(Value::Boolean(l >= r)),
                    },
                    (Value::String(l), Value::String(r)) => match op {
                        BinaryOp::Add => Ok(Value::String(format!("{}{}", l, r))),
                        _ => bail!("Unsupported operation {:?} for strings", op),
                    },
                    _ => bail!(
                        "Type mismatch in binary operation: {:?} {:?} {:?}",
                        left_val,
                        op,
                        right_val
                    ),
                }
            }
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
        }
    }
}
