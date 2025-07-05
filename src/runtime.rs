use crate::ast::{BinaryOp, Expr, FunParam, Stmt};
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
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))
            }
            Expr::Binary { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                
                match (&left_val, &right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        let result = match op {
                            BinaryOp::Add => l + r,
                            BinaryOp::Subtract => l - r,
                            BinaryOp::Multiply => l * r,
                            BinaryOp::Divide => {
                                if *r == 0.0 {
                                    bail!("Division by zero");
                                }
                                l / r
                            }
                        };
                        Ok(Value::Number(result))
                    }
                    (Value::String(l), Value::String(r)) => {
                        match op {
                            BinaryOp::Add => Ok(Value::String(format!("{}{}", l, r))),
                            _ => bail!("Unsupported operation {:?} for strings", op),
                        }
                    }
                    _ => bail!("Type mismatch in binary operation: {:?} {:?} {:?}", left_val, op, right_val),
                }
            }
            Expr::Call { callee, args } => {
                let function = self.evaluate(callee)?;
                
                match function {
                    Value::Function { params, body, return_expr } => {
                        if args.len() != params.len() {
                            bail!("Function expects {} arguments, got {}", params.len(), args.len());
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
            Stmt::Fun { name, params, body, return_expr } => {
                let function = Value::Function {
                    params: params.clone(),
                    body: body.clone(),
                    return_expr: return_expr.clone(),
                };
                self.variables.insert(name.clone(), function);
                Ok(None)
            }
            Stmt::Return(expr) => {
                let val = self.evaluate(expr)?;
                Ok(Some(val))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Expr};

    #[test]
    fn test_evaluate_number() {
        let mut runtime = Runtime::new();
        let expr = Expr::Number(42.0);
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_evaluate_binary_operations() {
        let test_cases = vec![
            (2.0, BinaryOp::Add, 3.0, Value::Number(5.0)),
            (5.0, BinaryOp::Subtract, 3.0, Value::Number(2.0)),
            (2.0, BinaryOp::Multiply, 3.0, Value::Number(6.0)),
            (6.0, BinaryOp::Divide, 2.0, Value::Number(3.0)),
        ];

        let mut runtime = Runtime::new();
        for (left, op, right, expected) in test_cases {
            let expr = Expr::binary(Expr::Number(left), op, Expr::Number(right));
            let result = runtime.evaluate(&expr).unwrap();
            assert_eq!(result, expected, "Failed for {} {:?} {}", left, op, right);
        }
    }

    #[test]
    fn test_evaluate_division_by_zero() {
        let mut runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(6.0), BinaryOp::Divide, Expr::Number(0.0));
        let result = runtime.evaluate(&expr);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Division by zero"));
    }

    #[test]
    fn test_evaluate_complex_expressions() {
        let mut runtime = Runtime::new();
        
        // 2 + 3 * 4 = 2 + 12 = 14
        let expr1 = Expr::binary(
            Expr::Number(2.0),
            BinaryOp::Add,
            Expr::binary(Expr::Number(3.0), BinaryOp::Multiply, Expr::Number(4.0))
        );
        let result1 = runtime.evaluate(&expr1).unwrap();
        assert_eq!(result1, Value::Number(14.0));

        // (2 + 3) * 4 = 5 * 4 = 20
        let expr2 = Expr::binary(
            Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0)),
            BinaryOp::Multiply,
            Expr::Number(4.0)
        );
        let result2 = runtime.evaluate(&expr2).unwrap();
        assert_eq!(result2, Value::Number(20.0));
    }

    #[test]
    fn test_function_definition_and_call() {
        let mut runtime = Runtime::new();
        
        // Define function: fun add(x, y) do return x + y; end
        let fun_stmt = Stmt::fun_stmt(
            "add".to_string(),
            vec![
                FunParam::new("x".to_string(), None),
                FunParam::new("y".to_string(), None),
            ],
            vec![],
            Some(Expr::binary(
                Expr::identifier("x".to_string()),
                BinaryOp::Add,
                Expr::identifier("y".to_string())
            ))
        );
        
        runtime.execute_stmt(&fun_stmt).unwrap();
        
        // Call function: add(2, 3)
        let call_expr = Expr::call(
            Expr::identifier("add".to_string()),
            vec![Expr::Number(2.0), Expr::Number(3.0)]
        );
        
        let result = runtime.evaluate(&call_expr).unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn test_function_with_local_variables() {
        let mut runtime = Runtime::new();
        
        // Define function: fun calculate(x) do let temp = x * 2; return temp + 1; end
        let fun_stmt = Stmt::fun_stmt(
            "calculate".to_string(),
            vec![FunParam::new("x".to_string(), None)],
            vec![
                Stmt::let_stmt("temp".to_string(), Expr::binary(
                    Expr::identifier("x".to_string()),
                    BinaryOp::Multiply,
                    Expr::Number(2.0)
                ))
            ],
            Some(Expr::binary(
                Expr::identifier("temp".to_string()),
                BinaryOp::Add,
                Expr::Number(1.0)
            ))
        );
        
        runtime.execute_stmt(&fun_stmt).unwrap();
        
        // Call function: calculate(5)
        let call_expr = Expr::call(
            Expr::identifier("calculate".to_string()),
            vec![Expr::Number(5.0)]
        );
        
        let result = runtime.evaluate(&call_expr).unwrap();
        assert_eq!(result, Value::Number(11.0)); // 5 * 2 + 1 = 11
    }

    #[test]
    fn test_nested_function_calls() {
        let mut runtime = Runtime::new();
        
        // Define function: fun double(x) do return x * 2; end
        let double_stmt = Stmt::fun_stmt(
            "double".to_string(),
            vec![FunParam::new("x".to_string(), None)],
            vec![],
            Some(Expr::binary(
                Expr::identifier("x".to_string()),
                BinaryOp::Multiply,
                Expr::Number(2.0)
            ))
        );
        
        // Define function: fun add_one(x) do return x + 1; end
        let add_one_stmt = Stmt::fun_stmt(
            "add_one".to_string(),
            vec![FunParam::new("x".to_string(), None)],
            vec![],
            Some(Expr::binary(
                Expr::identifier("x".to_string()),
                BinaryOp::Add,
                Expr::Number(1.0)
            ))
        );
        
        runtime.execute_stmt(&double_stmt).unwrap();
        runtime.execute_stmt(&add_one_stmt).unwrap();
        
        // Call nested functions: add_one(double(3))
        let nested_call = Expr::call(
            Expr::identifier("add_one".to_string()),
            vec![Expr::call(
                Expr::identifier("double".to_string()),
                vec![Expr::Number(3.0)]
            )]
        );
        
        let result = runtime.evaluate(&nested_call).unwrap();
        assert_eq!(result, Value::Number(7.0)); // double(3) = 6, add_one(6) = 7
    }
}