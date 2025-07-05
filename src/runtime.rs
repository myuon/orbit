use crate::ast::{BinaryOp, Expr};
use anyhow::{bail, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
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
        }
    }
}

pub struct Runtime;

impl Runtime {
    pub fn new() -> Self {
        Runtime
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Boolean(value) => Ok(Value::Boolean(*value)),
            Expr::String(value) => Ok(Value::String(value.clone())),
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, Expr};

    #[test]
    fn test_evaluate_number() {
        let runtime = Runtime::new();
        let expr = Expr::Number(42.0);
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(42.0));
    }

    #[test]
    fn test_evaluate_addition() {
        let runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0));
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(5.0));
    }

    #[test]
    fn test_evaluate_subtraction() {
        let runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(5.0), BinaryOp::Subtract, Expr::Number(3.0));
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(2.0));
    }

    #[test]
    fn test_evaluate_multiplication() {
        let runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(2.0), BinaryOp::Multiply, Expr::Number(3.0));
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(6.0));
    }

    #[test]
    fn test_evaluate_division() {
        let runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(6.0), BinaryOp::Divide, Expr::Number(2.0));
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(3.0));
    }

    #[test]
    fn test_evaluate_division_by_zero() {
        let runtime = Runtime::new();
        let expr = Expr::binary(Expr::Number(6.0), BinaryOp::Divide, Expr::Number(0.0));
        let result = runtime.evaluate(&expr);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Division by zero"));
    }

    #[test]
    fn test_evaluate_complex_expression() {
        let runtime = Runtime::new();
        // 2 + 3 * 4 = 2 + 12 = 14
        let expr = Expr::binary(
            Expr::Number(2.0),
            BinaryOp::Add,
            Expr::binary(Expr::Number(3.0), BinaryOp::Multiply, Expr::Number(4.0))
        );
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(14.0));
    }

    #[test]
    fn test_evaluate_parentheses() {
        let runtime = Runtime::new();
        // (2 + 3) * 4 = 5 * 4 = 20
        let expr = Expr::binary(
            Expr::binary(Expr::Number(2.0), BinaryOp::Add, Expr::Number(3.0)),
            BinaryOp::Multiply,
            Expr::Number(4.0)
        );
        let result = runtime.evaluate(&expr).unwrap();
        assert_eq!(result, Value::Number(20.0));
    }
}