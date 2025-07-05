use crate::ast::{BinaryOp, Expr};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
}

impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
        }
    }
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
        }
    }
}

pub struct Runtime;

impl Runtime {
    pub fn new() -> Self {
        Runtime
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Binary { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
                
                let left_num = left_val.as_number();
                let right_num = right_val.as_number();
                
                let result = match op {
                    BinaryOp::Add => left_num + right_num,
                    BinaryOp::Subtract => left_num - right_num,
                    BinaryOp::Multiply => left_num * right_num,
                    BinaryOp::Divide => {
                        if right_num == 0.0 {
                            return Err("Division by zero".to_string());
                        }
                        left_num / right_num
                    }
                };
                
                Ok(Value::Number(result))
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
        assert_eq!(result.unwrap_err(), "Division by zero");
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