use crate::ast::{BinaryOp, Decl, Expr, Function, IndexContainerType, Program, Stmt};
use anyhow::{bail, Result};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Bool,
    Number,
    String,
    Vector {
        element_type: Box<Type>,
    },
    Map {
        key_type: Box<Type>,
        value_type: Box<Type>,
    },
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Bool => write!(f, "bool"),
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Vector { element_type } => write!(f, "vec({})", element_type),
            Type::Map {
                key_type,
                value_type,
            } => write!(f, "map({}, {})", key_type, value_type),
            Type::Function {
                param_types,
                return_type,
            } => {
                write!(f, "(")?;
                for (i, param) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
        }
    }
}

impl Type {
    /// Parse a type string like "int", "vec(int)", "map(string, int)"
    pub fn from_string(type_str: &str) -> Type {
        match type_str {
            "bool" => Type::Bool,
            "int" | "number" => Type::Number,
            "string" | "[*]byte" => Type::String, // Treat [*]byte as string
            "byte" => Type::Number,               // Individual bytes are numbers
            _ => {
                if type_str.starts_with("vec(") && type_str.ends_with(')') {
                    let inner = &type_str[4..type_str.len() - 1];
                    Type::Vector {
                        element_type: Box::new(Type::from_string(inner)),
                    }
                } else if type_str.starts_with("map(") && type_str.ends_with(')') {
                    let inner = &type_str[4..type_str.len() - 1];
                    if let Some(comma_pos) = inner.find(", ") {
                        let key_type = &inner[..comma_pos];
                        let value_type = &inner[comma_pos + 2..];
                        Type::Map {
                            key_type: Box::new(Type::from_string(key_type)),
                            value_type: Box::new(Type::from_string(value_type)),
                        }
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
        }
    }

    /// Check if this type is compatible with another type
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            // Only allow Unknown compatibility during type inference phase
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Vector { element_type: e1 }, Type::Vector { element_type: e2 }) => {
                e1.is_compatible_with(e2)
            }
            (
                Type::Map {
                    key_type: k1,
                    value_type: v1,
                },
                Type::Map {
                    key_type: k2,
                    value_type: v2,
                },
            ) => k1.is_compatible_with(k2) && v1.is_compatible_with(v2),
            _ => false,
        }
    }

    /// Check strict type equality (no Unknown compatibility)
    pub fn is_exactly(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Vector { element_type: e1 }, Type::Vector { element_type: e2 }) => {
                e1.is_exactly(e2)
            }
            (
                Type::Map {
                    key_type: k1,
                    value_type: v1,
                },
                Type::Map {
                    key_type: k2,
                    value_type: v2,
                },
            ) => k1.is_exactly(k2) && v1.is_exactly(v2),
            _ => false,
        }
    }
}

pub struct TypeChecker {
    /// Variable type environment
    variables: HashMap<String, Type>,
    /// Function call stack for type scoping
    call_stack: Vec<HashMap<String, Type>>,
    /// Current function return type
    current_return_type: Option<Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            variables: HashMap::new(),
            call_stack: Vec::new(),
            current_return_type: None,
        }
    }

    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) -> Result<()> {
        for decl in &program.declarations {
            self.check_declaration(decl)?;
        }
        Ok(())
    }

    /// Perform type inference and fill in container types
    pub fn infer_types(&mut self, program: &mut Program) -> Result<()> {
        for decl in &mut program.declarations {
            self.infer_declaration_types(decl)?;
        }
        Ok(())
    }

    fn infer_declaration_types(&mut self, decl: &mut Decl) -> Result<()> {
        match decl {
            Decl::Function(function) => self.infer_function_types(function),
        }
    }

    fn infer_function_types(&mut self, function: &mut Function) -> Result<()> {
        // Create new scope for function
        let mut local_scope = HashMap::new();

        // Add parameters to scope
        for param in &function.params {
            let param_type = if let Some(type_name) = &param.type_name {
                Type::from_string(type_name)
            } else {
                Type::Unknown // Type inference could be added later
            };
            local_scope.insert(param.name.clone(), param_type);
        }

        self.call_stack.push(local_scope);

        // Infer types in function body
        for stmt in &mut function.body {
            self.infer_statement_types(stmt)?;
        }

        self.call_stack.pop();
        Ok(())
    }

    fn infer_statement_types(&mut self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Let { name, value } => {
                self.infer_expression_types(value)?;
                let value_type = self.check_expression(value)?;

                // Add to current scope
                if let Some(locals) = self.call_stack.last_mut() {
                    locals.insert(name.clone(), value_type);
                } else {
                    self.variables.insert(name.clone(), value_type);
                }
                Ok(())
            }

            Stmt::Expression(expr) => {
                self.infer_expression_types(expr)?;
                Ok(())
            }

            Stmt::Return(expr) => {
                self.infer_expression_types(expr)?;
                Ok(())
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.infer_expression_types(condition)?;

                for stmt in then_branch {
                    self.infer_statement_types(stmt)?;
                }

                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.infer_statement_types(stmt)?;
                    }
                }
                Ok(())
            }

            Stmt::While { condition, body } => {
                self.infer_expression_types(condition)?;

                for stmt in body {
                    self.infer_statement_types(stmt)?;
                }
                Ok(())
            }

            Stmt::Assign { name: _, value } => {
                self.infer_expression_types(value)?;
                Ok(())
            }

            Stmt::VectorPush { vector: _, value } => {
                self.infer_expression_types(value)?;
                Ok(())
            }

            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type,
            } => {
                self.infer_expression_types(index)?;
                self.infer_expression_types(value)?;

                // Infer container type based on variable type
                let collection_type = self.lookup_variable(container)?;
                match &collection_type {
                    Type::Vector { .. } => {
                        *container_type = Some(IndexContainerType::Vector);
                    }
                    Type::Map { .. } => {
                        *container_type = Some(IndexContainerType::Map);
                    }
                    _ => {}
                }
                Ok(())
            }
        }
    }

    fn infer_expression_types(&mut self, expr: &mut Expr) -> Result<()> {
        match expr {
            Expr::Number(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Identifier(_) => {
                // No inference needed for literals and identifiers
                Ok(())
            }

            Expr::Binary { left, right, .. } => {
                self.infer_expression_types(left)?;
                self.infer_expression_types(right)?;
                Ok(())
            }

            Expr::Call { callee, args } => {
                self.infer_expression_types(callee)?;
                for arg in args {
                    self.infer_expression_types(arg)?;
                }
                Ok(())
            }

            Expr::VectorNew { initial_values, .. } => {
                for value in initial_values {
                    self.infer_expression_types(value)?;
                }
                Ok(())
            }

            Expr::Index {
                container,
                index,
                container_type,
            } => {
                self.infer_expression_types(container)?;
                self.infer_expression_types(index)?;

                // Infer container type based on container expression type
                let container_value_type = self.check_expression(container)?;
                match &container_value_type {
                    Type::Vector { .. } => {
                        *container_type = Some(IndexContainerType::Vector);
                    }
                    Type::Map { .. } => {
                        *container_type = Some(IndexContainerType::Map);
                    }
                    _ => {}
                }
                Ok(())
            }

            Expr::MapNew { initial_pairs, .. } => {
                for (key, value) in initial_pairs {
                    self.infer_expression_types(key)?;
                    self.infer_expression_types(value)?;
                }
                Ok(())
            }
        }
    }

    /// Type check a declaration
    fn check_declaration(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::Function(function) => self.check_function(function),
        }
    }

    /// Type check a function
    fn check_function(&mut self, function: &Function) -> Result<()> {
        // Create new scope for function
        let mut local_scope = HashMap::new();

        // Add parameters to scope
        for param in &function.params {
            let param_type = if let Some(type_name) = &param.type_name {
                Type::from_string(type_name)
            } else {
                Type::Unknown // Type inference could be added later
            };
            local_scope.insert(param.name.clone(), param_type);
        }

        self.call_stack.push(local_scope);

        // Set return type context
        self.current_return_type = Some(Type::Unknown); // Could be inferred

        // Check function body
        for stmt in &function.body {
            self.check_statement(stmt)?;
        }

        self.call_stack.pop();
        self.current_return_type = None;

        Ok(())
    }

    /// Type check a statement
    fn check_statement(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let { name, value } => {
                let value_type = self.check_expression(value)?;

                // Add to current scope
                if let Some(locals) = self.call_stack.last_mut() {
                    locals.insert(name.clone(), value_type);
                } else {
                    self.variables.insert(name.clone(), value_type);
                }
                Ok(())
            }

            Stmt::Expression(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }

            Stmt::Return(expr) => {
                let expr_type = self.check_expression(expr)?;

                // Check against expected return type if available
                if let Some(expected_return_type) = &self.current_return_type {
                    if !expr_type.is_compatible_with(expected_return_type) {
                        bail!(
                            "Return type mismatch: expected {}, got {}",
                            expected_return_type,
                            expr_type
                        );
                    }
                }
                Ok(())
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_type = self.check_expression(condition)?;
                if !condition_type.is_compatible_with(&Type::Bool) {
                    bail!("If condition must be boolean, got {}", condition_type);
                }

                for stmt in then_branch {
                    self.check_statement(stmt)?;
                }

                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.check_statement(stmt)?;
                    }
                }
                Ok(())
            }

            Stmt::While { condition, body } => {
                let condition_type = self.check_expression(condition)?;
                if !condition_type.is_compatible_with(&Type::Bool) {
                    bail!("While condition must be boolean, got {}", condition_type);
                }

                for stmt in body {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }

            Stmt::Assign { name, value } => {
                let value_type = self.check_expression(value)?;
                let var_type = self.lookup_variable(name)?;

                if !value_type.is_compatible_with(&var_type) {
                    bail!(
                        "Assignment type mismatch: variable '{}' has type {}, assigned {}",
                        name,
                        var_type,
                        value_type
                    );
                }
                Ok(())
            }

            Stmt::VectorPush { vector, value } => {
                let value_type = self.check_expression(value)?;
                let vector_type = self.lookup_variable(vector)?;

                match &vector_type {
                    Type::Vector { element_type } => {
                        if !value_type.is_compatible_with(element_type) {
                            bail!(
                                "Vector push type mismatch: vector element type is {}, pushed {}",
                                element_type,
                                value_type
                            );
                        }
                    }
                    _ => bail!("Cannot push to non-vector type: {}", vector_type),
                }
                Ok(())
            }

            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type: _,
            } => {
                let index_type = self.check_expression(index)?;
                let value_type = self.check_expression(value)?;
                let collection_type = self.lookup_variable(container)?;

                // Just perform type checking, inference will be done separately
                match &collection_type {
                    Type::Vector { element_type } => {
                        if !index_type.is_compatible_with(&Type::Number) {
                            bail!("Vector index must be number, got {}", index_type);
                        }
                        if !value_type.is_compatible_with(element_type) {
                            bail!(
                                "Vector assignment type mismatch: element type is {}, assigned {}",
                                element_type,
                                value_type
                            );
                        }
                    }
                    Type::Map {
                        key_type,
                        value_type: map_value_type,
                    } => {
                        if !index_type.is_compatible_with(key_type) {
                            bail!(
                                "Map key type mismatch: expected {}, got {}",
                                key_type,
                                index_type
                            );
                        }
                        if !value_type.is_compatible_with(map_value_type) {
                            bail!(
                                "Map value type mismatch: expected {}, got {}",
                                map_value_type,
                                value_type
                            );
                        }
                    }
                    _ => bail!("Cannot index non-vector/map type: {}", collection_type),
                }
                Ok(())
            }
        }
    }

    /// Type check an expression and return its type
    fn check_expression(&mut self, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Number(_) => Ok(Type::Number),
            Expr::Boolean(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::String),

            Expr::Identifier(name) => self.lookup_variable(name),

            Expr::Binary { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match op {
                    BinaryOp::Add => {
                        // For string concatenation
                        if left_type.is_compatible_with(&Type::String)
                            && right_type.is_compatible_with(&Type::String)
                        {
                            Ok(Type::String)
                        } else if left_type.is_compatible_with(&Type::Number)
                            && right_type.is_compatible_with(&Type::Number)
                        {
                            Ok(Type::Number)
                        } else {
                            bail!("Addition operation type mismatch: {} + {} (can only add numbers or concatenate strings)", left_type, right_type);
                        }
                    }
                    BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if left_type.is_compatible_with(&Type::Number)
                            && right_type.is_compatible_with(&Type::Number)
                        {
                            Ok(Type::Number)
                        } else {
                            bail!(
                                "Arithmetic operation type mismatch: {} {} {} (requires numbers)",
                                left_type,
                                op_to_string(op),
                                right_type
                            );
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if left_type.is_compatible_with(&right_type) {
                            Ok(Type::Bool)
                        } else {
                            bail!(
                                "Comparison type mismatch: {} and {} are not comparable",
                                left_type,
                                right_type
                            );
                        }
                    }
                    BinaryOp::Less
                    | BinaryOp::Greater
                    | BinaryOp::LessEqual
                    | BinaryOp::GreaterEqual => {
                        if left_type.is_compatible_with(&Type::Number)
                            && right_type.is_compatible_with(&Type::Number)
                        {
                            Ok(Type::Bool)
                        } else {
                            bail!(
                                "Comparison operation requires numbers: {} {} {}",
                                left_type,
                                op_to_string(op),
                                right_type
                            );
                        }
                    }
                }
            }

            Expr::Call { callee, args } => {
                // For now, assume all function calls return unknown type
                // This would need proper function signature lookup
                let _callee_type = self.check_expression(callee)?;
                for arg in args {
                    self.check_expression(arg)?;
                }
                Ok(Type::Unknown) // Function call return type needs proper implementation
            }

            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let element_type = Type::from_string(element_type);

                // Check all initial values match element type
                for value in initial_values {
                    let value_type = self.check_expression(value)?;
                    if !value_type.is_compatible_with(&element_type) {
                        bail!(
                            "Vector initial value type mismatch: expected {}, got {}",
                            element_type,
                            value_type
                        );
                    }
                }

                Ok(Type::Vector {
                    element_type: Box::new(element_type),
                })
            }

            Expr::Index {
                container,
                index,
                container_type: _,
            } => {
                let container_value_type = self.check_expression(container)?;
                let index_type = self.check_expression(index)?;

                // Just perform type checking, inference will be done separately
                match &container_value_type {
                    Type::Vector { element_type } => {
                        if !index_type.is_compatible_with(&Type::Number) {
                            bail!("Vector index must be number, got {}", index_type);
                        }
                        Ok(*element_type.clone())
                    }
                    Type::Map {
                        key_type,
                        value_type,
                    } => {
                        if !index_type.is_compatible_with(key_type) {
                            bail!(
                                "Map key type mismatch: expected {}, got {}",
                                key_type,
                                index_type
                            );
                        }
                        Ok(*value_type.clone())
                    }
                    _ => bail!("Cannot index non-vector/map type: {}", container_value_type),
                }
            }

            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => {
                let key_type = Type::from_string(key_type);
                let value_type = Type::from_string(value_type);

                // Check all initial pairs match key/value types
                for (key_expr, value_expr) in initial_pairs {
                    let actual_key_type = self.check_expression(key_expr)?;
                    let actual_value_type = self.check_expression(value_expr)?;

                    if !actual_key_type.is_compatible_with(&key_type) {
                        bail!(
                            "Map initial key type mismatch: expected {}, got {}",
                            key_type,
                            actual_key_type
                        );
                    }
                    if !actual_value_type.is_compatible_with(&value_type) {
                        bail!(
                            "Map initial value type mismatch: expected {}, got {}",
                            value_type,
                            actual_value_type
                        );
                    }
                }

                Ok(Type::Map {
                    key_type: Box::new(key_type),
                    value_type: Box::new(value_type),
                })
            }
        }
    }

    /// Look up a variable's type in the current scope
    fn lookup_variable(&self, name: &str) -> Result<Type> {
        // Check call stack first (local variables)
        if let Some(locals) = self.call_stack.last() {
            if let Some(var_type) = locals.get(name) {
                return Ok(var_type.clone());
            }
        }

        // Then check global variables
        self.variables
            .get(name)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Undefined variable: {}", name))
    }
}

fn op_to_string(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::Greater => ">",
        BinaryOp::LessEqual => "<=",
        BinaryOp::GreaterEqual => ">=",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_basic_type_checking() {
        let _checker = TypeChecker::new();

        // Test basic types
        assert_eq!(Type::from_string("int"), Type::Number);
        assert_eq!(Type::from_string("bool"), Type::Bool);
        assert_eq!(Type::from_string("string"), Type::String);
    }

    #[test]
    fn test_vector_type_checking() {
        let input = r#"
            fun main() do
                let v = new vec(int) {1, 2, 3};
                v[0] = 4;
                return v[0];
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_map_type_checking() {
        let input = r#"
            fun main() do
                let mymap = new map(string, int) {};
                mymap["hello"] = 1;
                return mymap["hello"];
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_error_detection() {
        let input = r#"
            fun main() do
                let x = 5;
                let y = "hello";
                return x + y;
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&program);
        assert!(result.is_err());

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Addition operation type mismatch"));
    }
}
