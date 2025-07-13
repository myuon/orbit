use crate::ast::{Decl, Expr, Function, Program, Stmt, StructDecl, StructNewKind};
use anyhow::{bail, Result};
use std::collections::HashMap;

/// Desugaring phase that transforms high-level constructs into simpler forms
pub struct Desugarer {
    /// Map of struct names to their declarations for method resolution
    structs: HashMap<String, StructDecl>,
}

impl Desugarer {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
        }
    }

    /// Desugar a complete program
    pub fn desugar_program(&mut self, program: Program) -> Result<Program> {
        // First pass: collect all struct declarations
        for decl in &program.declarations {
            if let Decl::Struct(struct_decl) = decl {
                self.structs
                    .insert(struct_decl.name.clone(), struct_decl.clone());
            }
        }

        // Second pass: desugar all declarations
        let mut desugared_declarations = Vec::new();
        for decl in program.declarations {
            match decl {
                Decl::Function(function) => {
                    let desugared_function = self.desugar_function(function)?;
                    desugared_declarations.push(Decl::Function(desugared_function));
                }
                Decl::GlobalVariable(global_var) => {
                    // Global variables don't need desugaring, pass them through
                    desugared_declarations.push(Decl::GlobalVariable(global_var));
                }
                Decl::Struct(struct_decl) => {
                    // Add the original struct declaration (without methods)
                    let desugared_struct = StructDecl {
                        name: struct_decl.name.clone(),
                        type_params: struct_decl.type_params.clone(),
                        fields: struct_decl.fields.clone(),
                        methods: Vec::new(), // Remove methods after desugaring
                    };
                    desugared_declarations.push(Decl::Struct(desugared_struct));

                    // Convert each method to a standalone function with mangled name
                    for method in struct_decl.methods {
                        let mangled_function = self.desugar_method(&struct_decl.name, method)?;
                        desugared_declarations.push(Decl::Function(mangled_function));
                    }
                }
            }
        }

        Ok(Program {
            declarations: desugared_declarations,
        })
    }

    /// Desugar a function by transforming method calls in its body
    fn desugar_function(&mut self, function: Function) -> Result<Function> {
        let mut desugared_body = self.desugar_statements(function.body)?;

        // Add return 0; if the last statement is not a return
        if !desugared_body.is_empty() {
            if let Some(last_stmt) = desugared_body.last() {
                if !matches!(last_stmt, Stmt::Return(_)) {
                    desugared_body.push(Stmt::Return(Expr::Int(0)));
                }
            }
        } else {
            // If function body is empty, add return 0;
            desugared_body.push(Stmt::Return(Expr::Int(0)));
        }

        Ok(Function {
            name: function.name,
            type_params: function.type_params,
            params: function.params,
            body: desugared_body,
        })
    }

    /// Convert a struct method to a standalone function with mangled name
    fn desugar_method(&mut self, struct_name: &str, method: Function) -> Result<Function> {
        let mangled_name = format!("{}_{}", struct_name, method.name);
        let mut desugared_body = self.desugar_statements(method.body)?;

        // Add return 0; if the last statement is not a return
        if !desugared_body.is_empty() {
            if let Some(last_stmt) = desugared_body.last() {
                if !matches!(last_stmt, Stmt::Return(_)) {
                    desugared_body.push(Stmt::Return(Expr::Int(0)));
                }
            }
        } else {
            // If method body is empty, add return 0;
            desugared_body.push(Stmt::Return(Expr::Int(0)));
        }

        Ok(Function {
            name: mangled_name,
            type_params: method.type_params,
            params: method.params,
            body: desugared_body,
        })
    }

    /// Desugar a list of statements
    fn desugar_statements(&mut self, statements: Vec<Stmt>) -> Result<Vec<Stmt>> {
        let mut desugared = Vec::new();
        for stmt in statements {
            desugared.push(self.desugar_statement(stmt)?);
        }
        Ok(desugared)
    }

    /// Desugar a single statement
    fn desugar_statement(&mut self, statement: Stmt) -> Result<Stmt> {
        match statement {
            Stmt::Let { name, value } => {
                let desugared_value = self.desugar_expression(value)?;
                Ok(Stmt::Let {
                    name,
                    value: desugared_value,
                })
            }
            Stmt::Expression(expr) => {
                let desugared_expr = self.desugar_expression(expr)?;
                Ok(Stmt::Expression(desugared_expr))
            }
            Stmt::Return(expr) => {
                let desugared_expr = self.desugar_expression(expr)?;
                Ok(Stmt::Return(desugared_expr))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let desugared_condition = self.desugar_expression(condition)?;
                let desugared_then = self.desugar_statements(then_branch)?;
                let desugared_else = if let Some(else_stmts) = else_branch {
                    Some(self.desugar_statements(else_stmts)?)
                } else {
                    None
                };
                Ok(Stmt::If {
                    condition: desugared_condition,
                    then_branch: desugared_then,
                    else_branch: desugared_else,
                })
            }
            Stmt::While { condition, body } => {
                let desugared_condition = self.desugar_expression(condition)?;
                let desugared_body = self.desugar_statements(body)?;
                Ok(Stmt::While {
                    condition: desugared_condition,
                    body: desugared_body,
                })
            }
            Stmt::Assign { name, value } => {
                let desugared_value = self.desugar_expression(value)?;
                Ok(Stmt::Assign {
                    name,
                    value: desugared_value,
                })
            }
            Stmt::VectorPush { vector, value } => {
                let desugared_value = self.desugar_expression(value)?;
                Ok(Stmt::VectorPush {
                    vector,
                    value: desugared_value,
                })
            }
            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type,
            } => {
                let desugared_index = self.desugar_expression(index)?;
                let desugared_value = self.desugar_expression(value)?;
                Ok(Stmt::IndexAssign {
                    container,
                    index: desugared_index,
                    value: desugared_value,
                    container_type,
                })
            }
            Stmt::FieldAssign {
                object,
                field,
                value,
            } => {
                let desugared_object = self.desugar_expression(object)?;
                let desugared_value = self.desugar_expression(value)?;
                Ok(Stmt::FieldAssign {
                    object: desugared_object,
                    field,
                    value: desugared_value,
                })
            }
        }
    }

    /// Desugar an expression, converting method calls to function calls
    fn desugar_expression(&mut self, expression: Expr) -> Result<Expr> {
        match expression {
            // Method call is the main target for desugaring
            Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => {
                if let Some(obj) = object {
                    // Instance method call: obj.method(args)
                    let desugared_object = self.desugar_expression(*obj)?;
                    let mut desugared_args = Vec::new();
                    for arg in args {
                        desugared_args.push(self.desugar_expression(arg)?);
                    }

                    // Use embedded type information if available, otherwise fall back to heuristic
                    let mangled_name = if let Some(struct_type) = type_name {
                        format!("{}_{}", struct_type, method)
                    } else {
                        self.resolve_method_name(&desugared_object, &method)?
                    };

                    // Convert to a regular function call with object as first argument
                    let mut call_args = vec![desugared_object];
                    call_args.extend(desugared_args);

                    Ok(Expr::Call {
                        callee: Box::new(Expr::Identifier(mangled_name)),
                        args: call_args,
                    })
                } else if let Some(type_name_str) = type_name {
                    // Associated method call: (type T).method(args)
                    let mut desugared_args = Vec::new();
                    for arg in args {
                        desugared_args.push(self.desugar_expression(arg)?);
                    }

                    // Convert to a regular function call with mangled name
                    // For (type T).method(args), this becomes T_method(args)
                    let mangled_name = format!("{}_{}", type_name_str, method);

                    Ok(Expr::Call {
                        callee: Box::new(Expr::Identifier(mangled_name)),
                        args: desugared_args,
                    })
                } else {
                    bail!("MethodCall must have either object or type_name specified")
                }
            }

            // Recursively desugar other expressions
            Expr::Binary { left, op, right } => {
                let desugared_left = self.desugar_expression(*left)?;
                let desugared_right = self.desugar_expression(*right)?;
                Ok(Expr::Binary {
                    left: Box::new(desugared_left),
                    op,
                    right: Box::new(desugared_right),
                })
            }
            Expr::Call { callee, args } => {
                let desugared_callee = self.desugar_expression(*callee)?;
                let mut desugared_args = Vec::new();
                for arg in args {
                    desugared_args.push(self.desugar_expression(arg)?);
                }
                Ok(Expr::Call {
                    callee: Box::new(desugared_callee),
                    args: desugared_args,
                })
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let mut desugared_values = Vec::new();
                for value in initial_values {
                    desugared_values.push(self.desugar_expression(value)?);
                }
                Ok(Expr::VectorNew {
                    element_type,
                    initial_values: desugared_values,
                })
            }
            Expr::Index {
                container,
                index,
                container_type,
            } => {
                let desugared_container = self.desugar_expression(*container)?;
                let desugared_index = self.desugar_expression(*index)?;
                Ok(Expr::Index {
                    container: Box::new(desugared_container),
                    index: Box::new(desugared_index),
                    container_type,
                })
            }
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => {
                let mut desugared_pairs = Vec::new();
                for (key, value) in initial_pairs {
                    let desugared_key = self.desugar_expression(key)?;
                    let desugared_value = self.desugar_expression(value)?;
                    desugared_pairs.push((desugared_key, desugared_value));
                }
                Ok(Expr::MapNew {
                    key_type,
                    value_type,
                    initial_pairs: desugared_pairs,
                })
            }
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                let mut desugared_fields = Vec::new();
                for (field_name, field_value) in fields {
                    let desugared_value = self.desugar_expression(field_value)?;
                    desugared_fields.push((field_name, desugared_value));
                }
                Ok(Expr::StructNew {
                    type_name,
                    fields: desugared_fields,
                    kind,
                })
            }
            Expr::FieldAccess { object, field } => {
                let desugared_object = self.desugar_expression(*object)?;
                Ok(Expr::FieldAccess {
                    object: Box::new(desugared_object),
                    field,
                })
            }

            Expr::Alloc {
                element_type,
                kind,
                size,
                initial_values,
            } => {
                let desugared_size = if let Some(size_expr) = size {
                    Some(Box::new(self.desugar_expression(*size_expr)?))
                } else {
                    None
                };
                let desugared_initial_values = if let Some(values) = initial_values {
                    let mut desugared_values = Vec::new();
                    for value in values {
                        desugared_values.push(self.desugar_expression(value)?);
                    }
                    Some(desugared_values)
                } else {
                    None
                };
                Ok(Expr::Alloc {
                    element_type,
                    kind,
                    size: desugared_size,
                    initial_values: desugared_initial_values,
                })
            }

            // Leaf expressions that don't need desugaring
            Expr::Int(_)
            | Expr::Boolean(_)
            | Expr::String(_)
            | Expr::Byte(_)
            | Expr::Identifier(_)
            | Expr::TypeExpr { .. } => Ok(expression),
        }
    }

    /// Resolve method name to mangled function name
    /// This is a simplified implementation that tries to infer struct type from context
    fn resolve_method_name(&self, object: &Expr, method: &str) -> Result<String> {
        // Try to infer struct type from various contexts
        let struct_name = match object {
            // For identifiers, try to infer from available struct types
            Expr::Identifier(_) => {
                // For now, try all available struct types to see which one has this method
                for (struct_name, struct_decl) in &self.structs {
                    for struct_method in &struct_decl.methods {
                        if struct_method.name == method {
                            return Ok(format!("{}_{}", struct_name, method));
                        }
                    }
                }
                // If no match found, default to first available struct (for demo purposes)
                if let Some((struct_name, _)) = self.structs.iter().next() {
                    struct_name.clone()
                } else {
                    return Err(anyhow::anyhow!(
                        "No struct types available for method resolution"
                    ));
                }
            }
            // For struct instantiation, we can determine the type directly
            Expr::StructNew { type_name, .. } => type_name.clone(),
            // For field access, try to infer the field type
            Expr::FieldAccess { object: _, field } => {
                // Try to find the field type in our struct definitions
                for (_struct_name, struct_decl) in &self.structs {
                    for struct_field in &struct_decl.fields {
                        if struct_field.name == *field {
                            return Ok(format!("{}_{}", struct_field.type_name, method));
                        }
                    }
                }
                // If no match found, default to first available struct
                if let Some((struct_name, _)) = self.structs.iter().next() {
                    struct_name.clone()
                } else {
                    return Err(anyhow::anyhow!(
                        "No struct types available for method resolution"
                    ));
                }
            }
            // For other expressions, default to first available struct
            _ => {
                if let Some((struct_name, _)) = self.structs.iter().next() {
                    struct_name.clone()
                } else {
                    return Err(anyhow::anyhow!(
                        "No struct types available for method resolution"
                    ));
                }
            }
        };

        Ok(format!("{}_{}", struct_name, method))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, FunParam};

    #[test]
    fn test_method_call_desugaring() {
        let mut desugarer = Desugarer::new();

        // Add a struct type to the desugarer for testing
        let point_struct = StructDecl {
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![],
            methods: vec![Function {
                name: "sum".to_string(),
                type_params: vec![],
                params: vec![],
                body: vec![],
            }],
        };
        desugarer.structs.insert("Point".to_string(), point_struct);

        // Create a method call: p.sum()
        let method_call = Expr::MethodCall {
            object: Some(Box::new(Expr::Identifier("p".to_string()))),
            type_name: None,
            method: "sum".to_string(),
            args: vec![],
        };

        let result = desugarer.desugar_expression(method_call).unwrap();

        // Should be converted to: Point_sum(p)
        if let Expr::Call { callee, args } = result {
            if let Expr::Identifier(func_name) = callee.as_ref() {
                assert_eq!(func_name, "Point_sum");
                assert_eq!(args.len(), 1);
                if let Expr::Identifier(arg_name) = &args[0] {
                    assert_eq!(arg_name, "p");
                } else {
                    panic!("Expected identifier argument");
                }
            } else {
                panic!("Expected identifier callee");
            }
        } else {
            panic!("Expected function call result");
        }
    }

    #[test]
    fn test_struct_method_to_function_conversion() {
        let mut desugarer = Desugarer::new();

        // Create a struct with a method
        let method = Function {
            name: "sum".to_string(),
            type_params: vec![],
            params: vec![FunParam {
                name: "self".to_string(),
                type_name: Some("Point".to_string()),
            }],
            body: vec![],
        };

        let result = desugarer.desugar_method("Point", method).unwrap();

        assert_eq!(result.name, "Point_sum");
        assert_eq!(result.params.len(), 1);
        assert_eq!(result.params[0].name, "self");
    }

    #[test]
    fn test_method_call_with_embedded_type() {
        let mut desugarer = Desugarer::new();

        // Create a method call with embedded type information: p.sum()
        let method_call = Expr::MethodCall {
            object: Some(Box::new(Expr::Identifier("p".to_string()))),
            type_name: Some("Point".to_string()), // Type information embedded by type checker
            method: "sum".to_string(),
            args: vec![],
        };

        let result = desugarer.desugar_expression(method_call).unwrap();

        // Should be converted to: Point_sum(p) using embedded type info
        if let Expr::Call { callee, args } = result {
            if let Expr::Identifier(func_name) = callee.as_ref() {
                assert_eq!(func_name, "Point_sum");
                assert_eq!(args.len(), 1);
                if let Expr::Identifier(arg_name) = &args[0] {
                    assert_eq!(arg_name, "p");
                } else {
                    panic!("Expected identifier argument");
                }
            } else {
                panic!("Expected identifier callee");
            }
        } else {
            panic!("Expected function call result");
        }
    }

    #[test]
    fn test_struct_new_pattern_desugaring() {
        let mut desugarer = Desugarer::new();

        // Create a StructNew expression with kind=Pattern and a nested expression
        let struct_new_pattern = Expr::StructNew {
            type_name: "Point".to_string(),
            fields: vec![
                ("x".to_string(), Expr::Int(5)),
                (
                    "y".to_string(),
                    Expr::Binary {
                        left: Box::new(Expr::Int(2)),
                        op: BinaryOp::Add,
                        right: Box::new(Expr::Int(3)),
                    },
                ),
            ],
            kind: StructNewKind::Pattern,
        };

        let result = desugarer.desugar_expression(struct_new_pattern).unwrap();

        // Should desugar nested expressions while preserving the StructNew structure
        if let Expr::StructNew {
            type_name,
            fields,
            kind: StructNewKind::Pattern,
        } = result
        {
            assert_eq!(type_name, "Point");
            assert_eq!(fields.len(), 2);

            // Check first field
            assert_eq!(fields[0].0, "x");
            if let Expr::Int(val) = &fields[0].1 {
                assert_eq!(*val, 5);
            } else {
                panic!("Expected number in first field");
            }

            // Check second field - the nested expression should be desugared
            assert_eq!(fields[1].0, "y");
            if let Expr::Binary { left, op, right } = &fields[1].1 {
                assert_eq!(*op, BinaryOp::Add);
                if let Expr::Int(left_val) = left.as_ref() {
                    assert_eq!(*left_val, 2);
                } else {
                    panic!("Expected number in left operand");
                }
                if let Expr::Int(right_val) = right.as_ref() {
                    assert_eq!(*right_val, 3);
                } else {
                    panic!("Expected number in right operand");
                }
            } else {
                panic!("Expected binary expression in second field");
            }
        } else {
            panic!("Expected StructNew result with kind=Pattern");
        }
    }

    #[test]
    fn test_function_auto_return_zero() {
        let mut desugarer = Desugarer::new();

        // Create a function without explicit return
        let function = Function {
            name: "test_func".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![Stmt::Let {
                name: "x".to_string(),
                value: Expr::Int(42),
            }],
        };

        let result = desugarer.desugar_function(function).unwrap();

        // Should have added return 0; at the end
        assert_eq!(result.body.len(), 2);
        if let Stmt::Return(Expr::Int(0)) = &result.body[1] {
            // Expected behavior
        } else {
            panic!("Expected return 0; to be added");
        }
    }

    #[test]
    fn test_function_with_existing_return() {
        let mut desugarer = Desugarer::new();

        // Create a function with explicit return
        let function = Function {
            name: "test_func".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![
                Stmt::Let {
                    name: "x".to_string(),
                    value: Expr::Int(42),
                },
                Stmt::Return(Expr::Int(42)),
            ],
        };

        let result = desugarer.desugar_function(function).unwrap();

        // Should NOT have added another return
        assert_eq!(result.body.len(), 2);
        if let Stmt::Return(Expr::Int(42)) = &result.body[1] {
            // Expected behavior - original return preserved
        } else {
            panic!("Expected original return to be preserved");
        }
    }

    #[test]
    fn test_empty_function_gets_return_zero() {
        let mut desugarer = Desugarer::new();

        // Create an empty function
        let function = Function {
            name: "empty_func".to_string(),
            type_params: vec![],
            params: vec![],
            body: vec![],
        };

        let result = desugarer.desugar_function(function).unwrap();

        // Should have added return 0;
        assert_eq!(result.body.len(), 1);
        if let Stmt::Return(Expr::Int(0)) = &result.body[0] {
            // Expected behavior
        } else {
            panic!("Expected return 0; to be added to empty function");
        }
    }
}
