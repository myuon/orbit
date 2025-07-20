use crate::ast::{
    Decl, Expr, Function, Positioned, PositionedExpr, PositionedStmt, Program, Stmt, StructDecl,
    StructNewKind, Type,
};
use anyhow::{bail, Result};
use std::collections::HashMap;

/// Operations that can be desugared to method calls
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DesugarOperation {
    Index,  // container[index] -> container._get(index)
    Assign, // container[index] = value -> container._set(index, value)
    Push,   // push(container, value) -> container._push(value)
    New,    // new Type {} -> Type._new()
}

/// Configuration for which operations a type supports desugaring for
#[derive(Debug, Clone)]
struct TypeDesugarConfig {
    operations: HashMap<DesugarOperation, String>, // operation -> method_name
}

impl TypeDesugarConfig {
    fn new() -> Self {
        Self {
            operations: HashMap::new(),
        }
    }

    fn with_operation(mut self, op: DesugarOperation, method_name: &str) -> Self {
        self.operations.insert(op, method_name.to_string());
        self
    }

    fn supports_operation(&self, op: &DesugarOperation) -> bool {
        self.operations.contains_key(op)
    }

    fn get_method_name(&self, op: &DesugarOperation) -> Option<&String> {
        self.operations.get(op)
    }
}

/// Desugaring phase that transforms high-level constructs into simpler forms
pub struct Desugarer {
    /// Map of struct names to their declarations for method resolution
    structs: HashMap<String, StructDecl>,
    /// Map of type names to their desugar operation configurations
    type_operations: HashMap<String, TypeDesugarConfig>,
}

impl Desugarer {
    pub fn new() -> Self {
        let mut type_operations = HashMap::new();

        // Configure vec type operations
        let vec_config = TypeDesugarConfig::new()
            .with_operation(DesugarOperation::Index, "_get")
            .with_operation(DesugarOperation::Assign, "_set")
            .with_operation(DesugarOperation::Push, "_push")
            .with_operation(DesugarOperation::New, "_new");
        type_operations.insert("vec".to_string(), vec_config);

        // Configure array type operations
        let array_config = TypeDesugarConfig::new()
            .with_operation(DesugarOperation::Index, "_get")
            .with_operation(DesugarOperation::Assign, "_set");
        type_operations.insert("array".to_string(), array_config);

        Self {
            structs: HashMap::new(),
            type_operations,
        }
    }

    /// Check if a type supports a specific desugar operation
    fn supports_operation(&self, type_name: &str, operation: &DesugarOperation) -> bool {
        self.type_operations
            .get(type_name)
            .map(|config| config.supports_operation(operation))
            .unwrap_or(false)
    }

    /// Get the method name for a specific operation on a type
    fn get_method_name(&self, type_name: &str, operation: &DesugarOperation) -> Option<&String> {
        self.type_operations
            .get(type_name)
            .and_then(|config| config.get_method_name(operation))
    }

    /// Create a method call expression for collection operations
    fn create_collection_method_call(
        &mut self,
        container: PositionedExpr,
        operation: DesugarOperation,
        args: Vec<PositionedExpr>,
        type_name: &str,
        type_args: &[Type],
    ) -> Result<PositionedExpr> {
        let method_name = self
            .get_method_name(type_name, &operation)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Operation {:?} not supported for type {}",
                    operation,
                    type_name
                )
            })?
            .clone();

        let type_params = if type_args.is_empty() {
            "T".to_string()
        } else {
            type_args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        };

        let method_call = Expr::MethodCall {
            object: Some(Box::new(container)),
            type_name: Some(format!("{}({})", type_name, type_params)),
            method: method_name,
            args,
        };

        self.desugar_expression(&Positioned::with_unknown_span(method_call))
    }

    /// Create an associated method call expression for collection types (like _new)
    fn create_collection_associated_method_call(
        &mut self,
        operation: DesugarOperation,
        args: Vec<PositionedExpr>,
        type_name: &str,
        type_args: &[Type],
    ) -> Result<PositionedExpr> {
        let method_name = self
            .get_method_name(type_name, &operation)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Operation {:?} not supported for type {}",
                    operation,
                    type_name
                )
            })?
            .clone();

        let type_params = if type_args.is_empty() {
            "T".to_string()
        } else {
            type_args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        };

        let method_call = Expr::MethodCall {
            object: None,
            type_name: Some(format!("{}({})", type_name, type_params)),
            method: method_name,
            args,
        };

        self.desugar_expression(&Positioned::with_unknown_span(method_call))
    }

    /// Desugar a complete program
    pub fn desugar_program(&mut self, program: Program) -> Result<Program> {
        // First pass: collect all struct declarations
        for decl in &program.declarations {
            if let Decl::Struct(struct_decl) = &decl.value {
                self.structs
                    .insert(struct_decl.value.name.clone(), struct_decl.value.clone());
            }
        }

        // Second pass: desugar all declarations
        let mut desugared_declarations = Vec::new();
        for decl in program.declarations {
            match &decl.value {
                Decl::Function(function) => {
                    let desugared_function = self.desugar_function(&function.value)?;
                    desugared_declarations.push(Positioned::with_unknown_span(Decl::Function(
                        Positioned::with_unknown_span(desugared_function),
                    )));
                }
                Decl::GlobalVariable(_global_var) => {
                    // Global variables don't need desugaring, pass them through
                    desugared_declarations.push(decl);
                }
                Decl::Struct(struct_decl) => {
                    // Add the original struct declaration (without methods)
                    let desugared_struct = StructDecl {
                        name: struct_decl.value.name.clone(),
                        type_params: struct_decl.value.type_params.clone(),
                        fields: struct_decl.value.fields.clone(),
                        methods: Vec::new(), // Remove methods after desugaring
                    };
                    desugared_declarations.push(Positioned::with_unknown_span(Decl::Struct(
                        Positioned::with_unknown_span(desugared_struct),
                    )));

                    // Convert each method to a standalone function with mangled name
                    for method in &struct_decl.value.methods {
                        let mangled_function =
                            self.desugar_method(&struct_decl.value.name, &method.value)?;
                        desugared_declarations.push(Positioned::with_unknown_span(Decl::Function(
                            Positioned::with_unknown_span(mangled_function),
                        )));
                    }
                }
            }
        }

        Ok(Program {
            declarations: desugared_declarations,
        })
    }

    /// Desugar a function by transforming method calls in its body
    fn desugar_function(&mut self, function: &Function) -> Result<Function> {
        let mut desugared_body = self.desugar_statements(&function.body)?;

        // Add return 0; if the last statement is not a return
        if !desugared_body.is_empty() {
            if let Some(last_stmt) = desugared_body.last() {
                if !matches!(last_stmt.value, Stmt::Return(_)) {
                    desugared_body.push(Positioned::with_unknown_span(Stmt::Return(
                        Positioned::with_unknown_span(Expr::Int(0)),
                    )));
                }
            }
        } else {
            // If function body is empty, add return 0;
            desugared_body.push(Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Int(0)),
            )));
        }

        Ok(Function {
            name: function.name.clone(),
            type_params: function.type_params.clone(),
            params: function.params.clone(),
            body: desugared_body,
        })
    }

    /// Convert a struct method to a standalone function with mangled name
    fn desugar_method(&mut self, struct_name: &str, method: &Function) -> Result<Function> {
        let mangled_name = format!("{}__{}", struct_name, method.name);
        let mut desugared_body = self.desugar_statements(&method.body)?;

        // Add return 0; if the last statement is not a return
        if !desugared_body.is_empty() {
            if let Some(last_stmt) = desugared_body.last() {
                if !matches!(last_stmt.value, Stmt::Return(_)) {
                    desugared_body.push(Positioned::with_unknown_span(Stmt::Return(
                        Positioned::with_unknown_span(Expr::Int(0)),
                    )));
                }
            }
        } else {
            // If method body is empty, add return 0;
            desugared_body.push(Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Int(0)),
            )));
        }

        Ok(Function {
            name: mangled_name,
            type_params: method.type_params.clone(),
            params: method.params.clone(),
            body: desugared_body,
        })
    }

    /// Desugar a list of statements
    fn desugar_statements(&mut self, statements: &[PositionedStmt]) -> Result<Vec<PositionedStmt>> {
        let mut desugared = Vec::new();
        for stmt in statements {
            desugared.push(self.desugar_statement(stmt)?);
        }
        Ok(desugared)
    }

    /// Desugar a single statement
    fn desugar_statement(&mut self, statement: &PositionedStmt) -> Result<PositionedStmt> {
        let desugared_stmt = match &statement.value {
            Stmt::Let { name, value } => {
                let desugared_value = self.desugar_expression(value)?;
                Stmt::Let {
                    name: name.clone(),
                    value: desugared_value,
                }
            }
            Stmt::Expression(expr) => {
                let desugared_expr = self.desugar_expression(expr)?;
                Stmt::Expression(desugared_expr)
            }
            Stmt::Return(expr) => {
                let desugared_expr = self.desugar_expression(expr)?;
                Stmt::Return(desugared_expr)
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
                Stmt::If {
                    condition: desugared_condition,
                    then_branch: desugared_then,
                    else_branch: desugared_else,
                }
            }
            Stmt::While { condition, body } => {
                let desugared_condition = self.desugar_expression(condition)?;
                let desugared_body = self.desugar_statements(body)?;
                Stmt::While {
                    condition: desugared_condition,
                    body: desugared_body,
                }
            }
            Stmt::Assign { lvalue, value } => {
                let desugared_value = self.desugar_expression(value)?;

                // Check if lvalue is an Index expression with vec type
                if let Expr::Index {
                    container,
                    index,
                    container_value_type,
                    ..
                } = &lvalue.value
                {
                    if let Some(ref container_type) = container_value_type {
                        match container_type {
                            Type::Struct { name, args }
                                if self.supports_operation(name, &DesugarOperation::Assign) =>
                            {
                                // Convert to method call: container._set(index, value)
                                let desugared_container = self.desugar_expression(container)?;
                                let desugared_index = self.desugar_expression(index)?;
                                let desugared_method_call = self.create_collection_method_call(
                                    desugared_container,
                                    DesugarOperation::Assign,
                                    vec![desugared_index, desugared_value],
                                    name,
                                    args,
                                )?;
                                return Ok(Positioned::with_unknown_span(Stmt::Expression(
                                    desugared_method_call,
                                )));
                            }
                            Type::Struct {
                                name: struct_name,
                                args: _,
                            } if struct_name.contains("vec") => {
                                // Convert to method call: container._set(index, value)
                                let desugared_container = self.desugar_expression(container)?;
                                let desugared_index = self.desugar_expression(index)?;
                                let method_call = Expr::MethodCall {
                                    object: Some(Box::new(desugared_container)),
                                    type_name: Some(struct_name.clone()),
                                    method: "_set".to_string(),
                                    args: vec![desugared_index, desugared_value],
                                };
                                let desugared_method_call = self.desugar_expression(
                                    &Positioned::with_unknown_span(method_call),
                                )?;
                                return Ok(Positioned::with_unknown_span(Stmt::Expression(
                                    desugared_method_call,
                                )));
                            }
                            _ => {}
                        }
                    }
                }

                // Default case: regular assignment
                let desugared_lvalue = self.desugar_expression(lvalue)?;
                Stmt::Assign {
                    lvalue: desugared_lvalue,
                    value: desugared_value,
                }
            }
            Stmt::VectorPush {
                vector,
                value,
                vector_type,
            } => {
                let desugared_value = self.desugar_expression(value)?;

                // Check if this is a vec type that should be desugared to method call
                if let Some(ref vtype) = vector_type {
                    match vtype {
                        Type::Struct { name, args }
                            if self.supports_operation(name, &DesugarOperation::Push) =>
                        {
                            // Convert to method call: vector._push(value)
                            let vector_expr =
                                Positioned::with_unknown_span(Expr::Identifier(vector.clone()));
                            let desugared_method_call = self.create_collection_method_call(
                                vector_expr,
                                DesugarOperation::Push,
                                vec![desugared_value],
                                name,
                                args,
                            )?;
                            return Ok(Positioned::with_unknown_span(Stmt::Expression(
                                desugared_method_call,
                            )));
                        }
                        _ => {}
                    }
                }

                // Default case: regular vector push
                Stmt::VectorPush {
                    vector: vector.clone(),
                    value: desugared_value,
                    vector_type: vector_type.clone(),
                }
            }
        };
        Ok(Positioned::with_unknown_span(desugared_stmt))
    }

    /// Desugar an expression, converting method calls to function calls
    fn desugar_expression(&mut self, expression: &PositionedExpr) -> Result<PositionedExpr> {
        let desugared_expr = match &expression.value {
            // Method call is the main target for desugaring
            Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => {
                if let Some(obj) = object {
                    // Instance method call: obj.method(args)
                    let desugared_object = self.desugar_expression(obj)?;
                    let mut desugared_args = Vec::new();
                    for arg in args {
                        desugared_args.push(self.desugar_expression(arg)?);
                    }

                    // Use embedded type information if available, otherwise fall back to heuristic
                    let mangled_name = if let Some(struct_type) = type_name {
                        let separator = if method.starts_with('_') { "___" } else { "__" };
                        format!("{}{}{}", struct_type, separator, method)
                    } else {
                        self.resolve_method_name(&desugared_object.value, method)?
                    };

                    // Convert to a regular function call with object as first argument
                    let mut call_args = vec![desugared_object];
                    call_args.extend(desugared_args);

                    Expr::Call {
                        callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                            mangled_name,
                        ))),
                        args: call_args,
                    }
                } else if let Some(type_name_str) = type_name {
                    // Associated method call: (type T).method(args)
                    let mut desugared_args = Vec::new();
                    for arg in args {
                        desugared_args.push(self.desugar_expression(arg)?);
                    }

                    // Convert to a regular function call with mangled name
                    // For (type T).method(args), this becomes T__method(args)
                    let mangled_name = format!("{}__{}", type_name_str, method);

                    Expr::Call {
                        callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                            mangled_name,
                        ))),
                        args: desugared_args,
                    }
                } else {
                    bail!("MethodCall must have either object or type_name specified")
                }
            }

            // Recursively desugar other expressions
            Expr::Binary { left, op, right } => {
                let desugared_left = self.desugar_expression(left)?;
                let desugared_right = self.desugar_expression(right)?;
                Expr::Binary {
                    left: Box::new(desugared_left),
                    op: *op,
                    right: Box::new(desugared_right),
                }
            }
            Expr::Call { callee, args } => {
                let desugared_callee = self.desugar_expression(callee)?;
                let mut desugared_args = Vec::new();
                for arg in args {
                    desugared_args.push(self.desugar_expression(arg)?);
                }
                Expr::Call {
                    callee: Box::new(desugared_callee),
                    args: desugared_args,
                }
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let mut desugared_values = Vec::new();
                for value in initial_values {
                    desugared_values.push(self.desugar_expression(value)?);
                }
                Expr::VectorNew {
                    element_type: element_type.clone(),
                    initial_values: desugared_values,
                }
            }
            Expr::Index {
                container,
                index,
                container_type,
                container_value_type,
            } => {
                let desugared_container = self.desugar_expression(container)?;
                let desugared_index = self.desugar_expression(index)?;

                // Check if this is a string type that should be converted to array(byte) indexing
                if let Some(ref container_type) = container_value_type {
                    match container_type {
                        Type::String => {
                            // Convert string[index] to generic array function call: array(T)____get(container, index)
                            let function_call = Expr::Call {
                                callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                                    "array(T)____get".to_string(),
                                ))),
                                args: vec![desugared_container, desugared_index],
                            };
                            return Ok(Positioned::with_unknown_span(function_call));
                        }
                        Type::Struct { name, args }
                            if self.supports_operation(name, &DesugarOperation::Index) =>
                        {
                            // Convert to method call: container._get(index)
                            return self.create_collection_method_call(
                                desugared_container,
                                DesugarOperation::Index,
                                vec![desugared_index],
                                name,
                                args,
                            );
                        }
                        _ => {}
                    }
                }

                // Default case: regular index operation
                Expr::Index {
                    container: Box::new(desugared_container),
                    index: Box::new(desugared_index),
                    container_type: container_type.clone(),
                    container_value_type: container_value_type.clone(),
                }
            }
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                // Check if this is a vec type with Pattern kind (empty fields) that should be desugared to _new method call
                if *kind == StructNewKind::Pattern && fields.is_empty() {
                    match type_name {
                        Type::Struct { name, args }
                            if self.supports_operation(name, &DesugarOperation::New) =>
                        {
                            // Convert to method call: vec(T)._new()
                            return self.create_collection_associated_method_call(
                                DesugarOperation::New,
                                vec![],
                                name,
                                args,
                            );
                        }
                        Type::Struct {
                            name: struct_name,
                            args: _,
                        } if struct_name.contains("vec") => {
                            // Convert to method call: vec(T)._new()
                            let method_call = Expr::MethodCall {
                                object: None,
                                type_name: Some(struct_name.clone()),
                                method: "_new".to_string(),
                                args: vec![],
                            };
                            return Ok(self.desugar_expression(&Positioned::with_unknown_span(
                                method_call,
                            ))?);
                        }
                        _ => {}
                    }
                }

                let mut desugared_fields = Vec::new();
                for (field_name, field_value) in fields {
                    let desugared_value = self.desugar_expression(field_value)?;
                    desugared_fields.push((field_name.clone(), desugared_value));
                }
                Expr::StructNew {
                    type_name: type_name.clone(),
                    fields: desugared_fields,
                    kind: *kind,
                }
            }
            Expr::FieldAccess { object, field } => {
                let desugared_object = self.desugar_expression(object)?;
                Expr::FieldAccess {
                    object: Box::new(desugared_object),
                    field: field.clone(),
                }
            }

            Expr::Alloc { element_type, size } => {
                let desugared_size = Box::new(self.desugar_expression(size)?);
                Expr::Alloc {
                    element_type: element_type.clone(),
                    size: desugared_size,
                }
            }

            Expr::Sizeof { type_name } => {
                // sizeof expressions don't need desugaring
                Expr::Sizeof {
                    type_name: type_name.clone(),
                }
            }

            Expr::Cast { expr, target_type } => {
                // Cast expressions just need their inner expression desugared
                Expr::Cast {
                    expr: Box::new(self.desugar_expression(expr)?),
                    target_type: target_type.clone(),
                }
            }

            // String literals need to be converted to array(byte) structures
            Expr::String(s) => {
                // Convert "hello" to new(struct) array(byte) { .data = pushString("hello"), .length = 5 }
                let string_length = s.len() as i64;
                Expr::StructNew {
                    type_name: Type::Struct {
                        name: "array".to_string(),
                        args: vec![Type::Byte],
                    },
                    fields: vec![
                        (
                            "data".to_string(),
                            Positioned::with_unknown_span(Expr::PushString(s.clone())),
                        ),
                        (
                            "length".to_string(),
                            Positioned::with_unknown_span(Expr::Int(string_length)),
                        ),
                    ],
                    kind: StructNewKind::Regular,
                }
            }
            // Leaf expressions that don't need desugaring
            Expr::Int(_)
            | Expr::Boolean(_)
            | Expr::Byte(_)
            | Expr::Identifier(_)
            | Expr::TypeExpr { .. }
            | Expr::PushString(_) => expression.value.clone(),
        };
        Ok(Positioned::with_unknown_span(desugared_expr))
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
                        if struct_method.value.name == method {
                            return Ok(format!("{}__{}", struct_name, method));
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
            Expr::StructNew { type_name, .. } => type_name.to_string(),
            // For field access, try to infer the field type
            Expr::FieldAccess { object: _, field } => {
                // Try to find the field type in our struct definitions
                for (_struct_name, struct_decl) in &self.structs {
                    for struct_field in &struct_decl.fields {
                        if struct_field.name == *field {
                            return Ok(format!("{}__{}", struct_field.type_name, method));
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

        Ok(format!("{}__{}", struct_name, method))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, FunParam, StructNewKind, Type};

    #[test]
    fn test_method_call_desugaring() {
        let mut desugarer = Desugarer::new();

        // Add a struct type to the desugarer for testing
        let point_struct = StructDecl {
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![],
            methods: vec![Positioned::with_unknown_span(Function {
                name: "sum".to_string(),
                type_params: vec![],
                params: vec![],
                body: vec![],
            })],
        };
        desugarer.structs.insert("Point".to_string(), point_struct);

        // Create a method call: p.sum()
        let method_call = Positioned::with_unknown_span(Expr::MethodCall {
            object: Some(Box::new(Positioned::with_unknown_span(Expr::Identifier(
                "p".to_string(),
            )))),
            type_name: None,
            method: "sum".to_string(),
            args: vec![],
        });

        let result = desugarer.desugar_expression(&method_call).unwrap();

        // Should be converted to: Point__sum(p)
        if let Expr::Call { callee, args } = &result.value {
            if let Expr::Identifier(func_name) = &callee.value {
                assert_eq!(func_name, "Point__sum");
                assert_eq!(args.len(), 1);
                if let Expr::Identifier(arg_name) = &args[0].value {
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

        let result = desugarer.desugar_method("Point", &method).unwrap();

        assert_eq!(result.name, "Point__sum");
        assert_eq!(result.params.len(), 1);
        assert_eq!(result.params[0].name, "self");
    }

    #[test]
    fn test_method_call_with_embedded_type() {
        let mut desugarer = Desugarer::new();

        // Create a method call with embedded type information: p.sum()
        let method_call = Positioned::with_unknown_span(Expr::MethodCall {
            object: Some(Box::new(Positioned::with_unknown_span(Expr::Identifier(
                "p".to_string(),
            )))),
            type_name: Some("Point".to_string()), // Type information embedded by type checker
            method: "sum".to_string(),
            args: vec![],
        });

        let result = desugarer.desugar_expression(&method_call).unwrap();

        // Should be converted to: Point__sum(p) using embedded type info
        if let Expr::Call { callee, args } = &result.value {
            if let Expr::Identifier(func_name) = &callee.value {
                assert_eq!(func_name, "Point__sum");
                assert_eq!(args.len(), 1);
                if let Expr::Identifier(arg_name) = &args[0].value {
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
        let struct_new_pattern = Positioned::with_unknown_span(Expr::StructNew {
            type_name: Type::from_string("Point"),
            fields: vec![
                ("x".to_string(), Positioned::with_unknown_span(Expr::Int(5))),
                (
                    "y".to_string(),
                    Positioned::with_unknown_span(Expr::Binary {
                        left: Box::new(Positioned::with_unknown_span(Expr::Int(2))),
                        op: BinaryOp::Add,
                        right: Box::new(Positioned::with_unknown_span(Expr::Int(3))),
                    }),
                ),
            ],
            kind: StructNewKind::Pattern,
        });

        let result = desugarer.desugar_expression(&struct_new_pattern).unwrap();

        // Should desugar nested expressions while preserving the StructNew structure
        if let Expr::StructNew {
            type_name,
            fields,
            kind: StructNewKind::Pattern,
        } = &result.value
        {
            assert_eq!(*type_name, Type::from_string("Point"));
            assert_eq!(fields.len(), 2);

            // Check first field
            assert_eq!(fields[0].0, "x");
            if let Expr::Int(val) = &fields[0].1.value {
                assert_eq!(*val, 5);
            } else {
                panic!("Expected number in first field");
            }

            // Check second field - the nested expression should be desugared
            assert_eq!(fields[1].0, "y");
            if let Expr::Binary { left, op, right } = &fields[1].1.value {
                assert_eq!(*op, BinaryOp::Add);
                if let Expr::Int(left_val) = &left.value {
                    assert_eq!(*left_val, 2);
                } else {
                    panic!("Expected number in left operand");
                }
                if let Expr::Int(right_val) = &right.value {
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
            body: vec![Positioned::with_unknown_span(Stmt::Let {
                name: "x".to_string(),
                value: Positioned::with_unknown_span(Expr::Int(42)),
            })],
        };

        let result = desugarer.desugar_function(&function).unwrap();

        // Should have added return 0; at the end
        assert_eq!(result.body.len(), 2);
        if let Stmt::Return(return_expr) = &result.body[1].value {
            if let Expr::Int(0) = &return_expr.value {
                // Expected behavior
            } else {
                panic!("Expected return 0; to be added");
            }
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
                Positioned::with_unknown_span(Stmt::Let {
                    name: "x".to_string(),
                    value: Positioned::with_unknown_span(Expr::Int(42)),
                }),
                Positioned::with_unknown_span(Stmt::Return(Positioned::with_unknown_span(
                    Expr::Int(42),
                ))),
            ],
        };

        let result = desugarer.desugar_function(&function).unwrap();

        // Should NOT have added another return
        assert_eq!(result.body.len(), 2);
        if let Stmt::Return(return_expr) = &result.body[1].value {
            if let Expr::Int(42) = &return_expr.value {
                // Expected behavior - original return preserved
            } else {
                panic!("Expected original return to be preserved");
            }
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

        let result = desugarer.desugar_function(&function).unwrap();

        // Should have added return 0;
        assert_eq!(result.body.len(), 1);
        if let Stmt::Return(return_expr) = &result.body[0].value {
            if let Expr::Int(0) = &return_expr.value {
                // Expected behavior
            } else {
                panic!("Expected return 0; to be added to empty function");
            }
        } else {
            panic!("Expected return 0; to be added to empty function");
        }
    }
}
