use crate::{
    ast::{
        Decl, Expr, FunParam, Function, Positioned, PositionedDecl, PositionedExpr, PositionedStmt,
        Program, Stmt, StructDecl, StructField, StructNewKind, Type,
    },
    ast_visitor::{walk_expr_mut, walk_stmt_mut, VisitorMut},
};
use anyhow::Result;
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

/// Visitor for collecting struct declarations in the first pass
struct StructCollector {
    structs: HashMap<String, StructDecl>,
}

impl StructCollector {
    fn new() -> Self {
        Self {
            structs: HashMap::new(),
        }
    }
}

impl VisitorMut for StructCollector {
    fn visit_decl(&mut self, decl: &mut PositionedDecl) {
        if let Decl::Struct(struct_decl) = &decl.value {
            self.structs
                .insert(struct_decl.value.name.clone(), struct_decl.value.clone());
        }
        // Don't walk into the declaration - we only need the top-level struct info
    }
}

/// Visitor for transforming expressions and statements using visitor pattern
struct DesugarTransformer {
    structs: HashMap<String, StructDecl>,
    type_operations: HashMap<String, TypeDesugarConfig>,
}

impl DesugarTransformer {
    fn new(
        structs: HashMap<String, StructDecl>,
        type_operations: HashMap<String, TypeDesugarConfig>,
    ) -> Self {
        Self {
            structs,
            type_operations,
        }
    }

    fn supports_operation(&self, type_name: &str, operation: &DesugarOperation) -> bool {
        self.type_operations
            .get(type_name)
            .map(|config| config.supports_operation(operation))
            .unwrap_or(false)
    }

    fn get_method_name(&self, type_name: &str, operation: &DesugarOperation) -> Option<&String> {
        self.type_operations
            .get(type_name)
            .and_then(|config| config.get_method_name(operation))
    }

    fn transform_type(&self, type_expr: Type) -> Type {
        match type_expr {
            Type::String => Type::Struct {
                name: "array".to_string(),
                args: vec![Type::Byte],
            },
            Type::Struct { name, args } => Type::Struct {
                name,
                args: args
                    .into_iter()
                    .map(|arg| self.transform_type(arg))
                    .collect(),
            },
            Type::Pointer(inner) => Type::Pointer(Box::new(self.transform_type(*inner))),
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .into_iter()
                    .map(|param| self.transform_type(param))
                    .collect(),
                return_type: Box::new(self.transform_type(*return_type)),
            },
            // Other types remain unchanged
            t => t,
        }
    }
}

impl VisitorMut for DesugarTransformer {
    fn visit_expr(&mut self, expr: &mut PositionedExpr) {
        // First, recursively visit children
        walk_expr_mut(self, expr);

        // Then transform this expression based on desugar patterns
        match &mut expr.value {
            Expr::MethodCall {
                object,
                object_type,
                method,
                args,
            } => {
                if let Some(obj) = object {
                    // Instance method call: obj.method(args)
                    let mut call_args = vec![(**obj).clone()];
                    call_args.extend(args.clone());

                    // Use embedded type information if available
                    let mangled_name = if let Some(struct_type) = object_type {
                        struct_type.mangle_method_name(method)
                    } else {
                        // Fallback to resolving method name
                        match self.resolve_method_name(&obj.value, method) {
                            Ok(name) => name,
                            Err(_) => return, // Skip transformation if we can't resolve
                        }
                    };

                    // Convert to a regular function call
                    expr.value = Expr::Call {
                        callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                            mangled_name,
                        ))),
                        args: call_args,
                    };
                } else if let Some(type_name_str) = object_type {
                    // Associated method call: (type T).method(args)
                    let mangled_name = type_name_str.mangle_method_name(method);
                    expr.value = Expr::Call {
                        callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                            mangled_name,
                        ))),
                        args: args.clone(),
                    };
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
                        Type::Struct { name, args: _ }
                            if self.supports_operation(name, &DesugarOperation::New) =>
                        {
                            // Convert to method call: vec(T)._new()
                            let method_name = self
                                .get_method_name(name, &DesugarOperation::New)
                                .unwrap()
                                .clone();
                            expr.value = Expr::MethodCall {
                                object: None,
                                object_type: Some(type_name.clone()),
                                method: method_name,
                                args: vec![],
                            };
                            // Recursively process the new method call
                            self.visit_expr(expr);
                            return;
                        }
                        Type::Struct {
                            name: struct_name,
                            args: _,
                        } if struct_name.contains("vec") => {
                            // Convert to method call: vec(T)._new()
                            expr.value = Expr::MethodCall {
                                object: None,
                                object_type: Some(Type::Struct {
                                    name: struct_name.clone(),
                                    args: vec![],
                                }),
                                method: "_new".to_string(),
                                args: vec![],
                            };
                            // Recursively process the new method call
                            self.visit_expr(expr);
                            return;
                        }
                        _ => {}
                    }
                }

                // Check if this is an array struct with data/length fields that should use _new_from_data
                if let Type::Struct { name, args: _ } = type_name {
                    if name == "array" && *kind == StructNewKind::Regular && fields.len() == 2 {
                        // Check if fields are "data" and "length"
                        let has_data_field =
                            fields.iter().any(|(field_name, _)| field_name == "data");
                        let has_length_field =
                            fields.iter().any(|(field_name, _)| field_name == "length");

                        if has_data_field && has_length_field {
                            // Extract data and length field values
                            let data_expr = fields
                                .iter()
                                .find(|(field_name, _)| field_name == "data")
                                .map(|(_, expr)| expr.clone())
                                .unwrap();
                            let length_expr = fields
                                .iter()
                                .find(|(field_name, _)| field_name == "length")
                                .map(|(_, expr)| expr.clone())
                                .unwrap();

                            // Convert to method call: array(T)._new_from_data(data, length)
                            expr.value = Expr::MethodCall {
                                object: None,
                                object_type: Some(type_name.clone()),
                                method: "_new_from_data".to_string(),
                                args: vec![data_expr, length_expr],
                            };
                            // Recursively process the new method call
                            self.visit_expr(expr);
                            return;
                        }
                    }
                }
                // For other StructNew cases, the default walk_expr_mut will handle field expressions
            }
            Expr::Index {
                container,
                index,
                container_value_type,
                ..
            } => {
                // Check if this is a string type that should be converted to array(byte) indexing
                if let Some(ref container_type) = container_value_type {
                    match container_type {
                        Type::String => {
                            // Convert string[index] to concrete array function call: array(byte)#_get(container, index)
                            expr.value = Expr::Call {
                                callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                                    "array(byte)#_get".to_string(),
                                ))),
                                args: vec![(**container).clone(), (**index).clone()],
                            };
                            return;
                        }
                        Type::Struct { name, args: _ }
                            if self.supports_operation(name, &DesugarOperation::Index) =>
                        {
                            // Convert to method call: container._get(index)
                            let method_name = self
                                .get_method_name(name, &DesugarOperation::Index)
                                .unwrap()
                                .clone();
                            expr.value = Expr::MethodCall {
                                object: Some(Box::new((**container).clone())),
                                object_type: Some(container_type.clone()),
                                method: method_name,
                                args: vec![(**index).clone()],
                            };
                            // Recursively process the new method call
                            self.visit_expr(expr);
                            return;
                        }
                        _ => {}
                    }
                }
                // For other index cases, default walk_expr_mut will handle container and index
            }
            Expr::String(s) => {
                // Convert "hello" to array(byte) structure
                let string_length = s.len() as i64;
                expr.value = Expr::StructNew {
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
                };
                // Recursively process the new struct
                self.visit_expr(expr);
            }
            _ => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut PositionedStmt) {
        // Handle specific statement transformations
        match &mut stmt.value {
            Stmt::Assign { lvalue, value } => {
                // First visit the value expression
                self.visit_expr(value);

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
                            Type::Struct { name, args: _ }
                                if self.supports_operation(name, &DesugarOperation::Assign) =>
                            {
                                // Convert to method call: container._set(index, value)
                                let method_name = self
                                    .get_method_name(name, &DesugarOperation::Assign)
                                    .unwrap()
                                    .clone();
                                let method_call = Expr::MethodCall {
                                    object: Some(Box::new(container.as_ref().clone())),
                                    object_type: Some(container_type.clone()),
                                    method: method_name,
                                    args: vec![index.as_ref().clone(), value.clone()],
                                };
                                stmt.value =
                                    Stmt::Expression(Positioned::with_unknown_span(method_call));
                                // Visit the new method call
                                if let Stmt::Expression(expr) = &mut stmt.value {
                                    self.visit_expr(expr);
                                }
                                return;
                            }
                            Type::Struct {
                                name: struct_name,
                                args: _,
                            } if struct_name.contains("vec") => {
                                // Convert to method call: container._set(index, value)
                                let method_call = Expr::MethodCall {
                                    object: Some(Box::new(container.as_ref().clone())),
                                    object_type: Some(container_type.clone()),
                                    method: "_set".to_string(),
                                    args: vec![index.as_ref().clone(), value.clone()],
                                };
                                stmt.value =
                                    Stmt::Expression(Positioned::with_unknown_span(method_call));
                                // Visit the new method call
                                if let Stmt::Expression(expr) = &mut stmt.value {
                                    self.visit_expr(expr);
                                }
                                return;
                            }
                            _ => {}
                        }
                    }
                }

                // Default case: visit lvalue
                self.visit_expr(lvalue);
            }
            Stmt::VectorPush {
                vector,
                value,
                vector_type,
            } => {
                // First visit the value expression
                self.visit_expr(value);

                // Check if this is a vec type that should be desugared to method call
                if let Some(ref vtype) = vector_type {
                    if let Type::Struct { name, args: _ } = vtype {
                        if self.supports_operation(name, &DesugarOperation::Push) {
                            // Convert to method call: vector._push(value)
                            let method_name = self
                                .get_method_name(name, &DesugarOperation::Push)
                                .unwrap()
                                .clone();
                            let method_call = Expr::MethodCall {
                                object: Some(Box::new(Positioned::with_unknown_span(
                                    Expr::Identifier(vector.clone()),
                                ))),
                                object_type: Some(vtype.clone()),
                                method: method_name,
                                args: vec![value.clone()],
                            };
                            stmt.value =
                                Stmt::Expression(Positioned::with_unknown_span(method_call));
                            // Visit the new method call
                            if let Stmt::Expression(expr) = &mut stmt.value {
                                self.visit_expr(expr);
                            }
                            return;
                        }
                    }
                }
            }
            _ => {
                // For other statements, use default walking
                walk_stmt_mut(self, stmt);
            }
        }
    }
}

impl DesugarTransformer {
    fn resolve_method_name(&self, object: &Expr, method: &str) -> Result<String> {
        // Try to infer struct type from various contexts
        let struct_name = match object {
            Expr::Identifier(_) => {
                // For now, try all available struct types to see which one has this method
                for (struct_name, struct_decl) in &self.structs {
                    for struct_method in &struct_decl.methods {
                        if struct_method.value.name == method {
                            let struct_type = Type::from_struct_name(struct_name);
                            return Ok(struct_type.mangle_method_name(method));
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
            Expr::StructNew { type_name, .. } => type_name.to_string(),
            Expr::FieldAccess { field, .. } => {
                // Try to find the field type in our struct definitions
                for (_struct_name, struct_decl) in &self.structs {
                    for struct_field in &struct_decl.fields {
                        if struct_field.name == *field {
                            // Check if this field type has the method we're looking for
                            if let Type::Struct {
                                name: field_type_name,
                                ..
                            } = &struct_field.field_type
                            {
                                if let Some(field_struct) = self.structs.get(field_type_name) {
                                    for field_method in &field_struct.methods {
                                        if field_method.value.name == method {
                                            let field_struct_type =
                                                Type::from_struct_name(field_type_name);
                                            return Ok(field_struct_type.mangle_method_name(method));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // If not found, try all available struct types to see which one has this method
                for (struct_name, struct_decl) in &self.structs {
                    for struct_method in &struct_decl.methods {
                        if struct_method.value.name == method {
                            let struct_type = Type::from_struct_name(struct_name);
                            return Ok(struct_type.mangle_method_name(method));
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
            _ => {
                // For other expressions, try all available struct types to see which one has this method
                for (struct_name, struct_decl) in &self.structs {
                    for struct_method in &struct_decl.methods {
                        if struct_method.value.name == method {
                            let struct_type = Type::from_struct_name(struct_name);
                            return Ok(struct_type.mangle_method_name(method));
                        }
                    }
                }
                if let Some((struct_name, _)) = self.structs.iter().next() {
                    struct_name.clone()
                } else {
                    return Err(anyhow::anyhow!(
                        "No struct types available for method resolution"
                    ));
                }
            }
        };

        let struct_type = Type::from_struct_name(&struct_name);
        Ok(struct_type.mangle_method_name(method))
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
            .with_operation(DesugarOperation::Assign, "_set")
            .with_operation(DesugarOperation::New, "_new");
        type_operations.insert("array".to_string(), array_config);

        Self {
            structs: HashMap::new(),
            type_operations,
        }
    }

    /// Desugar a complete program using visitor pattern
    pub fn desugar_program(&mut self, mut program: Program) -> Result<Program> {
        // First pass: collect all struct declarations using visitor
        let mut collector = StructCollector::new();
        for decl in &mut program.declarations {
            collector.visit_decl(decl);
        }
        self.structs = collector.structs;

        // Second pass: desugar all declarations
        let mut desugared_declarations = Vec::new();
        let mut transformer =
            DesugarTransformer::new(self.structs.clone(), self.type_operations.clone());

        for decl in program.declarations {
            match &decl.value {
                Decl::Function(function) => {
                    let mut function_copy = function.value.clone();
                    let desugared_function =
                        self.desugar_function_with_visitor(&mut transformer, &mut function_copy)?;
                    desugared_declarations.push(Positioned::with_unknown_span(Decl::Function(
                        Positioned::with_unknown_span(desugared_function),
                    )));
                }
                Decl::GlobalVariable(_global_var) => {
                    // Global variables don't need desugaring, pass them through
                    desugared_declarations.push(decl);
                }
                Decl::Struct(struct_decl) => {
                    // Add the original struct declaration (without methods), with transformed field types
                    let transformed_fields = struct_decl
                        .value
                        .fields
                        .iter()
                        .map(|field| StructField {
                            name: field.name.clone(),
                            field_type: transformer.transform_type(field.field_type.clone()),
                        })
                        .collect();
                    let transformed_type_params = struct_decl
                        .value
                        .type_params
                        .iter()
                        .map(|t| transformer.transform_type(t.clone()))
                        .collect();
                    let desugared_struct = StructDecl {
                        name: struct_decl.value.name.clone(),
                        type_params: transformed_type_params,
                        fields: transformed_fields,
                        methods: Vec::new(), // Remove methods after desugaring
                    };
                    desugared_declarations.push(Positioned::with_unknown_span(Decl::Struct(
                        Positioned::with_unknown_span(desugared_struct),
                    )));

                    // Convert each method to a standalone function with mangled name
                    for method in &struct_decl.value.methods {
                        let mut method_copy = method.value.clone();
                        let mangled_function = self.desugar_method_with_visitor(
                            &mut transformer,
                            &struct_decl.value.name,
                            &mut method_copy,
                        )?;
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

    /// Desugar a function using visitor pattern
    fn desugar_function_with_visitor(
        &mut self,
        transformer: &mut DesugarTransformer,
        function: &mut Function,
    ) -> Result<Function> {
        // Visit all statements in the function body
        for stmt in &mut function.body {
            transformer.visit_stmt(stmt);
        }

        // Transform function parameter types
        let transformed_params = function
            .params
            .iter()
            .map(|param| FunParam {
                name: param.name.clone(),
                param_type: param
                    .param_type
                    .as_ref()
                    .map(|t| transformer.transform_type(t.clone())),
            })
            .collect();

        // Transform type parameters
        let transformed_type_params = function
            .type_params
            .iter()
            .map(|t| transformer.transform_type(t.clone()))
            .collect();

        // Add return 0; if the last statement is not a return
        if !function.body.is_empty() {
            if let Some(last_stmt) = function.body.last() {
                if !matches!(last_stmt.value, Stmt::Return(_)) {
                    function
                        .body
                        .push(Positioned::with_unknown_span(Stmt::Return(
                            Positioned::with_unknown_span(Expr::Int(0)),
                        )));
                }
            }
        } else {
            // If function body is empty, add return 0;
            function
                .body
                .push(Positioned::with_unknown_span(Stmt::Return(
                    Positioned::with_unknown_span(Expr::Int(0)),
                )));
        }

        Ok(Function {
            name: function.name.clone(),
            type_params: transformed_type_params,
            params: transformed_params,
            body: function.body.clone(),
        })
    }

    /// Convert a struct method to a standalone function with mangled name using visitor pattern
    fn desugar_method_with_visitor(
        &mut self,
        transformer: &mut DesugarTransformer,
        struct_name: &str,
        method: &mut Function,
    ) -> Result<Function> {
        let struct_type = Type::from_struct_name(struct_name);
        let mangled_name = struct_type.mangle_method_name(&method.name);

        // Visit all statements in the method body
        for stmt in &mut method.body {
            transformer.visit_stmt(stmt);
        }

        // Add return 0; if the last statement is not a return
        if !method.body.is_empty() {
            if let Some(last_stmt) = method.body.last() {
                if !matches!(last_stmt.value, Stmt::Return(_)) {
                    method.body.push(Positioned::with_unknown_span(Stmt::Return(
                        Positioned::with_unknown_span(Expr::Int(0)),
                    )));
                }
            }
        } else {
            // If method body is empty, add return 0;
            method.body.push(Positioned::with_unknown_span(Stmt::Return(
                Positioned::with_unknown_span(Expr::Int(0)),
            )));
        }

        Ok(Function {
            name: mangled_name,
            type_params: method.type_params.clone(),
            params: method.params.clone(),
            body: method.body.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunParam, StructNewKind, Type};

    #[test]
    fn test_visitor_method_call_desugaring() {
        let mut desugarer = Desugarer::new();

        // Create a simple program with a struct and method call
        let program = Program {
            declarations: vec![
                Positioned::with_unknown_span(Decl::Struct(Positioned::with_unknown_span(
                    StructDecl {
                        name: "Point".to_string(),
                        type_params: vec![],
                        fields: vec![],
                        methods: vec![Positioned::with_unknown_span(Function {
                            name: "sum".to_string(),
                            type_params: vec![],
                            params: vec![FunParam {
                                name: "self".to_string(),
                                param_type: Some(Type::Struct {
                                    name: "Point".to_string(),
                                    args: vec![],
                                }),
                            }],
                            body: vec![Positioned::with_unknown_span(Stmt::Return(
                                Positioned::with_unknown_span(Expr::Int(42)),
                            ))],
                        })],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "main".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![
                            Positioned::with_unknown_span(Stmt::Let {
                                name: "p".to_string(),
                                value: Positioned::with_unknown_span(Expr::StructNew {
                                    type_name: Type::Struct {
                                        name: "Point".to_string(),
                                        args: vec![],
                                    },
                                    fields: vec![],
                                    kind: StructNewKind::Pattern,
                                }),
                            }),
                            Positioned::with_unknown_span(Stmt::Expression(
                                Positioned::with_unknown_span(Expr::MethodCall {
                                    object: Some(Box::new(Positioned::with_unknown_span(
                                        Expr::Identifier("p".to_string()),
                                    ))),
                                    object_type: Some(Type::Struct {
                                        name: "Point".to_string(),
                                        args: vec![],
                                    }),
                                    method: "sum".to_string(),
                                    args: vec![],
                                }),
                            )),
                        ],
                    },
                ))),
            ],
        };

        let result = desugarer.desugar_program(program).unwrap();

        // Check that Point#sum function was created
        let point_sum_func = result
            .declarations
            .iter()
            .find(|decl| {
                if let Decl::Function(func) = &decl.value {
                    func.value.name == "Point#sum"
                } else {
                    false
                }
            })
            .expect("Point#sum function should exist");

        if let Decl::Function(func) = &point_sum_func.value {
            assert_eq!(func.value.name, "Point#sum");
            assert_eq!(func.value.params.len(), 1);
            assert_eq!(func.value.params[0].name, "self");
        }

        // Check that method call was transformed to function call in main
        let main_func = result
            .declarations
            .iter()
            .find(|decl| {
                if let Decl::Function(func) = &decl.value {
                    func.value.name == "main"
                } else {
                    false
                }
            })
            .expect("main function should exist");

        if let Decl::Function(func) = &main_func.value {
            if let Some(Stmt::Expression(expr)) = func.value.body.get(1).map(|s| &s.value) {
                if let Expr::Call { callee, args } = &expr.value {
                    if let Expr::Identifier(name) = &callee.value {
                        assert_eq!(name, "Point#sum");
                        assert_eq!(args.len(), 1);
                    } else {
                        panic!("Expected function call identifier");
                    }
                } else {
                    panic!("Expected function call");
                }
            } else {
                panic!("Expected expression statement");
            }
        }
    }

    #[test]
    fn test_visitor_string_to_array_conversion() {
        let mut desugarer = Desugarer::new();

        // Create a simple program with a string literal
        let program = Program {
            declarations: vec![Positioned::with_unknown_span(Decl::Function(
                Positioned::with_unknown_span(Function {
                    name: "main".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Positioned::with_unknown_span(Stmt::Let {
                        name: "s".to_string(),
                        value: Positioned::with_unknown_span(Expr::String("hello".to_string())),
                    })],
                }),
            ))],
        };

        let result = desugarer.desugar_program(program).unwrap();

        // Check that string was converted to array(byte) struct
        let main_func = result
            .declarations
            .iter()
            .find(|decl| {
                if let Decl::Function(func) = &decl.value {
                    func.value.name == "main"
                } else {
                    false
                }
            })
            .expect("main function should exist");

        if let Decl::Function(func) = &main_func.value {
            if let Some(Stmt::Let { value, .. }) = func.value.body.get(0).map(|s| &s.value) {
                if let Expr::Call { callee, args } = &value.value {
                    if let Expr::Identifier(name) = &callee.value {
                        assert_eq!(name, "array(byte)#_new_from_data");
                        assert_eq!(args.len(), 2);
                    } else {
                        panic!("Expected array constructor call");
                    }
                } else {
                    panic!("Expected function call for string conversion");
                }
            } else {
                panic!("Expected let statement");
            }
        }
    }

    #[test]
    fn test_visitor_embedded_type_method_call() {
        let mut desugarer = Desugarer::new();

        // Create a program that tests embedded type information
        let program = Program {
            declarations: vec![Positioned::with_unknown_span(Decl::Function(
                Positioned::with_unknown_span(Function {
                    name: "test".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Positioned::with_unknown_span(Stmt::Expression(
                        Positioned::with_unknown_span(Expr::MethodCall {
                            object: Some(Box::new(Positioned::with_unknown_span(
                                Expr::Identifier("p".to_string()),
                            ))),
                            object_type: Some(Type::Struct {
                                name: "Point".to_string(),
                                args: vec![],
                            }),
                            method: "sum".to_string(),
                            args: vec![],
                        }),
                    ))],
                }),
            ))],
        };

        let result = desugarer.desugar_program(program).unwrap();

        // Check that method call was converted using embedded type
        let test_func = result
            .declarations
            .iter()
            .find(|decl| {
                if let Decl::Function(func) = &decl.value {
                    func.value.name == "test"
                } else {
                    false
                }
            })
            .expect("test function should exist");

        if let Decl::Function(func) = &test_func.value {
            if let Some(Stmt::Expression(expr)) = func.value.body.get(0).map(|s| &s.value) {
                if let Expr::Call { callee, args } = &expr.value {
                    if let Expr::Identifier(name) = &callee.value {
                        assert_eq!(name, "Point#sum");
                        assert_eq!(args.len(), 1);
                    } else {
                        panic!("Expected Point#sum function call");
                    }
                } else {
                    panic!("Expected function call");
                }
            }
        }
    }

    #[test]
    fn test_visitor_function_return_auto_insertion() {
        let mut desugarer = Desugarer::new();

        // Create a function without explicit return
        let program = Program {
            declarations: vec![Positioned::with_unknown_span(Decl::Function(
                Positioned::with_unknown_span(Function {
                    name: "test_func".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Positioned::with_unknown_span(Stmt::Let {
                        name: "x".to_string(),
                        value: Positioned::with_unknown_span(Expr::Int(42)),
                    })],
                }),
            ))],
        };

        let result = desugarer.desugar_program(program).unwrap();

        // Check that return 0; was added
        let test_func = result
            .declarations
            .iter()
            .find(|decl| {
                if let Decl::Function(func) = &decl.value {
                    func.value.name == "test_func"
                } else {
                    false
                }
            })
            .expect("test_func should exist");

        if let Decl::Function(func) = &test_func.value {
            assert_eq!(func.value.body.len(), 2);
            if let Some(Stmt::Return(return_expr)) = func.value.body.get(1).map(|s| &s.value) {
                if let Expr::Int(0) = &return_expr.value {
                    // Expected behavior
                } else {
                    panic!("Expected return 0; to be added");
                }
            } else {
                panic!("Expected return statement to be added");
            }
        }
    }
}
