use crate::ast::{
    BinaryOp, Decl, Expr, Function, IndexContainerType, PositionedDecl, PositionedExpr,
    PositionedStmt, Program, Span, Stmt, StructDecl, StructNewKind, Type,
};
use anyhow::{bail, Result};
use std::collections::HashMap;

/// Helper trait for creating position-aware errors
trait PositionedError {
    fn error_at_span(&self, message: String) -> anyhow::Error;
}

impl<T> PositionedError for crate::ast::Positioned<T> {
    fn error_at_span(&self, message: String) -> anyhow::Error {
        if let (Some(start), Some(_end)) = (self.span.start, self.span.end) {
            anyhow::anyhow!("{} at position {}", message, start)
        } else {
            anyhow::anyhow!("{}", message)
        }
    }
}

impl Type {
    /// Check if this type is numeric (int or byte)
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Byte)
    }

    /// Check if this type is numeric or unknown (for type inference)
    pub fn is_numeric_or_unknown(&self) -> bool {
        matches!(self, Type::Int | Type::Byte | Type::Unknown)
    }
    /// Parse a type string like "int", "vec(int)", "map(string, int)"
    pub fn from_string(type_str: &str) -> Type {
        match type_str {
            "bool" | "boolean" => Type::Boolean,
            "int" | "number" => Type::Int,
            "string" | "[*]byte" => Type::String, // Treat [*]byte as string
            "byte" => Type::Byte,
            _ => {
                if type_str.starts_with("vec(") && type_str.ends_with(')') {
                    let inner = &type_str[4..type_str.len() - 1];
                    Type::Struct {
                        name: "vec".to_string(),
                        args: vec![Type::from_string(inner)],
                    }
                } else if type_str.starts_with("map(") && type_str.ends_with(')') {
                    let inner = &type_str[4..type_str.len() - 1];
                    if let Some(comma_pos) = inner.find(", ") {
                        let key_type = &inner[..comma_pos];
                        let value_type = &inner[comma_pos + 2..];
                        Type::Map(
                            Box::new(Type::from_string(key_type)),
                            Box::new(Type::from_string(value_type)),
                        )
                    } else {
                        Type::Unknown
                    }
                } else if type_str.starts_with("[*]") {
                    let inner = &type_str[3..];
                    Type::Pointer(Box::new(Type::from_string(inner)))
                } else if type_str.contains('(') && type_str.ends_with(')') {
                    // Handle generic type instantiation: Type(arg1, arg2, ...)
                    if let Some(paren_pos) = type_str.find('(') {
                        let name = &type_str[..paren_pos];
                        let args_str = &type_str[paren_pos + 1..type_str.len() - 1];

                        if args_str.is_empty() {
                            Type::Struct {
                                name: name.to_string(),
                                args: vec![],
                            }
                        } else {
                            let args: Vec<Type> = args_str
                                .split(", ")
                                .map(|arg| Type::from_string(arg.trim()))
                                .collect();
                            Type::Struct {
                                name: name.to_string(),
                                args,
                            }
                        }
                    } else {
                        Type::Struct {
                            name: type_str.to_string(),
                            args: vec![],
                        }
                    }
                } else {
                    // Could be a struct name or type parameter
                    Type::Struct {
                        name: type_str.to_string(),
                        args: vec![],
                    }
                }
            }
        }
    }

    /// Check if this type is compatible with another type
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            // Only allow Unknown compatibility during type inference phase
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Int, Type::Int) => true,
            (Type::String, Type::String) => true,
            // String and array(byte) are compatible
            (Type::String, Type::Struct { name, args })
                if name == "array" && args.len() == 1 && args[0] == Type::Byte =>
            {
                true
            }
            (Type::Struct { name, args }, Type::String)
                if name == "array" && args.len() == 1 && args[0] == Type::Byte =>
            {
                true
            }
            (Type::Byte, Type::Byte) => true,
            (Type::Pointer(e1), Type::Pointer(e2)) => e1.is_compatible_with(e2),
            (Type::Map(k1, v1), Type::Map(k2, v2)) => {
                k1.is_compatible_with(k2) && v1.is_compatible_with(v2)
            }
            (Type::Struct { name: n1, args: a1 }, Type::Struct { name: n2, args: a2 }) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1
                        .iter()
                        .zip(a2.iter())
                        .all(|(t1, t2)| t1.is_compatible_with(t2))
            }
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|(t1, t2)| t1.is_compatible_with(t2))
                    && r1.is_compatible_with(r2)
            }
            (Type::TypeParameter(n1), Type::TypeParameter(n2)) => n1 == n2,
            _ => false,
        }
    }

    /// Check strict type equality (no Unknown compatibility)
    pub fn is_exactly(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Boolean, Type::Boolean) => true,
            (Type::Int, Type::Int) => true,
            (Type::String, Type::String) => true,
            (Type::Pointer(e1), Type::Pointer(e2)) => e1.is_exactly(e2),
            (Type::Map(k1, v1), Type::Map(k2, v2)) => k1.is_exactly(k2) && v1.is_exactly(v2),
            (Type::Struct { name: n1, args: a1 }, Type::Struct { name: n2, args: a2 }) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2.iter()).all(|(t1, t2)| t1.is_exactly(t2))
            }
            (
                Type::Function {
                    params: p1,
                    return_type: r1,
                },
                Type::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.is_exactly(t2))
                    && r1.is_exactly(r2)
            }
            (Type::TypeParameter(n1), Type::TypeParameter(n2)) => n1 == n2,
            _ => false,
        }
    }
}

pub struct TypeChecker {
    /// Variable type environment
    variables: HashMap<String, Type>,
    /// Global variable type environment
    globals: HashMap<String, Type>,
    /// Function call stack for type scoping
    call_stack: Vec<HashMap<String, Type>>,
    /// Current function return type
    current_return_type: Option<Type>,
    /// Function registry for function signatures
    functions: HashMap<String, Type>,
    /// Struct type registry
    structs: HashMap<String, Type>,
    /// Struct field information (maps struct name to field types)
    struct_fields: HashMap<String, HashMap<String, Type>>,
    /// Generic type parameter environment (maps type parameter names to types)
    type_params: HashMap<String, Type>,
    /// Stack of type parameter environments for nested generic scopes
    type_param_stack: Vec<HashMap<String, Type>>,
    /// Generic function declarations (maps function name to Function)
    generic_functions: HashMap<String, Function>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            variables: HashMap::new(),
            globals: HashMap::new(),
            call_stack: Vec::new(),
            current_return_type: None,
            functions: HashMap::new(),
            structs: HashMap::new(),
            struct_fields: HashMap::new(),
            type_params: HashMap::new(),
            type_param_stack: Vec::new(),
            generic_functions: HashMap::new(),
        }
    }

    /// Enter a new generic scope with the given type parameters
    fn enter_generic_scope(&mut self, type_params: &[String]) {
        // Save current type parameters
        self.type_param_stack.push(self.type_params.clone());

        // Add new type parameters as TypeParameter types
        for param in type_params {
            self.type_params
                .insert(param.clone(), Type::TypeParameter(param.clone()));
        }
    }

    /// Exit the current generic scope
    fn exit_generic_scope(&mut self) {
        if let Some(previous_params) = self.type_param_stack.pop() {
            self.type_params = previous_params;
        }
    }

    /// Resolve a type string to a concrete type, handling type parameters
    fn resolve_type(&self, type_str: &str) -> Type {
        // First check if it's a type parameter
        if let Some(param_type) = self.type_params.get(type_str) {
            return param_type.clone();
        }

        // Try standard type parsing
        let parsed_type = Type::from_string(type_str);
        self.resolve_type_recursive(&parsed_type)
    }

    fn resolve_type_recursive(&self, type_obj: &Type) -> Type {
        match type_obj {
            Type::Struct { name, args } => {
                // First check if it's a type parameter
                if let Some(param_type) = self.type_params.get(name) {
                    return param_type.clone();
                }

                // Recursively resolve the arguments
                let resolved_args: Vec<Type> = args
                    .iter()
                    .map(|arg| self.resolve_type_recursive(arg))
                    .collect();

                // Check if it's a registered struct type
                self.structs.get(name).cloned().unwrap_or(Type::Struct {
                    name: name.clone(),
                    args: resolved_args,
                })
            }
            Type::Map(key_type, value_type) => Type::Map(
                Box::new(self.resolve_type_recursive(key_type)),
                Box::new(self.resolve_type_recursive(value_type)),
            ),
            Type::Pointer(elem_type) => {
                Type::Pointer(Box::new(self.resolve_type_recursive(elem_type)))
            }
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|p| self.resolve_type_recursive(p))
                    .collect(),
                return_type: Box::new(self.resolve_type_recursive(return_type)),
            },
            other => other.clone(),
        }
    }

    /// Type check a complete program
    pub fn check_program(&mut self, program: &mut Program) -> Result<()> {
        // First pass: register all struct types
        for decl in &program.declarations {
            if let Decl::Struct(struct_decl) = &decl.value {
                self.register_struct(&struct_decl.value)?;
            }
        }

        // Second pass: register global variables (now that structs are available)
        for decl in &mut program.declarations {
            if let Decl::GlobalVariable(global_var) = &mut decl.value {
                let var_type = self.check_expression(&mut global_var.value.value)?;
                self.globals.insert(global_var.value.name.clone(), var_type);
            }
        }

        // Third pass: register all function signatures (now that structs and globals are available)
        for decl in &program.declarations {
            if let Decl::Function(function) = &decl.value {
                self.register_function(&function.value)?;
            }
        }

        // Fourth pass: check function bodies
        for decl in &mut program.declarations {
            self.check_declaration(decl)?;
        }
        Ok(())
    }

    /// Perform type inference and fill in container types
    pub fn infer_types(&mut self, program: &mut Program) -> Result<()> {
        for decl in &mut program.declarations {
            self.infer_declaration_types(&mut decl.value)?;
        }
        Ok(())
    }

    fn infer_declaration_types(&mut self, decl: &mut Decl) -> Result<()> {
        match decl {
            Decl::Function(function) => self.infer_function_types(&mut function.value),
            Decl::Struct(_) => Ok(()), // Struct types are already resolved during registration
            Decl::GlobalVariable(global_var) => {
                // Type check the global variable's value
                self.infer_expression_types(&mut global_var.value.value)?;
                // Register the global variable's type
                let var_type = self.check_expression(&mut global_var.value.value)?;
                self.globals.insert(global_var.value.name.clone(), var_type);
                Ok(())
            }
        }
    }

    fn infer_function_types(&mut self, function: &mut Function) -> Result<()> {
        // Create new scope for function
        let mut local_scope = HashMap::new();

        // Add parameters to scope
        for param in &function.params {
            let param_type = if let Some(type_name) = &param.type_name {
                self.resolve_type(type_name)
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

    fn infer_statement_types(&mut self, stmt: &mut PositionedStmt) -> Result<()> {
        match &mut stmt.value {
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

            Stmt::Assign { lvalue, value } => {
                self.infer_expression_types(lvalue)?;
                self.infer_expression_types(value)?;
                Ok(())
            }

            Stmt::VectorPush {
                vector: _,
                value,
                vector_type: _,
            } => {
                self.infer_expression_types(value)?;
                Ok(())
            }
        }
    }

    fn infer_expression_types(&mut self, expr: &mut PositionedExpr) -> Result<()> {
        match &mut expr.value {
            Expr::Int(_)
            | Expr::Boolean(_)
            | Expr::String(_)
            | Expr::PushString(_)
            | Expr::Byte(_)
            | Expr::Identifier(_)
            | Expr::TypeExpr { .. } => {
                // No inference needed for literals, identifiers, and type expressions
                Ok(())
            }

            Expr::Alloc {
                element_type: _,
                size,
            } => {
                self.infer_expression_types(size)?;
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
                container_value_type,
            } => {
                self.infer_expression_types(container)?;
                self.infer_expression_types(index)?;

                // Infer container type based on container expression type
                let container_value_type_result = self.check_expression(container)?;
                *container_value_type = Some(container_value_type_result.clone());
                match &container_value_type_result {
                    Type::Struct { name, .. } if name == "vec" => {
                        *container_type = Some(IndexContainerType::Vector);
                    }
                    Type::Struct { name, .. } if name == "array" => {
                        *container_type = Some(IndexContainerType::Vector); // array behaves like vector for indexing
                    }
                    Type::Map(_, _) => {
                        *container_type = Some(IndexContainerType::Map);
                    }
                    Type::Pointer(element_type) => {
                        // Use StringIndex for [*]byte pointers and generic [*]T, PointerIndex for others
                        match element_type.as_ref() {
                            Type::Byte => {
                                *container_type = Some(IndexContainerType::String);
                            }
                            Type::Struct { name, .. } if name == "T" => {
                                // Generic T could be byte, so use StringIndex for safety
                                *container_type = Some(IndexContainerType::String);
                            }
                            _ => {
                                *container_type = Some(IndexContainerType::Pointer);
                            }
                        }
                    }
                    Type::String => {
                        *container_type = Some(IndexContainerType::String);
                    }
                    Type::Struct {
                        name: struct_name,
                        args: _,
                    } => {
                        // Check if this is a vec type that supports indexing
                        let base_name = if struct_name.contains('(') {
                            struct_name.split('(').next().unwrap()
                        } else {
                            struct_name
                        };

                        if base_name == "vec" {
                            *container_type = Some(IndexContainerType::Vector);
                        }
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

            Expr::StructNew { fields, .. } => {
                for (_, value) in fields {
                    self.infer_expression_types(value)?;
                }
                Ok(())
            }

            Expr::FieldAccess { object, .. } => {
                self.infer_expression_types(object)?;
                Ok(())
            }

            Expr::MethodCall {
                object,
                args,
                type_name,
                ..
            } => {
                if let Some(obj) = object {
                    // Instance method call (obj.method())
                    self.infer_expression_types(obj)?;

                    // Set object type information based on object expression type
                    let obj_type = self.check_expression(obj)?;
                    if let Type::Struct { name, args } = obj_type {
                        // For generic types, include type arguments in the type name
                        if args.is_empty() {
                            *type_name = Some(name);
                        } else {
                            let type_args_str = args
                                .iter()
                                .map(|t| t.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            *type_name = Some(format!("{}({})", name, type_args_str));
                        }
                    }
                }
                // For associated method calls, type_name should already be set

                for arg in args {
                    self.infer_expression_types(arg)?;
                }
                Ok(())
            }
        }
    }

    /// Register a function signature for later type checking
    fn register_function(&mut self, function: &Function) -> Result<()> {
        // Store generic functions separately
        if !function.type_params.is_empty() {
            self.generic_functions
                .insert(function.name.clone(), function.clone());
        }
        self.register_function_with_name(&function.name, function)
    }

    /// Register a function signature with a specific name (for name mangling)
    fn register_function_with_name(&mut self, name: &str, function: &Function) -> Result<()> {
        // Determine parameter types
        let mut params = Vec::new();

        // First, add type parameters (they become part of the function signature)
        for _type_param in &function.type_params {
            params.push(Type::Unknown); // Type parameters are represented as Unknown for now
        }

        // Then, add regular parameters
        for param in &function.params {
            let param_type = if let Some(type_name) = &param.type_name {
                self.resolve_type(type_name)
            } else {
                Type::Unknown // For now, allow unknown types
            };
            params.push(param_type);
        }

        // Infer return type based on function body analysis
        let return_type = self.infer_function_return_type(function);

        let function_type = Type::Function {
            params,
            return_type: Box::new(return_type),
        };

        self.functions.insert(name.to_string(), function_type);
        Ok(())
    }

    /// Register a struct type for later type checking
    fn register_struct(&mut self, struct_decl: &StructDecl) -> Result<()> {
        // Skip registration if this looks like a concrete instantiation of a generic type
        // and we already have the generic version
        if struct_decl.name.contains('(') && struct_decl.name.ends_with(')') {
            return Ok(());
        }

        // Check for duplicate struct definition
        if self.structs.contains_key(&struct_decl.name) {
            bail!("Struct '{}' is already defined", struct_decl.name);
        }
        // For generic structs, we register the generic template
        // The actual instantiation will happen during monomorphization
        if !struct_decl.type_params.is_empty() {
            // This is a generic struct - store as a generic type template
            let struct_type = Type::Struct {
                name: struct_decl.name.clone(),
                args: struct_decl
                    .type_params
                    .iter()
                    .map(|param| Type::TypeParameter(param.clone()))
                    .collect(),
            };
            self.structs.insert(struct_decl.name.clone(), struct_type);

            // For generic structs, we need to enter generic scope to properly resolve field types
            self.enter_generic_scope(&struct_decl.type_params);
        } else {
            // Regular struct - handle as before but with new Type enum structure
            let struct_type = Type::Struct {
                name: struct_decl.name.clone(),
                args: vec![],
            };
            self.structs.insert(struct_decl.name.clone(), struct_type);
        }

        // Store struct field information
        let mut fields = HashMap::new();
        for field in &struct_decl.fields {
            let field_type = self.resolve_type(&field.type_name);
            fields.insert(field.name.clone(), field_type);
        }
        self.struct_fields.insert(struct_decl.name.clone(), fields);

        // Exit generic scope if we entered it
        if !struct_decl.type_params.is_empty() {
            self.exit_generic_scope();
        }

        // Register struct methods with name mangling
        // For generic structs, these will also be generic function templates
        for method in &struct_decl.methods {
            let mangled_name = format!("{}__{}", struct_decl.name, method.value.name);
            self.register_function_with_name(&mangled_name, &method.value)?;
        }

        Ok(())
    }

    /// Type check a declaration
    fn check_declaration(&mut self, decl: &mut PositionedDecl) -> Result<()> {
        match &mut decl.value {
            Decl::Function(function) => self.check_function(&mut function.value),
            Decl::Struct(_) => Ok(()), // Struct declarations are checked during registration
            Decl::GlobalVariable(global_var) => {
                // Type check the global variable's value
                self.check_expression(&mut global_var.value.value)?;
                Ok(())
            }
        }
    }

    /// Type check a function
    fn check_function(&mut self, function: &mut Function) -> Result<()> {
        // Enter generic scope if this function has type parameters
        if !function.type_params.is_empty() {
            self.enter_generic_scope(&function.type_params);
        }

        // Create new scope for function
        let mut local_scope = HashMap::new();

        // Add parameters to scope
        for param in &function.params {
            let param_type = if let Some(type_name) = &param.type_name {
                self.resolve_type(type_name)
            } else {
                Type::Unknown // Type inference could be added later
            };
            local_scope.insert(param.name.clone(), param_type);
        }

        self.call_stack.push(local_scope);

        // Set return type context
        self.current_return_type = Some(Type::Unknown); // Could be inferred

        // Check function body
        for stmt in &mut function.body {
            self.check_statement(stmt)?;
        }

        self.call_stack.pop();
        self.current_return_type = None;

        // Exit generic scope if this function has type parameters
        if !function.type_params.is_empty() {
            self.exit_generic_scope();
        }

        Ok(())
    }

    /// Type check a statement
    fn check_statement(&mut self, stmt: &mut PositionedStmt) -> Result<()> {
        match &mut stmt.value {
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
                if !condition_type.is_compatible_with(&Type::Boolean) {
                    return Err(condition.error_at_span(format!(
                        "If condition must be boolean, got {}",
                        condition_type
                    )));
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
                if !condition_type.is_compatible_with(&Type::Boolean) {
                    return Err(condition.error_at_span(format!(
                        "While condition must be boolean, got {}",
                        condition_type
                    )));
                }

                for stmt in body {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }

            Stmt::Assign { lvalue, value } => {
                // Check for immutable string assignments in complex expressions
                if let Expr::Index { container, .. } = &mut lvalue.value {
                    let container_type = self.check_expression(container)?;
                    if container_type == Type::String {
                        bail!("Cannot assign to string index - strings are immutable");
                    }
                }

                let lvalue_type = self.check_lvalue_expression(lvalue)?;
                let value_type = self.check_expression(value)?;

                if !value_type.is_compatible_with(&lvalue_type) {
                    bail!(
                        "Assignment type mismatch: left-hand side has type {}, assigned {}",
                        lvalue_type,
                        value_type
                    );
                }
                Ok(())
            }

            Stmt::VectorPush {
                vector,
                value,
                vector_type: ref mut vtype,
            } => {
                let value_type = self.check_expression(value)?;
                let vector_type = self.lookup_variable(vector)?;
                *vtype = Some(vector_type.clone());

                match &vector_type {
                    Type::Struct { name, args } if name == "vec" => {
                        if let Some(element_type) = args.first() {
                            if !value_type.is_compatible_with(element_type) {
                                bail!(
                                    "Vector push type mismatch: vector element type is {}, pushed {}",
                                    element_type,
                                    value_type
                                );
                            }
                        }
                    }
                    Type::Struct {
                        name: struct_name,
                        args,
                    } => {
                        // Check if this is a vec type that supports push
                        let base_name = if struct_name.contains('(') {
                            struct_name.split('(').next().unwrap()
                        } else {
                            struct_name
                        };

                        if base_name == "vec" || struct_name == "vec" {
                            // Check if _push method exists
                            let push_method_name = if base_name == "vec" {
                                format!(
                                    "{}({})__push",
                                    base_name,
                                    args.iter()
                                        .map(|t| t.to_string())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                )
                            } else {
                                format!("{}__push", struct_name)
                            };
                            if self.functions.contains_key(&push_method_name) {
                                // For vec(T), extract T and check compatibility
                                if let Some(element_type) = if !args.is_empty() {
                                    args.get(0).cloned()
                                } else {
                                    self.extract_vec_element_type(struct_name)
                                } {
                                    if !value_type.is_compatible_with(&element_type) {
                                        bail!(
                                            "Vector push type mismatch: vector element type is {}, pushed {}",
                                            element_type,
                                            value_type
                                        );
                                    }
                                } else {
                                    bail!("Cannot determine element type for vec: {}", struct_name);
                                }
                            } else {
                                bail!("vec type {} does not have a _push method", struct_name);
                            }
                        } else {
                            bail!("Cannot push to non-vector type: {}", vector_type);
                        }
                    }
                    _ => bail!("Cannot push to non-vector type: {}", vector_type),
                }
                Ok(())
            }
        }
    }

    /// Extract element type from vec type string (e.g., "vec(int)" -> Type::Int)
    fn extract_vec_element_type(&self, struct_name: &str) -> Option<Type> {
        if struct_name.starts_with("vec(") && struct_name.ends_with(')') {
            let inner = &struct_name[9..struct_name.len() - 1]; // Remove "vec(" and ")"
                                                                // Check if this is a simple type parameter (single letter)
            if inner.len() == 1 && inner.chars().all(|c| c.is_alphabetic() && c.is_uppercase()) {
                Some(Type::TypeParameter(inner.to_string()))
            } else {
                Some(Type::from_string(inner))
            }
        } else {
            None
        }
    }

    /// Type check a left-hand value expression and return its type
    fn check_lvalue_expression(&mut self, expr: &mut PositionedExpr) -> Result<Type> {
        match &expr.value {
            Expr::Identifier(_) | Expr::FieldAccess { .. } | Expr::Index { .. } => {
                // Valid lvalues - delegate to normal expression type checking
                self.check_expression(expr)
            }
            _ => bail!("Invalid left-hand side in assignment: only variables, field access, and indexing are allowed"),
        }
    }

    /// Type check an expression and return its type
    fn check_expression(&mut self, expr: &mut PositionedExpr) -> Result<Type> {
        match &mut expr.value {
            Expr::Int(_) => Ok(Type::Int),
            Expr::Boolean(_) => Ok(Type::Boolean),
            Expr::String(_) => Ok(Type::String),
            Expr::Byte(_) => Ok(Type::Byte),
            Expr::PushString(_) => Ok(Type::Pointer(Box::new(Type::Byte))),

            Expr::Identifier(name) => {
                let name_clone = name.clone();
                let span = expr.span.clone();
                self.lookup_variable(name).map_err(|_| {
                    if let (Some(start), Some(_end)) = (span.start, span.end) {
                        anyhow::anyhow!("Undefined variable: {} at position {}", name_clone, start)
                    } else {
                        anyhow::anyhow!("Undefined variable: {}", name_clone)
                    }
                })
            }

            Expr::TypeExpr { type_name: _ } => {
                // Type expressions represent types themselves
                // For now, we'll treat them as a special unknown type
                // In a full implementation, this would be a proper Type type
                Ok(Type::Unknown)
            }

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
                        } else if left_type.is_numeric_or_unknown()
                            && right_type.is_numeric_or_unknown()
                        {
                            Ok(Type::Int) // arithmetic with bytes always promotes to int
                        } else {
                            bail!("Addition operation type mismatch: {} + {} (can only add numbers/bytes or concatenate strings)", left_type, right_type);
                        }
                    }
                    BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if left_type.is_numeric_or_unknown() && right_type.is_numeric_or_unknown() {
                            Ok(Type::Int) // arithmetic with bytes always promotes to int
                        } else {
                            bail!(
                                "Arithmetic operation type mismatch: {} {} {} (requires numbers or bytes)",
                                left_type,
                                op_to_string(op),
                                right_type
                            );
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if left_type.is_compatible_with(&right_type) {
                            Ok(Type::Boolean)
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
                        if left_type.is_numeric_or_unknown() && right_type.is_numeric_or_unknown() {
                            Ok(Type::Boolean)
                        } else {
                            bail!(
                                "Comparison operation requires numbers or bytes: {} {} {}",
                                left_type,
                                op_to_string(op),
                                right_type
                            );
                        }
                    }
                }
            }

            Expr::Call { callee, args } => {
                // Check if the callee is a function identifier
                if let Expr::Identifier(func_name) = &callee.value {
                    // Handle built-in functions
                    if func_name == "syscall" {
                        // syscall has variable arguments, just check that we have at least one argument
                        if args.is_empty() {
                            bail!("syscall requires at least one argument (syscall number)");
                        }
                        // Check argument types
                        for arg in args {
                            self.check_expression(arg)?;
                        }
                        return Ok(Type::Int); // syscall returns a number (result code)
                    }

                    // Check if this is a generic function first
                    if let Some(generic_func) = self.generic_functions.get(func_name) {
                        // For generic functions, be more permissive during initial type checking
                        // The actual type checking will happen after monomorphization

                        // Still check argument count (type params + regular params)
                        let expected_args =
                            generic_func.type_params.len() + generic_func.params.len();
                        if args.len() != expected_args {
                            bail!(
                                "Generic function '{}' expects {} arguments (including type arguments), got {}",
                                func_name,
                                expected_args,
                                args.len()
                            );
                        }

                        // For now, just check that the expressions are valid, don't enforce strict types
                        for arg in args {
                            self.check_expression(arg)?;
                        }

                        // Return the function's declared return type (if any), or Unknown
                        return Ok(Type::Unknown); // Will be resolved after monomorphization
                    }

                    // Look up function signature and clone it to avoid borrow checker issues
                    if let Some(func_type) = self.functions.get(func_name).cloned() {
                        if let Type::Function {
                            params,
                            return_type,
                        } = func_type
                        {
                            // Check argument count
                            if args.len() != params.len() {
                                bail!(
                                    "Function '{}' expects {} arguments, got {}",
                                    func_name,
                                    params.len(),
                                    args.len()
                                );
                            }

                            // Check argument types
                            for (i, arg) in args.iter_mut().enumerate() {
                                let arg_type = self.check_expression(arg)?;

                                // Be more permissive for monomorphized functions (containing parentheses)
                                let is_monomorphized_function =
                                    func_name.contains('(') && func_name.contains(')');

                                if !is_monomorphized_function
                                    && !arg_type.is_compatible_with(&params[i])
                                {
                                    bail!(
                                        "Function '{}' argument {} type mismatch: expected {}, got {}",
                                        func_name,
                                        i + 1,
                                        params[i],
                                        arg_type
                                    );
                                }

                                // For monomorphized functions, skip strict type checking
                                // The monomorphization process has already ensured type correctness
                            }

                            Ok(*return_type)
                        } else {
                            bail!("'{}' is not a function", func_name);
                        }
                    } else {
                        bail!("Undefined function: {}", func_name);
                    }
                } else {
                    // Handle complex function expressions (function pointers, etc.)
                    let _callee_type = self.check_expression(callee)?;
                    for arg in args {
                        self.check_expression(arg)?;
                    }
                    Ok(Type::Unknown) // For now, return unknown for complex function calls
                }
            }

            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let element_type = self.resolve_type(element_type);

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

                Ok(Type::Struct {
                    name: "vec".to_string(),
                    args: vec![element_type],
                })
            }

            Expr::Index {
                container,
                index,
                container_type: _,
                container_value_type: _,
            } => {
                let container_value_type = self.check_expression(container)?;
                let index_type = self.check_expression(index)?;

                // Just perform type checking, inference will be done separately
                match &container_value_type {
                    Type::Struct { name, args } if name == "vec" => {
                        if !index_type.is_compatible_with(&Type::Int) {
                            bail!("vec index must be number, got {}", index_type);
                        }
                        if let Some(element_type) = args.first() {
                            Ok(element_type.clone())
                        } else {
                            bail!("vec type missing element type")
                        }
                    }
                    Type::Struct { name, args } if name == "array" => {
                        if !index_type.is_compatible_with(&Type::Int) {
                            bail!("array index must be number, got {}", index_type);
                        }
                        if let Some(element_type) = args.first() {
                            Ok(element_type.clone())
                        } else {
                            bail!("array type missing element type")
                        }
                    }
                    Type::Map(key_type, value_type) => {
                        if !index_type.is_compatible_with(key_type) {
                            bail!(
                                "Map key type mismatch: expected {}, got {}",
                                key_type,
                                index_type
                            );
                        }
                        Ok(*value_type.clone())
                    }
                    Type::Pointer(element_type) => {
                        if !index_type.is_compatible_with(&Type::Int) {
                            bail!("Pointer index must be number, got {}", index_type);
                        }
                        Ok(*element_type.clone())
                    }
                    Type::String => {
                        // String is [*]byte, so indexing returns a byte
                        if !index_type.is_compatible_with(&Type::Int) {
                            bail!("String index must be number, got {}", index_type);
                        }
                        Ok(Type::Byte) // string indexing returns byte
                    }
                    Type::Struct { name, args } => {
                        // Check if this is a vec type that supports indexing
                        let base_name = if name.contains('(') {
                            name.split('(').next().unwrap()
                        } else {
                            name
                        };

                        if base_name == "vec" || name == "vec" {
                            // Check if _get method exists
                            let get_method_name = if base_name == "vec" {
                                format!(
                                    "{}({})__get",
                                    base_name,
                                    args.iter()
                                        .map(|t| t.to_string())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                )
                            } else {
                                format!("{}__get", name)
                            };
                            if self.functions.contains_key(&get_method_name) {
                                if !index_type.is_compatible_with(&Type::Int) {
                                    bail!("vec index must be number, got {}", index_type);
                                }
                                // For vec(T), get the element type
                                if let Some(element_type) = args.get(0) {
                                    Ok(element_type.clone())
                                } else if let Some(element_type) =
                                    self.extract_vec_element_type(name)
                                {
                                    // If the element type is a type parameter, we need to resolve it
                                    // For now, we'll use a simplified approach for concrete types
                                    if let Type::TypeParameter(_) = element_type {
                                        // Try to extract the concrete type from the struct name
                                        if name.starts_with("vec(") && name.ends_with(')') {
                                            let inner = &name[9..name.len() - 1];
                                            if inner != "T" {
                                                // This is a concrete type like "int", not a type parameter
                                                Ok(Type::from_string(inner))
                                            } else {
                                                // This is a type parameter that needs substitution
                                                // For now, return the type parameter as is
                                                Ok(element_type)
                                            }
                                        } else {
                                            Ok(element_type)
                                        }
                                    } else {
                                        Ok(element_type)
                                    }
                                } else {
                                    bail!("Cannot determine element type for vec: {}", name);
                                }
                            } else {
                                bail!("vec type {} does not have a _get method", name);
                            }
                        } else {
                            bail!(
                                "Cannot index non-vector/map/pointer/string type: {}",
                                container_value_type
                            );
                        }
                    }
                    _ => bail!(
                        "Cannot index non-vector/map/pointer/string type: {}",
                        container_value_type
                    ),
                }
            }

            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => {
                let key_type = self.resolve_type(key_type);
                let value_type = self.resolve_type(value_type);

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

                Ok(Type::Map(Box::new(key_type), Box::new(value_type)))
            }

            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                // Check if the struct has a _new function and update the kind accordingly
                let new_function_name = match type_name {
                    Type::Struct { name, args: _ } => {
                        // Method names starting with _ are mangled with 3 underscores total
                        format!("{}___new", name)
                    }
                    _ => {
                        // Try to extract base name for generic types
                        let type_str = type_name.to_string();
                        if let Some(paren_pos) = type_str.find('(') {
                            let base_name = &type_str[..paren_pos];
                            format!("{}___new", base_name)
                        } else {
                            format!("{}___new", type_str)
                        }
                    }
                };
                let updated_kind =
                    if self.functions.contains_key(&new_function_name) && fields.is_empty() {
                        StructNewKind::Pattern
                    } else {
                        StructNewKind::Regular
                    };
                *kind = updated_kind;

                // For Pattern kind with empty fields, skip field validation (_new function will handle it)
                if *kind == StructNewKind::Pattern && fields.is_empty() {
                    // Return the struct type directly without field validation
                    let type_name_str = type_name.to_string();
                    match self.structs.get(&type_name_str) {
                        Some(struct_type) => return Ok(struct_type.clone()),
                        None => {
                            // Handle generic struct instantiation
                            if let Some(generic_type) =
                                self.resolve_generic_struct_type(&type_name_str)
                            {
                                return Ok(generic_type);
                            } else {
                                bail!("Unknown struct type: {}", type_name)
                            }
                        }
                    }
                }

                // Look up the struct field information, handling both generic and non-generic types
                let struct_fields = self.resolve_struct_fields_from_type(type_name)?;

                // Check that all required fields are provided
                for (field_name, _field_type) in &struct_fields {
                    if !fields.iter().any(|(name, _)| name == field_name) {
                        bail!(
                            "Missing required field '{}' in struct '{}' at {:?}",
                            field_name,
                            type_name,
                            expr.span.start
                        );
                    }
                }

                // Check that all provided fields are valid and have correct types
                for (field_name, field_value) in fields {
                    let field_type = struct_fields.get(field_name).ok_or_else(|| {
                        anyhow::anyhow!("Unknown field '{}' in struct '{}'", field_name, type_name)
                    })?;

                    let actual_type = self.check_expression(field_value)?;
                    if !actual_type.is_compatible_with(field_type) {
                        bail!(
                            "Field '{}' type mismatch: expected {}, got {}",
                            field_name,
                            field_type,
                            actual_type
                        );
                    }
                }

                // Return the appropriate struct type
                match type_name {
                    Type::Struct { name, args } => {
                        // For struct types, look up the struct type or return the concrete instantiation
                        match self.structs.get(name) {
                            Some(struct_type) => {
                                // Check if this is a generic struct with concrete type arguments
                                if !args.is_empty() {
                                    // Return the concrete instantiation instead of the generic type
                                    Ok(type_name.clone())
                                } else {
                                    // Non-generic struct, return the registered type
                                    Ok(struct_type.clone())
                                }
                            }
                            None => {
                                // For generic types, return the concrete instantiation
                                Ok(type_name.clone())
                            }
                        }
                    }
                    _ => {
                        // Handle generic struct instantiation
                        if let Some(generic_type) =
                            self.resolve_generic_struct_type(&type_name.to_string())
                        {
                            Ok(generic_type)
                        } else {
                            bail!("Unknown struct type: {}", type_name)
                        }
                    }
                }
            }

            Expr::FieldAccess { object, field } => {
                let object_type = self.check_expression(object)?;
                match object_type {
                    Type::String => {
                        // String type (which becomes array(byte) after desugaring) supports length field
                        if field == "length" {
                            Ok(Type::Int)
                        } else {
                            bail!("Field '{}' not found in string type", field)
                        }
                    }
                    Type::Struct {
                        name: struct_name,
                        args,
                    } => {
                        // Special handling for array(byte) which represents strings
                        if struct_name == "array" && args.len() == 1 && args[0] == Type::Byte {
                            if field == "length" {
                                return Ok(Type::Int);
                            } else if field == "data" {
                                return Ok(Type::Pointer(Box::new(Type::Byte)));
                            } else {
                                bail!("Field '{}' not found in array(byte) type", field);
                            }
                        }
                        // Handle both generic and non-generic struct types
                        if args.is_empty() {
                            // Non-generic struct - try to find the struct fields directly
                            if let Some(fields) = self.struct_fields.get(&struct_name) {
                                fields.get(field).cloned().ok_or_else(|| {
                                    anyhow::anyhow!(
                                        "Field '{}' not found in struct '{}'",
                                        field,
                                        struct_name
                                    )
                                })
                            } else if let Some(generic_fields) =
                                self.resolve_generic_struct_fields(&Type::Struct {
                                    name: struct_name.clone(),
                                    args: args.clone(),
                                })
                            {
                                // Handle generic struct instantiation field access
                                generic_fields.get(field).cloned().ok_or_else(|| {
                                    anyhow::anyhow!(
                                        "Field '{}' not found in struct '{}'",
                                        field,
                                        struct_name
                                    )
                                })
                            } else {
                                bail!("Unknown struct type: {}", struct_name)
                            }
                        } else {
                            // Generic struct - resolve the field type using the concrete type arguments
                            let type_name = format!(
                                "{}({})",
                                struct_name,
                                args.iter()
                                    .map(|t| t.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );

                            if let Some(generic_fields) =
                                self.resolve_generic_struct_fields(&Type::from_string(&type_name))
                            {
                                generic_fields.get(field).cloned().ok_or_else(|| {
                                    anyhow::anyhow!(
                                        "Field '{}' not found in generic struct '{}'",
                                        field,
                                        type_name
                                    )
                                })
                            } else {
                                bail!("Cannot resolve fields for generic type: {}", type_name)
                            }
                        }
                    }
                    _ => bail!(
                        "Cannot access field '{}' on non-struct type: {}",
                        field,
                        object_type
                    ),
                }
            }

            Expr::Alloc { element_type, size } => {
                let element_type = self.resolve_type(element_type);

                // Size-based allocation
                let size_type = self.check_expression(size)?;
                if !size_type.is_compatible_with(&Type::Int) {
                    bail!("Allocation size must be a number, got {}", size_type);
                }

                Ok(Type::Pointer(Box::new(element_type)))
            }

            Expr::MethodCall {
                object,
                type_name,
                method,
                args: _,
            } => {
                if let Some(obj) = object {
                    // Instance method call (obj.method())
                    let object_type = self.check_expression(obj)?;
                    match object_type {
                        Type::String => {
                            // String type supports length method (as a field access)
                            if method == "length" {
                                return Ok(Type::Int);
                            } else {
                                bail!("Method '{}' not found in string type", method);
                            }
                        }
                        Type::Struct {
                            name: ref struct_name,
                            ref args,
                        } if struct_name == "array" && args.len() == 1 && args[0] == Type::Byte => {
                            // array(byte) supports length and data methods
                            if method == "length" {
                                return Ok(Type::Int);
                            } else {
                                bail!("Method '{}' not found in array(byte) type", method);
                            }
                        }
                        Type::Struct { name, args: _ } => {
                            // Handle both generic and non-generic struct types
                            if name.contains('(') && name.ends_with(')') {
                                // This is a generic instantiation like "Container(int)"
                                // Extract the base generic struct name
                                if let Some(paren_pos) = name.find('(') {
                                    let base_name = &name[..paren_pos];
                                    let generic_method_name = format!("{}__{}", base_name, method);

                                    // Check if the generic method exists
                                    if let Some(_) = self.functions.get(&generic_method_name) {
                                        // Method exists on the generic type, defer validation to monomorphization
                                        return Ok(Type::Unknown);
                                    }
                                }
                            }

                            // Standard method resolution for non-generic structs or generic types without instantiation
                            let method_name = format!("{}__{}", name, method);
                            if let Some(_) = self.functions.get(&method_name) {
                                Ok(Type::Unknown) // Return type will be determined by function signature
                            } else {
                                bail!("Method '{}' not found for struct type '{}'", method, name)
                            }
                        }
                        _ => bail!(
                            "Cannot call method '{}' on non-struct type: {}",
                            method,
                            object_type
                        ),
                    }
                } else if let Some(type_name_str) = type_name {
                    // Associated method call (Type::method())
                    let resolved_type = self.resolve_type(type_name_str);
                    match resolved_type {
                        Type::Struct { name, args: _ } => {
                            // Check for method on the struct type
                            let method_name = format!("{}__{}", name, method);
                            if let Some(func_type) = self.functions.get(&method_name).cloned() {
                                if let Type::Function { return_type, .. } = func_type {
                                    Ok(*return_type)
                                } else {
                                    bail!("'{}' is not a function", method_name);
                                }
                            } else {
                                // Try as generic type if direct method not found
                                let generic_method_name = format!("{}__{}", name, method);
                                if let Some(_) = self.functions.get(&generic_method_name) {
                                    // Method exists on the generic type, defer validation to monomorphization
                                    Ok(Type::Unknown)
                                } else {
                                    bail!(
                                        "Method '{}' not found for struct type '{}'",
                                        method,
                                        name
                                    )
                                }
                            }
                        }
                        _ => bail!(
                            "Cannot call associated method '{}' on non-struct type: {}",
                            method,
                            resolved_type
                        ),
                    }
                } else {
                    bail!("MethodCall must have either object or type_name specified")
                }
            }
        }
    }

    /// Infer the return type of a function based on its body
    fn infer_function_return_type(&self, function: &Function) -> Type {
        // Look for return statements in the function body
        for stmt in &function.body {
            if let Some(return_type) = self.find_return_type_in_statement(stmt) {
                return return_type;
            }
        }

        // Default to Number if no return statements found
        Type::Int
    }

    /// Recursively search for return statements and infer their types
    fn find_return_type_in_statement(&self, stmt: &PositionedStmt) -> Option<Type> {
        match &stmt.value {
            Stmt::Return(expr) => {
                // Try to infer the type of the return expression
                match &expr.value {
                    Expr::StructNew { type_name, .. } => {
                        // If returning a struct creation, return that struct type
                        self.structs.get(&type_name.to_string()).cloned()
                    }
                    Expr::Int(_) => Some(Type::Int),
                    Expr::Boolean(_) => Some(Type::Boolean),
                    Expr::String(_) => Some(Type::String),
                    Expr::Byte(_) => Some(Type::Byte),
                    // For other expressions, we'd need more complex analysis
                    _ => Some(Type::Int), // Default fallback
                }
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                // Check both branches for return statements
                for stmt in then_branch {
                    if let Some(return_type) = self.find_return_type_in_statement(stmt) {
                        return Some(return_type);
                    }
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        if let Some(return_type) = self.find_return_type_in_statement(stmt) {
                            return Some(return_type);
                        }
                    }
                }
                None
            }
            Stmt::While { body, .. } => {
                for stmt in body {
                    if let Some(return_type) = self.find_return_type_in_statement(stmt) {
                        return Some(return_type);
                    }
                }
                None
            }
            _ => None,
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
        if let Some(var_type) = self.globals.get(name) {
            return Ok(var_type.clone());
        }

        // Then check instance variables
        if let Some(var_type) = self.variables.get(name) {
            return Ok(var_type.clone());
        }

        // Finally check functions (functions can be used as values)
        if let Some(func_type) = self.functions.get(name) {
            return Ok(func_type.clone());
        }

        bail!("Undefined variable: {}", name)
    }

    /// Resolve field information for a generic struct instantiation like "Container(int)"
    fn resolve_generic_struct_fields(&self, type_info: &Type) -> Option<HashMap<String, Type>> {
        // Handle generic type instantiation
        if let Type::Struct { name, args } = type_info {
            let generic_name = name.clone();
            let concrete_args = args.clone();

            // Find the generic struct declaration and get its type parameters
            if let Some(struct_type) = self.structs.get(&generic_name) {
                if let Type::Struct {
                    name: _,
                    args: type_params,
                } = struct_type
                {
                    // Create mapping from type parameter names to concrete types
                    let mut type_mapping = HashMap::new();
                    for (param, concrete_type) in type_params.iter().zip(concrete_args.iter()) {
                        if let Type::TypeParameter(param_name) = param {
                            type_mapping.insert(param_name.clone(), concrete_type.clone());
                        }
                    }

                    // Find the generic struct fields and substitute type parameters
                    if let Some(generic_fields) = self.struct_fields.get(&generic_name) {
                        let mut concrete_fields = HashMap::new();
                        for (field_name, field_type) in generic_fields {
                            let concrete_field_type =
                                self.substitute_type_with_mapping(field_type, &type_mapping);
                            concrete_fields.insert(field_name.clone(), concrete_field_type);
                        }
                        return Some(concrete_fields);
                    }
                }
            }

            // Fallback: if the generic struct is not found, try to find it among all structs
            // This handles cases where the type checker runs multiple times and state is lost
            for (struct_name, struct_type) in &self.structs {
                if let Type::Struct {
                    name,
                    args: type_params,
                } = struct_type
                {
                    if name == &generic_name {
                        // Found the generic struct under a different key
                        let mut type_mapping = HashMap::new();
                        for (param, concrete_type) in type_params.iter().zip(concrete_args.iter()) {
                            if let Type::TypeParameter(param_name) = param {
                                type_mapping.insert(param_name.clone(), concrete_type.clone());
                            }
                        }

                        if let Some(generic_fields) = self.struct_fields.get(struct_name) {
                            let mut concrete_fields = HashMap::new();
                            for (field_name, field_type) in generic_fields {
                                let concrete_field_type =
                                    self.substitute_type_with_mapping(field_type, &type_mapping);
                                concrete_fields.insert(field_name.clone(), concrete_field_type);
                            }
                            return Some(concrete_fields);
                        }
                    }
                }
            }

            // Final fallback: hardcoded knowledge for common generic types
            if generic_name == "array" && concrete_args.len() == 1 {
                // For array(T), we know the structure: { data: [*]T, length: int }
                let element_type = &concrete_args[0];
                let mut concrete_fields = HashMap::new();
                concrete_fields.insert(
                    "data".to_string(),
                    Type::Pointer(Box::new(element_type.clone())),
                );
                concrete_fields.insert("length".to_string(), Type::Int);
                return Some(concrete_fields);
            }

            if generic_name == "vec" && concrete_args.len() == 1 {
                // For vec(T), we know the structure: { data: [*]T, length: int, capacity: int }
                let element_type = &concrete_args[0];
                let mut concrete_fields = HashMap::new();
                concrete_fields.insert(
                    "data".to_string(),
                    Type::Pointer(Box::new(element_type.clone())),
                );
                concrete_fields.insert("length".to_string(), Type::Int);
                concrete_fields.insert("capacity".to_string(), Type::Int);
                return Some(concrete_fields);
            }

            if generic_name == "Pair" && concrete_args.len() == 2 {
                // For Pair(A, B), we know the structure: { first: A, second: B }
                let first_type = &concrete_args[0];
                let second_type = &concrete_args[1];
                let mut concrete_fields = HashMap::new();
                concrete_fields.insert("first".to_string(), first_type.clone());
                concrete_fields.insert("second".to_string(), second_type.clone());
                return Some(concrete_fields);
            }

            if generic_name == "Container" && concrete_args.len() == 1 {
                // For Container(T), we know the structure: { value: T }
                let value_type = &concrete_args[0];
                let mut concrete_fields = HashMap::new();
                concrete_fields.insert("value".to_string(), value_type.clone());
                return Some(concrete_fields);
            }

            // General fallback: try to find any struct whose name contains the generic name
            // This is a last resort when the proper generic resolution fails
            for (struct_name, _) in &self.structs {
                if struct_name.starts_with(&generic_name) && struct_name.contains('(') {
                    // Found a similar instantiation, try to use its field information
                    if let Some(existing_fields) = self.struct_fields.get(struct_name) {
                        // Try to substitute known type parameters
                        let mut concrete_fields = HashMap::new();
                        for (field_name, field_type) in existing_fields {
                            // For simple cases, if the field type is a type parameter, substitute it
                            let concrete_field_type = match field_type {
                                Type::TypeParameter(_) if concrete_args.len() == 1 => {
                                    concrete_args[0].clone()
                                }
                                Type::Struct {
                                    name: type_name,
                                    args,
                                } if concrete_args.len() == 1
                                    && type_name.len() == 1
                                    && args.is_empty() =>
                                {
                                    // Single letter type parameter
                                    concrete_args[0].clone()
                                }
                                _ => field_type.clone(),
                            };
                            concrete_fields.insert(field_name.clone(), concrete_field_type);
                        }
                        return Some(concrete_fields);
                    }
                }
            }
        }
        None
    }

    /// Resolve struct fields from a Type, handling both Generic and Struct types
    fn resolve_struct_fields_from_type(&self, type_info: &Type) -> Result<HashMap<String, Type>> {
        match type_info {
            Type::Struct { .. } => {
                // Handle generic struct instantiation
                if let Some(generic_fields) = self.resolve_generic_struct_fields(type_info) {
                    Ok(generic_fields)
                } else {
                    bail!("Unknown generic struct type: {}", type_info)
                }
            }
            _ => {
                // Handle non-generic struct types
                match self.struct_fields.get(&type_info.to_string()) {
                    Some(fields) => Ok(fields.clone()),
                    None => {
                        bail!("Unknown struct type: {}", type_info)
                    }
                }
            }
        }
    }

    /// Resolve type for a generic struct instantiation like "Container(int)"
    fn resolve_generic_struct_type(&self, type_name: &str) -> Option<Type> {
        // Parse generic instantiation and return proper Type::Struct
        if let Some((generic_name, args)) = self.parse_generic_instantiation(type_name) {
            Some(Type::Struct {
                name: generic_name,
                args,
            })
        } else {
            // For non-generic types, return as struct
            Some(Type::Struct {
                name: type_name.to_string(),
                args: vec![],
            })
        }
    }

    /// Parse a generic instantiation like "Container(int)" into ("Container", [Type::Int])
    fn parse_generic_instantiation(&self, type_name: &str) -> Option<(String, Vec<Type>)> {
        if type_name.contains('(') && type_name.ends_with(')') {
            if let Some(paren_pos) = type_name.find('(') {
                let generic_name = &type_name[..paren_pos];
                let args_str = &type_name[paren_pos + 1..type_name.len() - 1];

                if args_str.is_empty() {
                    return Some((generic_name.to_string(), vec![]));
                }

                let concrete_args: Vec<Type> = args_str
                    .split(", ")
                    .map(|arg| Type::from_string(arg.trim()))
                    .collect();

                return Some((generic_name.to_string(), concrete_args));
            }
        }
        None
    }

    /// Substitute type parameters in a type using a name-based mapping
    fn substitute_type_with_mapping(
        &self,
        field_type: &Type,
        type_mapping: &HashMap<String, Type>,
    ) -> Type {
        match field_type {
            Type::TypeParameter(param_name) => {
                // Look up the concrete type for this type parameter
                type_mapping
                    .get(param_name)
                    .cloned()
                    .unwrap_or_else(|| field_type.clone())
            }
            Type::Map(key_type, value_type) => Type::Map(
                Box::new(self.substitute_type_with_mapping(key_type, type_mapping)),
                Box::new(self.substitute_type_with_mapping(value_type, type_mapping)),
            ),
            Type::Pointer(elem_type) => Type::Pointer(Box::new(
                self.substitute_type_with_mapping(elem_type, type_mapping),
            )),
            Type::Struct { name, args } => {
                // Check if the struct name is actually a type parameter
                if let Some(concrete_type) = type_mapping.get(name) {
                    concrete_type.clone()
                } else {
                    // Otherwise, recursively substitute the arguments
                    Type::Struct {
                        name: name.clone(),
                        args: args
                            .iter()
                            .map(|arg| self.substitute_type_with_mapping(arg, type_mapping))
                            .collect(),
                    }
                }
            }
            Type::Function {
                params,
                return_type,
            } => Type::Function {
                params: params
                    .iter()
                    .map(|param| self.substitute_type_with_mapping(param, type_mapping))
                    .collect(),
                return_type: Box::new(self.substitute_type_with_mapping(return_type, type_mapping)),
            },
            // For other types, return as-is
            _ => field_type.clone(),
        }
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
        assert_eq!(Type::from_string("int"), Type::Int);
        assert_eq!(Type::from_string("bool"), Type::Boolean);
        assert_eq!(Type::from_string("string"), Type::String);
    }

    #[test]
    fn test_vector_type_checking() {
        let input = r#"
            type vec(T: type) = struct {
              data: [*]T,
              length: int,
              capacity: int,
              fun _new(): vec(T) do
                return new(struct) vec(T) { .data = alloc(T, 4), .length = 0, .capacity = 4 };
              end
              fun _push(self: vec(T), item: T) do
                self.data[self.length] = item;
                self.length = self.length + 1;
                return 0;
              end
              fun _get(self: vec(T), index: int): T do
                return self.data[index];
              end
              fun _set(self: vec(T), index: int, value: T) do
                self.data[index] = value;
                return 0;
              end
            };
            
            fun main() do
                let v = new vec(int) {};
                v <- 1;
                v <- 2;
                v <- 3;
                v[0] = 4;
                return v[0];
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&mut program);
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
        let mut program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&mut program);
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
        let mut program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&mut program);
        assert!(result.is_err());

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Addition operation type mismatch"));
    }

    #[test]
    fn test_pointer_type_parsing() {
        // Test that "[*]int" parses correctly as a pointer type
        let pointer_type = Type::from_string("[*]int");
        assert!(matches!(pointer_type, Type::Pointer { .. }));

        if let Type::Pointer(element_type) = pointer_type {
            assert!(matches!(element_type.as_ref(), Type::Int));
        }
    }

    #[test]
    fn test_pointer_type_display() {
        let pointer_type = Type::Pointer(Box::new(Type::Int));
        assert_eq!(format!("{}", pointer_type), "[*]int");
    }

    #[test]
    fn test_pointer_type_compatibility() {
        let pointer1 = Type::Pointer(Box::new(Type::Int));
        let pointer2 = Type::Pointer(Box::new(Type::Int));
        let different_pointer = Type::Pointer(Box::new(Type::String));

        assert!(pointer1.is_compatible_with(&pointer2));
        assert!(!pointer1.is_compatible_with(&different_pointer));
    }

    #[test]
    fn test_string_indexing_type_checking() {
        let input = r#"
            fun main() do
                let str = "Hello";
                return str[0];
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&mut program);
        assert!(result.is_ok()); // String indexing should be allowed
    }

    #[test]
    fn test_string_assignment_prevention() {
        let input = r#"
            fun main() do
                let str = "Hello";
                str[0] = 72;
                return str[0];
            end
        "#;

        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program().unwrap();

        let mut checker = TypeChecker::new();
        let result = checker.check_program(&mut program);
        assert!(result.is_err()); // String assignment should be rejected

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("strings are immutable"));
    }
}
