use crate::ast::{
    Decl, Expr, Function, Positioned, PositionedDecl, PositionedExpr, PositionedFunction,
    PositionedStmt, PositionedStructDecl, Program, Stmt, StructDecl, Type,
};
use anyhow::{bail, Result};
use std::collections::{HashMap, HashSet};

/// Helper function to substitute type parameters in a Type enum
fn substitute_type(original_type: &Type, substitutions: &HashMap<String, Type>) -> Type {
    match original_type {
        Type::TypeParameter(param_name) => {
            // Replace type parameter with concrete type if substitution exists
            substitutions
                .get(param_name)
                .cloned()
                .unwrap_or_else(|| original_type.clone())
        }
        Type::Struct { name, args } => {
            // Recursively substitute type arguments
            let substituted_args = args
                .iter()
                .map(|arg| substitute_type(arg, substitutions))
                .collect();
            Type::Struct {
                name: name.clone(),
                args: substituted_args,
            }
        }
        Type::Pointer(inner_type) => {
            Type::Pointer(Box::new(substitute_type(inner_type, substitutions)))
        }
        // Primitive types don't need substitution
        Type::Int | Type::Boolean | Type::String | Type::Byte => original_type.clone(),
        // Unknown and Function types are passed through without substitution
        Type::Unknown => original_type.clone(),
        Type::Function {
            params,
            return_type,
        } => {
            // Recursively substitute function parameter and return types
            let substituted_params = params
                .iter()
                .map(|param| substitute_type(param, substitutions))
                .collect();
            let substituted_return_type = Box::new(substitute_type(return_type, substitutions));
            Type::Function {
                params: substituted_params,
                return_type: substituted_return_type,
            }
        }
    }
}

/// Target for monomorphization - represents a generic symbol that needs to be instantiated
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonomorphizationTarget {
    /// The symbol name (function or struct name)
    pub symbol: String,
    /// The concrete type arguments for instantiation
    pub args: Vec<Type>,
}

impl MonomorphizationTarget {
    /// Generate a unique name for this monomorphization target
    /// Keep the original format like "Container(int)" instead of mangling
    pub fn instantiated_name(&self) -> String {
        if self.args.is_empty() {
            self.symbol.clone()
        } else {
            // Check if this is a struct method (contains "#")
            if let Some(separator_pos) = self.symbol.find('#') {
                // For struct methods, replace the generic type in the struct part
                // e.g., "Container#_create_default" with args [int] -> "Container(int)#_create_default"
                let struct_part = &self.symbol[..separator_pos];
                let method_part = &self.symbol[separator_pos..];

                // Generate the instantiated struct name with type arguments
                let args_str = self
                    .args
                    .iter()
                    .map(|t| match t {
                        Type::Int => "int".to_string(),
                        Type::Boolean => "bool".to_string(),
                        Type::Byte => "byte".to_string(),
                        _ => t.to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({}){}", struct_part, args_str, method_part)
            } else {
                // For regular functions, use the original logic
                let args_str = self
                    .args
                    .iter()
                    .map(|t| match t {
                        Type::Int => "int".to_string(),
                        Type::Boolean => "bool".to_string(),
                        Type::Byte => "byte".to_string(),
                        _ => t.to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.symbol, args_str)
            }
        }
    }
}

/// The monomorphization engine
pub struct Monomorphizer {
    /// Queue of targets that need to be monomorphized
    targets: Vec<MonomorphizationTarget>,
    /// Set of targets that have already been processed
    visited: HashSet<MonomorphizationTarget>,
    /// Map from original generic symbols to their declarations
    generic_functions: HashMap<String, PositionedFunction>,
    generic_structs: HashMap<String, PositionedStructDecl>,
    /// Map from mangled names to monomorphized declarations
    monomorphized_functions: HashMap<String, PositionedFunction>,
    monomorphized_structs: HashMap<String, PositionedStructDecl>,
}

impl Monomorphizer {
    pub fn new() -> Self {
        Monomorphizer {
            targets: Vec::new(),
            visited: HashSet::new(),
            generic_functions: HashMap::new(),
            generic_structs: HashMap::new(),
            monomorphized_functions: HashMap::new(),
            monomorphized_structs: HashMap::new(),
        }
    }

    /// Register a generic function for later monomorphization
    pub fn register_generic_function(&mut self, function: PositionedFunction) {
        if !function.value.type_params.is_empty() {
            self.generic_functions
                .insert(function.value.name.clone(), function);
        }
    }

    /// Register a generic struct for later monomorphization
    pub fn register_generic_struct(&mut self, struct_decl: PositionedStructDecl) {
        if !struct_decl.value.type_params.is_empty() {
            self.generic_structs
                .insert(struct_decl.value.name.clone(), struct_decl);
        }
    }

    /// Add a target for monomorphization
    pub fn add_target(&mut self, target: MonomorphizationTarget) {
        if !self.visited.contains(&target) {
            self.targets.push(target);
        }
    }

    /// Collect all monomorphization targets from a program
    pub fn collect_targets(&mut self, program: &Program) -> Result<()> {
        for decl in &program.declarations {
            self.collect_targets_from_decl(decl)?;
        }
        Ok(())
    }

    /// Collect targets from a declaration
    fn collect_targets_from_decl(&mut self, decl: &PositionedDecl) -> Result<()> {
        match &decl.value {
            Decl::Function(func) => {
                if !func.value.type_params.is_empty() {
                    self.register_generic_function(func.clone());
                }
                // Look for generic calls in function body
                for stmt in &func.value.body {
                    self.collect_targets_from_stmt(stmt)?;
                }
            }
            Decl::Struct(struct_decl) => {
                if !struct_decl.value.type_params.is_empty() {
                    self.register_generic_struct(struct_decl.clone());
                }
                // Look for generic calls in methods AND register generic methods
                for method in &struct_decl.value.methods {
                    // Register generic methods as standalone functions with mangled names
                    if !struct_decl.value.type_params.is_empty() {
                        let method_name =
                            format!("{}#{}", struct_decl.value.name, method.value.name);

                        // Create a standalone function from the method
                        let standalone_function = PositionedFunction {
                            value: Function {
                                name: method_name.clone(),
                                type_params: struct_decl.value.type_params.clone(),
                                params: method.value.params.clone(),
                                body: method.value.body.clone(),
                            },
                            span: method.span.clone(),
                        };
                        self.register_generic_function(standalone_function);
                    }

                    for stmt in &method.value.body {
                        self.collect_targets_from_stmt(stmt)?;
                    }
                }
            }
            Decl::GlobalVariable(var) => {
                if let Some(ref value_expr) = var.value.value {
                    self.collect_targets_from_expr(value_expr)?;
                }
            }
        }
        Ok(())
    }

    /// Collect targets from a statement
    fn collect_targets_from_stmt(&mut self, stmt: &PositionedStmt) -> Result<()> {
        match &stmt.value {
            Stmt::Let { value, .. } => {
                self.collect_targets_from_expr(value)?;
            }
            Stmt::Expression(expr) => {
                self.collect_targets_from_expr(expr)?;
            }
            Stmt::Return(expr) => {
                self.collect_targets_from_expr(expr)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_targets_from_expr(condition)?;
                for stmt in then_branch {
                    self.collect_targets_from_stmt(stmt)?;
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.collect_targets_from_stmt(stmt)?;
                    }
                }
            }
            Stmt::While { condition, body } => {
                self.collect_targets_from_expr(condition)?;
                for stmt in body {
                    self.collect_targets_from_stmt(stmt)?;
                }
            }
            Stmt::Assign { lvalue, value } => {
                self.collect_targets_from_expr(lvalue)?;
                self.collect_targets_from_expr(value)?;
            }
            Stmt::VectorPush { value, .. } => {
                self.collect_targets_from_expr(value)?;
            }
        }
        Ok(())
    }

    /// Collect targets from an expression
    fn collect_targets_from_expr(&mut self, expr: &PositionedExpr) -> Result<()> {
        match &expr.value {
            Expr::Call { callee, args } => {
                eprintln!("DEBUG: Processing call expression");
                // Check if this is a generic function call
                if let Expr::Identifier(name) = &callee.value {
                    // Look for type arguments in the arguments
                    let mut type_args = Vec::new();
                    for arg in args {
                        if let Some(type_arg) = self.extract_type_from_expr(arg) {
                            type_args.push(type_arg);
                        }
                    }

                    // Only add as monomorphization target if we have type arguments
                    // AND the function is actually registered as generic
                    if !type_args.is_empty() && self.generic_functions.contains_key(name) {
                        self.add_target(MonomorphizationTarget {
                            symbol: name.clone(),
                            args: type_args,
                        });
                    } else {
                        // Check if this is a desugared method call (e.g., "Container(int)_create_default")
                        self.try_extract_monomorphization_target_from_desugared_name(name);
                    }
                }

                // Recursively check callee and arguments
                self.collect_targets_from_expr(callee)?;
                for arg in args {
                    self.collect_targets_from_expr(arg)?;
                }
            }
            Expr::StructNew {
                type_name,
                fields,
                kind: _,
            } => {
                // Check if this is a generic struct instantiation
                if let Some(generic_type) = self.parse_generic_type(&type_name.to_string()) {
                    if let Type::Struct { name, args } = generic_type {
                        // Only add as target if the struct is actually registered as generic
                        if self.generic_structs.contains_key(&name) {
                            self.add_target(MonomorphizationTarget { symbol: name, args });
                        }
                    }
                }

                // Recursively check field expressions
                for (_, field_expr) in fields {
                    self.collect_targets_from_expr(field_expr)?;
                }
            }
            Expr::VectorNew { initial_values, .. } => {
                for value in initial_values {
                    self.collect_targets_from_expr(value)?;
                }
            }
            Expr::Binary { left, right, .. } => {
                self.collect_targets_from_expr(left)?;
                self.collect_targets_from_expr(right)?;
            }
            Expr::Index {
                container,
                index,
                container_value_type,
                ..
            } => {
                // Check if this is string indexing - if so, add array(byte)#_get as monomorphization target
                if let Some(ref container_type) = container_value_type {
                    if matches!(container_type, Type::String) {
                        // Add array(byte)#_get as a monomorphization target
                        let target = MonomorphizationTarget {
                            symbol: "array#_get".to_string(),
                            args: vec![Type::Byte],
                        };
                        self.add_target(target);

                        // Also add array(byte) struct as a target
                        let struct_target = MonomorphizationTarget {
                            symbol: "array".to_string(),
                            args: vec![Type::Byte],
                        };
                        self.add_target(struct_target);
                    }
                }

                self.collect_targets_from_expr(container)?;
                self.collect_targets_from_expr(index)?;
            }
            Expr::FieldAccess { object, .. } => {
                self.collect_targets_from_expr(object)?;
            }
            Expr::MethodCall {
                object,
                object_type,
                method,
                args,
            } => {
                // Check if this is a type method call (object_type is set and object is None)
                if let Some(obj_type) = object_type {
                    if let Type::Struct {
                        name,
                        args: type_args,
                    } = obj_type
                    {
                        // Generate the generic function name that this should call
                        let generic_function_name = format!("{}#{}", name, method);

                        // Check if this generic function exists
                        if self.generic_functions.contains_key(&generic_function_name) {
                            let target = MonomorphizationTarget {
                                symbol: generic_function_name,
                                args: type_args.clone(),
                            };
                            self.add_target(target);

                            // Also add the struct itself as a target for monomorphization
                            if self.generic_structs.contains_key(name) {
                                let struct_target = MonomorphizationTarget {
                                    symbol: name.clone(),
                                    args: type_args.clone(),
                                };
                                self.add_target(struct_target);
                            }
                        }
                    }
                }

                if let Some(obj) = object {
                    self.collect_targets_from_expr(obj)?;
                }
                for arg in args {
                    self.collect_targets_from_expr(arg)?;
                }
            }
            Expr::Alloc {
                element_type: _,
                size,
            } => {
                self.collect_targets_from_expr(size)?;
            }
            Expr::Sizeof { .. } => {
                // sizeof expressions don't need recursive processing
            }
            Expr::Cast {
                expr,
                target_type: _,
            } => {
                self.collect_targets_from_expr(expr)?;
            }
            // Simple expressions don't need recursive processing
            Expr::Int(_)
            | Expr::Boolean(_)
            | Expr::String(_)
            | Expr::PushString(_)
            | Expr::Byte(_)
            | Expr::Identifier(_)
            | Expr::TypeExpr { .. } => {}
        }
        Ok(())
    }

    /// Extract type from expression if it's a type expression
    fn extract_type_from_expr(&self, expr: &PositionedExpr) -> Option<Type> {
        match &expr.value {
            Expr::TypeExpr { type_name } => {
                // For monomorphization, we need to preserve original type names
                // So we'll create a custom type that maintains the source name
                let type_name_str = type_name.to_string();
                match type_name_str.as_str() {
                    "int" | "number" => Some(Type::Int),
                    "bool" | "boolean" => Some(Type::Boolean),
                    "string" | "[*]byte" => Some(Type::String),
                    _ => {
                        // Try to parse as a generic type or struct type
                        if let Some(generic_type) = self.parse_generic_type(&type_name_str) {
                            Some(generic_type)
                        } else {
                            // Assume it's a struct type
                            Some(Type::Struct {
                                name: type_name_str,
                                args: vec![],
                            })
                        }
                    }
                }
            }
            Expr::Identifier(name) => {
                // Keep old handling for backward compatibility
                match name.as_str() {
                    "type" => Some(Type::TypeParameter("type".to_string())),
                    "int" | "number" => Some(Type::Int),
                    "bool" | "boolean" => Some(Type::Boolean),
                    "string" => Some(Type::String),
                    _ => None, // Don't assume all identifiers are types
                }
            }
            // Add more type expression patterns as needed
            _ => None,
        }
    }

    /// Parse a generic type string like "Container(int, string)"
    fn parse_generic_type(&self, type_str: &str) -> Option<Type> {
        if type_str.contains('(') && type_str.ends_with(')') {
            if let Some(paren_pos) = type_str.find('(') {
                let name = &type_str[..paren_pos];
                let args_str = &type_str[paren_pos + 1..type_str.len() - 1];

                if args_str.is_empty() {
                    return Some(Type::Struct {
                        name: name.to_string(),
                        args: vec![],
                    });
                }

                let args: Vec<Type> = args_str
                    .split(", ")
                    .map(|arg| {
                        let trimmed = arg.trim();
                        match trimmed {
                            "bool" | "boolean" => Type::Boolean,
                            "int" | "number" => Type::Int,
                            "string" => Type::String,
                            "byte" => Type::Byte,
                            _ => Type::Struct {
                                name: trimmed.to_string(),
                                args: vec![],
                            },
                        }
                    })
                    .collect();

                return Some(Type::Struct {
                    name: name.to_string(),
                    args,
                });
            }
        }
        None
    }

    /// Try to extract monomorphization target from a desugared function name
    /// e.g., "Container(int)#_create_default" -> MonomorphizationTarget { symbol: "Container#_create_default", args: [int] }
    fn try_extract_monomorphization_target_from_desugared_name(&mut self, desugared_name: &str) {
        // Look for pattern: Type(args)#method
        if let Some(separator_pos) = desugared_name.find("#") {
            let type_part = &desugared_name[..separator_pos];
            let method_part = &desugared_name[separator_pos..];

            // Extract type and arguments from type_part (e.g., "Container(int)" -> "Container", [int])
            if let Some(paren_pos) = type_part.find('(') {
                if type_part.ends_with(')') {
                    let base_type = &type_part[..paren_pos];
                    let args_str = &type_part[paren_pos + 1..type_part.len() - 1];

                    // Parse the type arguments
                    let mut type_args = Vec::new();
                    if !args_str.is_empty() {
                        for arg in args_str.split(',') {
                            let arg = arg.trim();
                            match arg {
                                "int" | "number" => type_args.push(Type::Int),
                                "bool" | "boolean" => type_args.push(Type::Boolean),
                                "string" | "[*]byte" => type_args.push(Type::String),
                                "byte" => type_args.push(Type::Byte),
                                _ => {
                                    // Try to parse as a complex type
                                    if let Some(parsed_type) = self.parse_generic_type(arg) {
                                        type_args.push(parsed_type);
                                    }
                                }
                            }
                        }
                    }

                    // Generate the generic function name (e.g., "Container#_create_default")
                    let generic_function_name = format!("{}{}", base_type, method_part);

                    // Check if this generic function exists and add as target
                    if self.generic_functions.contains_key(&generic_function_name)
                        || self.generic_structs.contains_key(base_type)
                    {
                        let target = MonomorphizationTarget {
                            symbol: generic_function_name,
                            args: type_args,
                        };
                        self.add_target(target);
                    }
                }
            }
        }
    }

    /// Process all targets and generate monomorphized code
    pub fn monomorphize(&mut self) -> Result<()> {
        while let Some(target) = self.targets.pop() {
            if self.visited.contains(&target) {
                continue;
            }

            self.visited.insert(target.clone());

            // Check if this is a function or struct
            if let Some(generic_function) = self.generic_functions.get(&target.symbol).cloned() {
                self.monomorphize_function(&target, &generic_function)?;
            } else if let Some(generic_struct) = self.generic_structs.get(&target.symbol).cloned() {
                self.monomorphize_struct(&target, &generic_struct)?;
            } else {
                bail!("Unknown generic symbol: {}", target.symbol);
            }
        }

        Ok(())
    }

    /// Monomorphize a generic function
    fn monomorphize_function(
        &mut self,
        target: &MonomorphizationTarget,
        function: &PositionedFunction,
    ) -> Result<()> {
        // Create type substitution map
        let mut substitutions = HashMap::new();
        if target.args.len() != function.value.type_params.len() {
            bail!(
                "Type argument mismatch for function {}: expected {}, got {}",
                function.value.name,
                function.value.type_params.len(),
                target.args.len()
            );
        }

        for (param, arg) in function.value.type_params.iter().zip(target.args.iter()) {
            if let Type::TypeParameter(param_name) = param {
                substitutions.insert(param_name.clone(), arg.clone());
            }
        }

        // Create monomorphized function
        let monomorphized_function = Function {
            name: target.instantiated_name(),
            type_params: Vec::new(), // Monomorphized functions have no type parameters
            params: function
                .value
                .params
                .iter()
                .map(|param| crate::ast::FunParam {
                    name: param.name.clone(),
                    param_type: param
                        .param_type
                        .as_ref()
                        .map(|t| substitute_type(t, &substitutions)),
                })
                .collect(),
            body: self.substitute_statements(&function.value.body, &substitutions)?,
        };

        let positioned_function = Positioned::with_unknown_span(monomorphized_function);
        self.monomorphized_functions
            .insert(target.instantiated_name(), positioned_function);
        Ok(())
    }

    /// Monomorphize a generic struct
    fn monomorphize_struct(
        &mut self,
        target: &MonomorphizationTarget,
        struct_decl: &PositionedStructDecl,
    ) -> Result<()> {
        // Create type substitution map
        let mut substitutions = HashMap::new();
        if target.args.len() != struct_decl.value.type_params.len() {
            bail!(
                "Type argument mismatch for struct {}: expected {}, got {}",
                struct_decl.value.name,
                struct_decl.value.type_params.len(),
                target.args.len()
            );
        }

        for (param, arg) in struct_decl.value.type_params.iter().zip(target.args.iter()) {
            if let Type::TypeParameter(param_name) = param {
                substitutions.insert(param_name.clone(), arg.clone());
            }
        }

        // Create monomorphized struct
        let monomorphized_struct = StructDecl {
            name: target.instantiated_name(),
            type_params: Vec::new(), // Monomorphized structs have no type parameters
            fields: struct_decl
                .value
                .fields
                .iter()
                .map(|field| crate::ast::StructField {
                    name: field.name.clone(),
                    field_type: field.field_type.clone(), // TODO: implement proper type substitution
                })
                .collect(),
            methods: struct_decl
                .value
                .methods
                .iter()
                .map(|method| {
                    let substituted_function = Function {
                        name: method.value.name.clone(),
                        type_params: method.value.type_params.clone(), // Methods might have their own type params
                        params: method
                            .value
                            .params
                            .iter()
                            .map(|param| crate::ast::FunParam {
                                name: param.name.clone(),
                                param_type: param
                                    .param_type
                                    .as_ref()
                                    .map(|t| substitute_type(t, &substitutions)),
                            })
                            .collect(),
                        body: self
                            .substitute_statements(&method.value.body, &substitutions)
                            .unwrap_or_else(|_| method.value.body.clone()),
                    };
                    Positioned::with_unknown_span(substituted_function)
                })
                .collect(),
        };

        let positioned_struct = Positioned::with_unknown_span(monomorphized_struct.clone());
        self.monomorphized_structs
            .insert(target.instantiated_name(), positioned_struct);

        // Also register methods as standalone functions with mangled names
        for method in &monomorphized_struct.methods {
            let mangled_name = format!("{}#{}", target.instantiated_name(), method.value.name);
            let mangled_function = Function {
                name: mangled_name.clone(),
                type_params: method.value.type_params.clone(),
                params: method.value.params.clone(),
                body: method.value.body.clone(),
            };
            let positioned_mangled_function = Positioned::with_unknown_span(mangled_function);
            self.monomorphized_functions
                .insert(mangled_name, positioned_mangled_function);
        }

        Ok(())
    }

    /// Substitute type parameters in statement list
    fn substitute_statements(
        &mut self,
        statements: &[PositionedStmt],
        substitutions: &HashMap<String, Type>,
    ) -> Result<Vec<PositionedStmt>> {
        statements
            .iter()
            .map(|stmt| self.substitute_statement(stmt, substitutions))
            .collect()
    }

    /// Substitute type parameters in a single statement
    fn substitute_statement(
        &mut self,
        statement: &PositionedStmt,
        substitutions: &HashMap<String, Type>,
    ) -> Result<PositionedStmt> {
        let substituted = match &statement.value {
            Stmt::Let { name, value } => Stmt::Let {
                name: name.clone(),
                value: self.substitute_expression(value, substitutions)?,
            },
            Stmt::Expression(expr) => {
                Stmt::Expression(self.substitute_expression(expr, substitutions)?)
            }
            Stmt::Return(expr) => Stmt::Return(self.substitute_expression(expr, substitutions)?),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Stmt::If {
                condition: self.substitute_expression(condition, substitutions)?,
                then_branch: self.substitute_statements(then_branch, substitutions)?,
                else_branch: else_branch
                    .as_ref()
                    .map(|branch| self.substitute_statements(branch, substitutions))
                    .transpose()?,
            },
            Stmt::While { condition, body } => Stmt::While {
                condition: self.substitute_expression(condition, substitutions)?,
                body: self.substitute_statements(body, substitutions)?,
            },
            Stmt::Assign { lvalue, value } => Stmt::Assign {
                lvalue: self.substitute_expression(lvalue, substitutions)?,
                value: self.substitute_expression(value, substitutions)?,
            },
            Stmt::VectorPush {
                vector,
                value,
                vector_type,
            } => Stmt::VectorPush {
                vector: vector.clone(),
                value: self.substitute_expression(value, substitutions)?,
                vector_type: vector_type.clone(),
            },
        };
        Ok(Positioned::with_unknown_span(substituted))
    }

    /// Substitute type parameters in an expression
    fn substitute_expression(
        &mut self,
        expression: &PositionedExpr,
        substitutions: &HashMap<String, Type>,
    ) -> Result<PositionedExpr> {
        let substituted = match &expression.value {
            // Simple expressions that don't need substitution
            Expr::Int(n) => Expr::Int(*n),
            Expr::Boolean(b) => Expr::Boolean(*b),
            Expr::String(s) => Expr::String(s.clone()),
            Expr::PushString(s) => Expr::PushString(s.clone()),
            Expr::Byte(b) => Expr::Byte(*b),
            Expr::Identifier(name) => Expr::Identifier(name.clone()),
            Expr::TypeExpr { type_name } => Expr::TypeExpr {
                type_name: substitute_type(type_name, substitutions),
            },
            Expr::Alloc { element_type, size } => Expr::Alloc {
                element_type: substitute_type(element_type, substitutions),
                size: Box::new(self.substitute_expression(size, substitutions)?),
            },
            Expr::Sizeof { type_name } => Expr::Sizeof {
                type_name: substitute_type(type_name, substitutions),
            },
            Expr::Cast { expr, target_type } => Expr::Cast {
                expr: Box::new(self.substitute_expression(expr, substitutions)?),
                target_type: substitute_type(target_type, substitutions),
            },

            // Complex expressions that may contain type information
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(self.substitute_expression(left, substitutions)?),
                op: *op,
                right: Box::new(self.substitute_expression(right, substitutions)?),
            },
            Expr::Call { callee, args } => {
                // This is where we might discover new monomorphization targets
                Expr::Call {
                    callee: Box::new(self.substitute_expression(callee, substitutions)?),
                    args: args
                        .iter()
                        .map(|arg| self.substitute_expression(arg, substitutions))
                        .collect::<Result<Vec<_>>>()?,
                }
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => Expr::VectorNew {
                element_type: substitute_type(element_type, substitutions),
                initial_values: initial_values
                    .iter()
                    .map(|val| self.substitute_expression(val, substitutions))
                    .collect::<Result<Vec<_>>>()?,
            },
            Expr::Index {
                container,
                index,
                container_type,
                container_value_type,
            } => Expr::Index {
                container: Box::new(self.substitute_expression(container, substitutions)?),
                index: Box::new(self.substitute_expression(index, substitutions)?),
                container_type: *container_type,
                container_value_type: container_value_type.clone(),
            },
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Expr::StructNew {
                type_name: substitute_type(type_name, substitutions),
                fields: fields
                    .iter()
                    .map(|(name, expr)| {
                        Ok((
                            name.clone(),
                            self.substitute_expression(expr, substitutions)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
                kind: *kind,
            },
            Expr::FieldAccess { object, field } => Expr::FieldAccess {
                object: Box::new(self.substitute_expression(object, substitutions)?),
                field: field.clone(),
            },
            Expr::MethodCall {
                object,
                object_type,
                method,
                args,
            } => Expr::MethodCall {
                object: if let Some(obj) = object {
                    Some(Box::new(self.substitute_expression(obj, substitutions)?))
                } else {
                    None
                },
                object_type: object_type
                    .as_ref()
                    .map(|t| substitute_type(t, substitutions)),
                method: method.clone(),
                args: args
                    .iter()
                    .map(|arg| self.substitute_expression(arg, substitutions))
                    .collect::<Result<Vec<_>>>()?,
            },
        };
        Ok(Positioned::with_unknown_span(substituted))
    }

    /// Get all monomorphized functions
    pub fn get_monomorphized_functions(&self) -> &HashMap<String, PositionedFunction> {
        &self.monomorphized_functions
    }

    /// Get all monomorphized structs
    pub fn get_monomorphized_structs(&self) -> &HashMap<String, PositionedStructDecl> {
        &self.monomorphized_structs
    }

    /// Generate a monomorphized program with all concrete instantiations
    pub fn generate_monomorphized_program(&self, original_program: &Program) -> Result<Program> {
        let mut new_declarations = Vec::new();

        // Add all non-generic declarations from the original program, substituting expressions
        for decl in &original_program.declarations {
            match &decl.value {
                Decl::Function(func) => {
                    if func.value.type_params.is_empty() {
                        // Non-generic function: substitute generic type instantiations in body
                        let substituted_func = Function {
                            name: func.value.name.clone(),
                            type_params: func.value.type_params.clone(),
                            params: func.value.params.clone(),
                            body: self.substitute_statements_globally(&func.value.body)?,
                        };
                        let positioned_func = Positioned::with_unknown_span(substituted_func);
                        new_declarations.push(Positioned::with_unknown_span(Decl::Function(
                            positioned_func,
                        )));
                    }
                }
                Decl::Struct(struct_decl) => {
                    if struct_decl.value.type_params.is_empty() {
                        new_declarations.push(decl.clone());
                    }
                }
                Decl::GlobalVariable(var) => {
                    // Substitute expressions in global variable values
                    let substituted_value = if let Some(ref value_expr) = var.value.value {
                        Some(self.substitute_expression_globally(value_expr)?)
                    } else {
                        None
                    };

                    let substituted_var = crate::ast::GlobalVariable {
                        name: var.value.name.clone(),
                        value: substituted_value,
                    };
                    let positioned_var = Positioned::with_unknown_span(substituted_var);
                    new_declarations.push(Positioned::with_unknown_span(Decl::GlobalVariable(
                        positioned_var,
                    )));
                }
            }
        }

        // Add all monomorphized functions
        for monomorphized_func in self.monomorphized_functions.values() {
            new_declarations.push(Positioned::with_unknown_span(Decl::Function(
                monomorphized_func.clone(),
            )));
        }

        // Add all monomorphized structs
        for monomorphized_struct in self.monomorphized_structs.values() {
            new_declarations.push(Positioned::with_unknown_span(Decl::Struct(
                monomorphized_struct.clone(),
            )));
        }

        Ok(Program {
            declarations: new_declarations,
        })
    }

    /// Substitute generic type instantiations globally (without type parameter substitutions)
    fn substitute_statements_globally(
        &self,
        statements: &[PositionedStmt],
    ) -> Result<Vec<PositionedStmt>> {
        statements
            .iter()
            .map(|stmt| self.substitute_statement_globally(stmt))
            .collect()
    }

    /// Substitute generic type instantiations in a single statement globally
    fn substitute_statement_globally(&self, statement: &PositionedStmt) -> Result<PositionedStmt> {
        let substituted = match &statement.value {
            Stmt::Let { name, value } => Stmt::Let {
                name: name.clone(),
                value: self.substitute_expression_globally(value)?,
            },
            Stmt::Expression(expr) => Stmt::Expression(self.substitute_expression_globally(expr)?),
            Stmt::Return(expr) => Stmt::Return(self.substitute_expression_globally(expr)?),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Stmt::If {
                condition: self.substitute_expression_globally(condition)?,
                then_branch: self.substitute_statements_globally(then_branch)?,
                else_branch: else_branch
                    .as_ref()
                    .map(|branch| self.substitute_statements_globally(branch))
                    .transpose()?,
            },
            Stmt::While { condition, body } => Stmt::While {
                condition: self.substitute_expression_globally(condition)?,
                body: self.substitute_statements_globally(body)?,
            },
            Stmt::Assign { lvalue, value } => Stmt::Assign {
                lvalue: self.substitute_expression_globally(lvalue)?,
                value: self.substitute_expression_globally(value)?,
            },
            Stmt::VectorPush {
                vector,
                value,
                vector_type,
            } => Stmt::VectorPush {
                vector: vector.clone(),
                value: self.substitute_expression_globally(value)?,
                vector_type: vector_type.clone(),
            },
        };
        Ok(Positioned::with_unknown_span(substituted))
    }

    /// Substitute generic type instantiations in an expression globally
    fn substitute_expression_globally(
        &self,
        expression: &PositionedExpr,
    ) -> Result<PositionedExpr> {
        let substituted = match &expression.value {
            // Simple expressions that don't need substitution
            Expr::Int(n) => Expr::Int(*n),
            Expr::Boolean(b) => Expr::Boolean(*b),
            Expr::String(s) => Expr::String(s.clone()),
            Expr::PushString(s) => Expr::PushString(s.clone()),
            Expr::Byte(b) => Expr::Byte(*b),
            Expr::Identifier(name) => Expr::Identifier(name.clone()),
            Expr::TypeExpr { type_name } => Expr::TypeExpr {
                type_name: type_name.clone(),
            },
            Expr::Alloc { element_type, size } => Expr::Alloc {
                element_type: element_type.clone(), // No global substitution needed for types
                size: Box::new(self.substitute_expression_globally(size)?),
            },
            Expr::Sizeof { type_name } => Expr::Sizeof {
                type_name: type_name.clone(), // No global substitution needed for types
            },
            Expr::Cast { expr, target_type } => Expr::Cast {
                expr: Box::new(self.substitute_expression_globally(expr)?),
                target_type: target_type.clone(), // No global substitution needed for types
            },

            // Complex expressions
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(self.substitute_expression_globally(left)?),
                op: *op,
                right: Box::new(self.substitute_expression_globally(right)?),
            },
            Expr::Call { callee, args } => {
                // Check if this is a generic function call
                if let Expr::Identifier(func_name) = &callee.value {
                    if self.generic_functions.contains_key(func_name) {
                        // Extract type arguments from the beginning of args
                        let mut type_args = Vec::new();
                        let mut remaining_args = Vec::new();

                        let generic_func = &self.generic_functions[func_name];
                        let num_type_params = generic_func.value.type_params.len();

                        // First n arguments should be type expressions
                        for (i, arg) in args.iter().enumerate() {
                            if i < num_type_params {
                                if let Some(type_arg) = self.extract_type_from_expr(arg) {
                                    type_args.push(type_arg);
                                }
                            } else {
                                remaining_args.push(self.substitute_expression_globally(arg)?);
                            }
                        }

                        // Generate the monomorphized function name
                        if !type_args.is_empty() {
                            let target = MonomorphizationTarget {
                                symbol: func_name.clone(),
                                args: type_args,
                            };
                            let monomorphized_name = target.instantiated_name();

                            return Ok(Positioned::with_unknown_span(Expr::Call {
                                callee: Box::new(Positioned::with_unknown_span(Expr::Identifier(
                                    monomorphized_name,
                                ))),
                                args: remaining_args,
                            }));
                        }
                    }
                }

                // Default case: not a generic function call
                Expr::Call {
                    callee: Box::new(self.substitute_expression_globally(callee)?),
                    args: args
                        .iter()
                        .map(|arg| self.substitute_expression_globally(arg))
                        .collect::<Result<Vec<_>>>()?,
                }
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => Expr::VectorNew {
                element_type: element_type.clone(), // No global substitution needed for types
                initial_values: initial_values
                    .iter()
                    .map(|val| self.substitute_expression_globally(val))
                    .collect::<Result<Vec<_>>>()?,
            },
            Expr::Index {
                container,
                index,
                container_type,
                container_value_type,
            } => Expr::Index {
                container: Box::new(self.substitute_expression_globally(container)?),
                index: Box::new(self.substitute_expression_globally(index)?),
                container_type: *container_type,
                container_value_type: container_value_type.clone(),
            },
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Expr::StructNew {
                type_name: type_name.clone(), // No global substitution needed for types
                fields: fields
                    .iter()
                    .map(|(name, expr)| {
                        Ok((name.clone(), self.substitute_expression_globally(expr)?))
                    })
                    .collect::<Result<Vec<_>>>()?,
                kind: *kind,
            },
            Expr::FieldAccess { object, field } => Expr::FieldAccess {
                object: Box::new(self.substitute_expression_globally(object)?),
                field: field.clone(),
            },
            Expr::MethodCall {
                object,
                object_type,
                method,
                args,
            } => Expr::MethodCall {
                object: if let Some(obj) = object {
                    Some(Box::new(self.substitute_expression_globally(obj)?))
                } else {
                    None
                },
                object_type: object_type.clone(),
                method: method.clone(),
                args: args
                    .iter()
                    .map(|arg| self.substitute_expression_globally(arg))
                    .collect::<Result<Vec<_>>>()?,
            },
        };
        Ok(Positioned::with_unknown_span(substituted))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::FunParam;

    #[test]
    fn test_monomorphization_target_naming() {
        let target = MonomorphizationTarget {
            symbol: "Container".to_string(),
            args: vec![Type::Int, Type::String],
        };
        assert_eq!(target.instantiated_name(), "Container(int, string)");
    }

    #[test]
    fn test_monomorphizer_registration() {
        let mut monomorphizer = Monomorphizer::new();

        let generic_function = Function {
            name: "identity".to_string(),
            type_params: vec![Type::TypeParameter("T".to_string())],
            params: vec![FunParam {
                name: "value".to_string(),
                param_type: Some(Type::TypeParameter("T".to_string())),
            }],
            body: vec![],
        };

        let positioned_function = Positioned::with_unknown_span(generic_function);
        monomorphizer.register_generic_function(positioned_function);
        assert!(monomorphizer.generic_functions.contains_key("identity"));
    }
}
