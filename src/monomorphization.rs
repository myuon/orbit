use crate::ast::{
    Decl, Expr, Function, Positioned, PositionedDecl, PositionedExpr, PositionedFunction,
    PositionedStmt, PositionedStructDecl, Program, Stmt, StructDecl, Type,
};
use anyhow::{bail, Result};
use std::collections::{HashMap, HashSet};

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
            // Use the original type names as they appear in source code
            let args_str = self
                .args
                .iter()
                .map(|t| match t {
                    Type::Int => "int".to_string(),      // Keep as "int" not "number"
                    Type::Boolean => "bool".to_string(), // Keep as "bool" not "boolean"
                    _ => t.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", self.symbol, args_str)
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
                // Look for generic calls in methods
                for method in &struct_decl.value.methods {
                    for stmt in &method.value.body {
                        self.collect_targets_from_stmt(stmt)?;
                    }
                }
            }
            Decl::GlobalVariable(var) => {
                self.collect_targets_from_expr(&var.value.value)?;
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
            Expr::MapNew { initial_pairs, .. } => {
                for (key, value) in initial_pairs {
                    self.collect_targets_from_expr(key)?;
                    self.collect_targets_from_expr(value)?;
                }
            }
            Expr::Binary { left, right, .. } => {
                self.collect_targets_from_expr(left)?;
                self.collect_targets_from_expr(right)?;
            }
            Expr::Index {
                container, index, ..
            } => {
                self.collect_targets_from_expr(container)?;
                self.collect_targets_from_expr(index)?;
            }
            Expr::FieldAccess { object, .. } => {
                self.collect_targets_from_expr(object)?;
            }
            Expr::MethodCall { object, args, .. } => {
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
            // Simple expressions don't need recursive processing
            Expr::Int(_)
            | Expr::Boolean(_)
            | Expr::String(_)
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
                    .map(|arg| Type::from_string(arg.trim()))
                    .collect();

                return Some(Type::Struct {
                    name: name.to_string(),
                    args,
                });
            }
        }
        None
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
            substitutions.insert(param.clone(), arg.clone());
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
                    type_name: param
                        .type_name
                        .as_ref()
                        .map(|t| substitute_type_in_string(t, &substitutions)),
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
            substitutions.insert(param.clone(), arg.clone());
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
                    type_name: substitute_type_in_string(&field.type_name, &substitutions),
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
                                type_name: param
                                    .type_name
                                    .as_ref()
                                    .map(|t| substitute_type_in_string(t, &substitutions)),
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
            let separator = if method.value.name.starts_with('_') {
                "___"
            } else {
                "__"
            };
            let mangled_name = format!(
                "{}{}{}",
                target.instantiated_name(),
                separator,
                method.value.name
            );
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
            Expr::Byte(b) => Expr::Byte(*b),
            Expr::Identifier(name) => Expr::Identifier(name.clone()),
            Expr::TypeExpr { type_name } => Expr::TypeExpr {
                type_name: Type::from_string(&substitute_type_in_string(
                    &type_name.to_string(),
                    substitutions,
                )),
            },
            Expr::Alloc { element_type, size } => Expr::Alloc {
                element_type: substitute_type_in_string(element_type, substitutions),
                size: Box::new(self.substitute_expression(size, substitutions)?),
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
                element_type: substitute_type_in_string(element_type, substitutions),
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
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => Expr::MapNew {
                key_type: substitute_type_in_string(key_type, substitutions),
                value_type: substitute_type_in_string(value_type, substitutions),
                initial_pairs: initial_pairs
                    .iter()
                    .map(|(k, v)| {
                        Ok((
                            self.substitute_expression(k, substitutions)?,
                            self.substitute_expression(v, substitutions)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            },
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Expr::StructNew {
                type_name: Type::from_string(&substitute_type_in_string(
                    &type_name.to_string(),
                    substitutions,
                )),
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
                type_name,
                method,
                args,
            } => Expr::MethodCall {
                object: if let Some(obj) = object {
                    Some(Box::new(self.substitute_expression(obj, substitutions)?))
                } else {
                    None
                },
                type_name: type_name.clone(),
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
                    let substituted_var = crate::ast::GlobalVariable {
                        name: var.value.name.clone(),
                        value: self.substitute_expression_globally(&var.value.value)?,
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
            Expr::Byte(b) => Expr::Byte(*b),
            Expr::Identifier(name) => Expr::Identifier(name.clone()),
            Expr::TypeExpr { type_name } => Expr::TypeExpr {
                type_name: type_name.clone(),
            },
            Expr::Alloc { element_type, size } => Expr::Alloc {
                element_type: substitute_type_in_string_globally(element_type),
                size: Box::new(self.substitute_expression_globally(size)?),
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
                element_type: substitute_type_in_string_globally(element_type),
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
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => Expr::MapNew {
                key_type: substitute_type_in_string_globally(key_type),
                value_type: substitute_type_in_string_globally(value_type),
                initial_pairs: initial_pairs
                    .iter()
                    .map(|(k, v)| {
                        Ok((
                            self.substitute_expression_globally(k)?,
                            self.substitute_expression_globally(v)?,
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?,
            },
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Expr::StructNew {
                type_name: Type::from_string(&substitute_type_in_string_globally(
                    &type_name.to_string(),
                )),
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
                type_name,
                method,
                args,
            } => Expr::MethodCall {
                object: if let Some(obj) = object {
                    Some(Box::new(self.substitute_expression_globally(obj)?))
                } else {
                    None
                },
                type_name: type_name.clone(),
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

/// Substitute type parameters in a type string globally (for type instantiations)
fn substitute_type_in_string_globally(type_str: &str) -> String {
    // Keep type string as-is: "Container(int)" stays "Container(int)"
    // No normalization needed - preserve the original type syntax
    type_str.to_string()
}

/// Substitute type parameters in a type string
fn substitute_type_in_string(type_str: &str, substitutions: &HashMap<String, Type>) -> String {
    // Handle generic type instantiation like "Container(T)" -> "Container(int)"
    if type_str.contains('(') && type_str.ends_with(')') {
        if let Some(paren_pos) = type_str.find('(') {
            let name = &type_str[..paren_pos];
            let args_str = &type_str[paren_pos + 1..type_str.len() - 1];

            if args_str.is_empty() {
                return name.to_string();
            }

            let args: Vec<Type> = args_str
                .split(", ")
                .map(|arg| {
                    let trimmed = arg.trim();
                    // Apply substitutions to each argument
                    if let Some(replacement) = substitutions.get(trimmed) {
                        replacement.clone()
                    } else {
                        Type::from_string(trimmed)
                    }
                })
                .collect();

            // Keep the original format with substituted types using source syntax
            let args_str = args
                .iter()
                .map(|t| match t {
                    Type::Int => "int".to_string(),      // Keep as "int" not "number"
                    Type::Boolean => "bool".to_string(), // Keep as "bool" not "boolean"
                    _ => t.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");

            return format!("{}({})", name, args_str);
        }
    }

    // Handle pointer types like [*]T
    if type_str.starts_with("[*]") {
        let inner_type = &type_str[3..];
        let substituted_inner = substitute_type_in_string(inner_type, substitutions);
        return format!("[*]{}", substituted_inner);
    }

    // Simple string substitution for type parameters
    if let Some(replacement) = substitutions.get(type_str) {
        // Convert Type back to source-friendly string representation
        match replacement {
            Type::Int => "int".to_string(),       // Prefer "int" over "number"
            Type::Boolean => "bool".to_string(),  // Prefer "bool" over "boolean"
            Type::String => "string".to_string(), // Could be "string" or "[*]byte", use "string"
            other => other.to_string(),
        }
    } else {
        type_str.to_string()
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
            type_params: vec!["T".to_string()],
            params: vec![FunParam {
                name: "value".to_string(),
                type_name: Some("T".to_string()),
            }],
            body: vec![],
        };

        let positioned_function = Positioned::with_unknown_span(generic_function);
        monomorphizer.register_generic_function(positioned_function);
        assert!(monomorphizer.generic_functions.contains_key("identity"));
    }
}
