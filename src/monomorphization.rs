use crate::ast::{Decl, Expr, Function, Program, Stmt, StructDecl, Type};
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
    generic_functions: HashMap<String, Function>,
    generic_structs: HashMap<String, StructDecl>,
    /// Map from mangled names to monomorphized declarations
    monomorphized_functions: HashMap<String, Function>,
    monomorphized_structs: HashMap<String, StructDecl>,
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
    pub fn register_generic_function(&mut self, function: Function) {
        if !function.type_params.is_empty() {
            self.generic_functions
                .insert(function.name.clone(), function);
        }
    }

    /// Register a generic struct for later monomorphization
    pub fn register_generic_struct(&mut self, struct_decl: StructDecl) {
        if !struct_decl.type_params.is_empty() {
            self.generic_structs
                .insert(struct_decl.name.clone(), struct_decl);
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
    fn collect_targets_from_decl(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::Function(func) => {
                if !func.type_params.is_empty() {
                    self.register_generic_function(func.clone());
                }
                // Look for generic calls in function body
                for stmt in &func.body {
                    self.collect_targets_from_stmt(stmt)?;
                }
            }
            Decl::Struct(struct_decl) => {
                if !struct_decl.type_params.is_empty() {
                    self.register_generic_struct(struct_decl.clone());
                }
                // Look for generic calls in methods
                for method in &struct_decl.methods {
                    for stmt in &method.body {
                        self.collect_targets_from_stmt(stmt)?;
                    }
                }
            }
            Decl::GlobalVariable(var) => {
                self.collect_targets_from_expr(&var.value)?;
            }
        }
        Ok(())
    }

    /// Collect targets from a statement
    fn collect_targets_from_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
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
            Stmt::Assign { value, .. } => {
                self.collect_targets_from_expr(value)?;
            }
            Stmt::VectorPush { value, .. } => {
                self.collect_targets_from_expr(value)?;
            }
            Stmt::IndexAssign { index, value, .. } => {
                self.collect_targets_from_expr(index)?;
                self.collect_targets_from_expr(value)?;
            }
            Stmt::FieldAssign { object, value, .. } => {
                self.collect_targets_from_expr(object)?;
                self.collect_targets_from_expr(value)?;
            }
        }
        Ok(())
    }

    /// Collect targets from an expression
    fn collect_targets_from_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Call { callee, args } => {
                // Check if this is a generic function call
                if let Expr::Identifier(name) = callee.as_ref() {
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
                if let Some(generic_type) = self.parse_generic_type(type_name) {
                    if let Type::Generic { name, args } = generic_type {
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
                kind: _,
                size,
                initial_values,
                ..
            } => {
                if let Some(size_expr) = size {
                    self.collect_targets_from_expr(size_expr)?;
                }
                if let Some(values) = initial_values {
                    for value in values {
                        self.collect_targets_from_expr(value)?;
                    }
                }
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
    fn extract_type_from_expr(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::TypeExpr { type_name } => {
                // For monomorphization, we need to preserve original type names
                // So we'll create a custom type that maintains the source name
                match type_name.as_str() {
                    "int" | "number" => Some(Type::Int),
                    "bool" | "boolean" => Some(Type::Boolean),
                    "string" | "[*]byte" => Some(Type::String),
                    _ => {
                        // Try to parse as a generic type or struct type
                        if let Some(generic_type) = self.parse_generic_type(type_name) {
                            Some(generic_type)
                        } else {
                            // Assume it's a struct type
                            Some(Type::Struct(type_name.clone()))
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
                    return Some(Type::Generic {
                        name: name.to_string(),
                        args: vec![],
                    });
                }

                let args: Vec<Type> = args_str
                    .split(", ")
                    .map(|arg| Type::from_string(arg.trim()))
                    .collect();

                return Some(Type::Generic {
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
        function: &Function,
    ) -> Result<()> {
        // Create type substitution map
        let mut substitutions = HashMap::new();
        if target.args.len() != function.type_params.len() {
            bail!(
                "Type argument mismatch for function {}: expected {}, got {}",
                function.name,
                function.type_params.len(),
                target.args.len()
            );
        }

        for (param, arg) in function.type_params.iter().zip(target.args.iter()) {
            substitutions.insert(param.clone(), arg.clone());
        }

        // Create monomorphized function
        let monomorphized = Function {
            name: target.instantiated_name(),
            type_params: Vec::new(), // Monomorphized functions have no type parameters
            params: function
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
            body: self.substitute_statements(&function.body, &substitutions)?,
        };

        self.monomorphized_functions
            .insert(target.instantiated_name(), monomorphized);
        Ok(())
    }

    /// Monomorphize a generic struct
    fn monomorphize_struct(
        &mut self,
        target: &MonomorphizationTarget,
        struct_decl: &StructDecl,
    ) -> Result<()> {
        // Create type substitution map
        let mut substitutions = HashMap::new();
        if target.args.len() != struct_decl.type_params.len() {
            bail!(
                "Type argument mismatch for struct {}: expected {}, got {}",
                struct_decl.name,
                struct_decl.type_params.len(),
                target.args.len()
            );
        }

        for (param, arg) in struct_decl.type_params.iter().zip(target.args.iter()) {
            substitutions.insert(param.clone(), arg.clone());
        }

        // Create monomorphized struct
        let monomorphized = StructDecl {
            name: target.instantiated_name(),
            type_params: Vec::new(), // Monomorphized structs have no type parameters
            fields: struct_decl
                .fields
                .iter()
                .map(|field| crate::ast::StructField {
                    name: field.name.clone(),
                    type_name: substitute_type_in_string(&field.type_name, &substitutions),
                })
                .collect(),
            methods: struct_decl
                .methods
                .iter()
                .map(|method| {
                    Function {
                        name: method.name.clone(),
                        type_params: method.type_params.clone(), // Methods might have their own type params
                        params: method
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
                            .substitute_statements(&method.body, &substitutions)
                            .unwrap_or_else(|_| method.body.clone()),
                    }
                })
                .collect(),
        };

        self.monomorphized_structs
            .insert(target.instantiated_name(), monomorphized.clone());

        // Also register methods as standalone functions with mangled names
        for method in &monomorphized.methods {
            let mangled_name = format!("{}_{}", target.instantiated_name(), method.name);
            let mangled_function = Function {
                name: mangled_name.clone(),
                type_params: method.type_params.clone(),
                params: method.params.clone(),
                body: method.body.clone(),
            };
            self.monomorphized_functions
                .insert(mangled_name, mangled_function);
        }

        Ok(())
    }

    /// Substitute type parameters in statement list
    fn substitute_statements(
        &mut self,
        statements: &[Stmt],
        substitutions: &HashMap<String, Type>,
    ) -> Result<Vec<Stmt>> {
        statements
            .iter()
            .map(|stmt| self.substitute_statement(stmt, substitutions))
            .collect()
    }

    /// Substitute type parameters in a single statement
    fn substitute_statement(
        &mut self,
        statement: &Stmt,
        substitutions: &HashMap<String, Type>,
    ) -> Result<Stmt> {
        match statement {
            Stmt::Let { name, value } => Ok(Stmt::Let {
                name: name.clone(),
                value: self.substitute_expression(value, substitutions)?,
            }),
            Stmt::Expression(expr) => Ok(Stmt::Expression(
                self.substitute_expression(expr, substitutions)?,
            )),
            Stmt::Return(expr) => Ok(Stmt::Return(
                self.substitute_expression(expr, substitutions)?,
            )),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Ok(Stmt::If {
                condition: self.substitute_expression(condition, substitutions)?,
                then_branch: self.substitute_statements(then_branch, substitutions)?,
                else_branch: else_branch
                    .as_ref()
                    .map(|branch| self.substitute_statements(branch, substitutions))
                    .transpose()?,
            }),
            Stmt::While { condition, body } => Ok(Stmt::While {
                condition: self.substitute_expression(condition, substitutions)?,
                body: self.substitute_statements(body, substitutions)?,
            }),
            Stmt::Assign { name, value } => Ok(Stmt::Assign {
                name: name.clone(),
                value: self.substitute_expression(value, substitutions)?,
            }),
            Stmt::VectorPush { vector, value } => Ok(Stmt::VectorPush {
                vector: vector.clone(),
                value: self.substitute_expression(value, substitutions)?,
            }),
            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type,
            } => Ok(Stmt::IndexAssign {
                container: container.clone(),
                index: self.substitute_expression(index, substitutions)?,
                value: self.substitute_expression(value, substitutions)?,
                container_type: *container_type,
            }),
            Stmt::FieldAssign {
                object,
                field,
                value,
            } => Ok(Stmt::FieldAssign {
                object: self.substitute_expression(object, substitutions)?,
                field: field.clone(),
                value: self.substitute_expression(value, substitutions)?,
            }),
        }
    }

    /// Substitute type parameters in an expression
    fn substitute_expression(
        &mut self,
        expression: &Expr,
        substitutions: &HashMap<String, Type>,
    ) -> Result<Expr> {
        match expression {
            // Simple expressions that don't need substitution
            Expr::Int(n) => Ok(Expr::Int(*n)),
            Expr::Boolean(b) => Ok(Expr::Boolean(*b)),
            Expr::String(s) => Ok(Expr::String(s.clone())),
            Expr::Byte(b) => Ok(Expr::Byte(*b)),
            Expr::Identifier(name) => Ok(Expr::Identifier(name.clone())),
            Expr::TypeExpr { type_name } => Ok(Expr::TypeExpr {
                type_name: substitute_type_in_string(type_name, substitutions),
            }),
            Expr::Alloc {
                element_type,
                kind,
                size,
                initial_values,
            } => Ok(Expr::Alloc {
                element_type: substitute_type_in_string(element_type, substitutions),
                kind: *kind,
                size: if let Some(size_expr) = size {
                    Some(Box::new(
                        self.substitute_expression(size_expr, substitutions)?,
                    ))
                } else {
                    None
                },
                initial_values: if let Some(values) = initial_values {
                    Some(
                        values
                            .iter()
                            .map(|val| self.substitute_expression(val, substitutions))
                            .collect::<Result<Vec<_>>>()?,
                    )
                } else {
                    None
                },
            }),

            // Complex expressions that may contain type information
            Expr::Binary { left, op, right } => Ok(Expr::Binary {
                left: Box::new(self.substitute_expression(left, substitutions)?),
                op: *op,
                right: Box::new(self.substitute_expression(right, substitutions)?),
            }),
            Expr::Call { callee, args } => {
                // This is where we might discover new monomorphization targets
                Ok(Expr::Call {
                    callee: Box::new(self.substitute_expression(callee, substitutions)?),
                    args: args
                        .iter()
                        .map(|arg| self.substitute_expression(arg, substitutions))
                        .collect::<Result<Vec<_>>>()?,
                })
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => Ok(Expr::VectorNew {
                element_type: substitute_type_in_string(element_type, substitutions),
                initial_values: initial_values
                    .iter()
                    .map(|val| self.substitute_expression(val, substitutions))
                    .collect::<Result<Vec<_>>>()?,
            }),
            Expr::Index {
                container,
                index,
                container_type,
            } => Ok(Expr::Index {
                container: Box::new(self.substitute_expression(container, substitutions)?),
                index: Box::new(self.substitute_expression(index, substitutions)?),
                container_type: *container_type,
            }),
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => Ok(Expr::MapNew {
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
            }),
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Ok(Expr::StructNew {
                type_name: substitute_type_in_string(type_name, substitutions),
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
            }),
            Expr::FieldAccess { object, field } => Ok(Expr::FieldAccess {
                object: Box::new(self.substitute_expression(object, substitutions)?),
                field: field.clone(),
            }),
            Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => Ok(Expr::MethodCall {
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
            }),
        }
    }

    /// Get all monomorphized functions
    pub fn get_monomorphized_functions(&self) -> &HashMap<String, Function> {
        &self.monomorphized_functions
    }

    /// Get all monomorphized structs
    pub fn get_monomorphized_structs(&self) -> &HashMap<String, StructDecl> {
        &self.monomorphized_structs
    }

    /// Generate a monomorphized program with all concrete instantiations
    pub fn generate_monomorphized_program(&self, original_program: &Program) -> Result<Program> {
        let mut new_declarations = Vec::new();

        // Add all non-generic declarations from the original program, substituting expressions
        for decl in &original_program.declarations {
            match decl {
                Decl::Function(func) => {
                    if func.type_params.is_empty() {
                        // Non-generic function: substitute generic type instantiations in body
                        let substituted_func = Function {
                            name: func.name.clone(),
                            type_params: func.type_params.clone(),
                            params: func.params.clone(),
                            body: self.substitute_statements_globally(&func.body)?,
                        };
                        new_declarations.push(Decl::Function(substituted_func));
                    }
                }
                Decl::Struct(struct_decl) => {
                    if struct_decl.type_params.is_empty() {
                        new_declarations.push(decl.clone());
                    }
                }
                Decl::GlobalVariable(var) => {
                    // Substitute expressions in global variable values
                    let substituted_var = crate::ast::GlobalVariable {
                        name: var.name.clone(),
                        value: self.substitute_expression_globally(&var.value)?,
                    };
                    new_declarations.push(Decl::GlobalVariable(substituted_var));
                }
            }
        }

        // Add all monomorphized functions
        for monomorphized_func in self.monomorphized_functions.values() {
            new_declarations.push(Decl::Function(monomorphized_func.clone()));
        }

        // Add all monomorphized structs
        for monomorphized_struct in self.monomorphized_structs.values() {
            new_declarations.push(Decl::Struct(monomorphized_struct.clone()));
        }

        Ok(Program {
            declarations: new_declarations,
        })
    }

    /// Substitute generic type instantiations globally (without type parameter substitutions)
    fn substitute_statements_globally(&self, statements: &[Stmt]) -> Result<Vec<Stmt>> {
        statements
            .iter()
            .map(|stmt| self.substitute_statement_globally(stmt))
            .collect()
    }

    /// Substitute generic type instantiations in a single statement globally
    fn substitute_statement_globally(&self, statement: &Stmt) -> Result<Stmt> {
        match statement {
            Stmt::Let { name, value } => Ok(Stmt::Let {
                name: name.clone(),
                value: self.substitute_expression_globally(value)?,
            }),
            Stmt::Expression(expr) => {
                Ok(Stmt::Expression(self.substitute_expression_globally(expr)?))
            }
            Stmt::Return(expr) => Ok(Stmt::Return(self.substitute_expression_globally(expr)?)),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => Ok(Stmt::If {
                condition: self.substitute_expression_globally(condition)?,
                then_branch: self.substitute_statements_globally(then_branch)?,
                else_branch: else_branch
                    .as_ref()
                    .map(|branch| self.substitute_statements_globally(branch))
                    .transpose()?,
            }),
            Stmt::While { condition, body } => Ok(Stmt::While {
                condition: self.substitute_expression_globally(condition)?,
                body: self.substitute_statements_globally(body)?,
            }),
            Stmt::Assign { name, value } => Ok(Stmt::Assign {
                name: name.clone(),
                value: self.substitute_expression_globally(value)?,
            }),
            Stmt::VectorPush { vector, value } => Ok(Stmt::VectorPush {
                vector: vector.clone(),
                value: self.substitute_expression_globally(value)?,
            }),
            Stmt::IndexAssign {
                container,
                index,
                value,
                container_type,
            } => Ok(Stmt::IndexAssign {
                container: container.clone(),
                index: self.substitute_expression_globally(index)?,
                value: self.substitute_expression_globally(value)?,
                container_type: *container_type,
            }),
            Stmt::FieldAssign {
                object,
                field,
                value,
            } => Ok(Stmt::FieldAssign {
                object: self.substitute_expression_globally(object)?,
                field: field.clone(),
                value: self.substitute_expression_globally(value)?,
            }),
        }
    }

    /// Substitute generic type instantiations in an expression globally
    fn substitute_expression_globally(&self, expression: &Expr) -> Result<Expr> {
        match expression {
            // Simple expressions that don't need substitution
            Expr::Int(n) => Ok(Expr::Int(*n)),
            Expr::Boolean(b) => Ok(Expr::Boolean(*b)),
            Expr::String(s) => Ok(Expr::String(s.clone())),
            Expr::Byte(b) => Ok(Expr::Byte(*b)),
            Expr::Identifier(name) => Ok(Expr::Identifier(name.clone())),
            Expr::TypeExpr { type_name } => Ok(Expr::TypeExpr {
                type_name: type_name.clone(),
            }),
            Expr::Alloc {
                element_type,
                kind,
                size,
                initial_values,
            } => Ok(Expr::Alloc {
                element_type: substitute_type_in_string_globally(element_type),
                kind: *kind,
                size: if let Some(size_expr) = size {
                    Some(Box::new(self.substitute_expression_globally(size_expr)?))
                } else {
                    None
                },
                initial_values: if let Some(values) = initial_values {
                    Some(
                        values
                            .iter()
                            .map(|val| self.substitute_expression_globally(val))
                            .collect::<Result<Vec<_>>>()?,
                    )
                } else {
                    None
                },
            }),

            // Complex expressions
            Expr::Binary { left, op, right } => Ok(Expr::Binary {
                left: Box::new(self.substitute_expression_globally(left)?),
                op: *op,
                right: Box::new(self.substitute_expression_globally(right)?),
            }),
            Expr::Call { callee, args } => {
                // Check if this is a generic function call
                if let Expr::Identifier(func_name) = callee.as_ref() {
                    if self.generic_functions.contains_key(func_name) {
                        // Extract type arguments from the beginning of args
                        let mut type_args = Vec::new();
                        let mut remaining_args = Vec::new();

                        let generic_func = &self.generic_functions[func_name];
                        let num_type_params = generic_func.type_params.len();

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

                            return Ok(Expr::Call {
                                callee: Box::new(Expr::Identifier(monomorphized_name)),
                                args: remaining_args,
                            });
                        }
                    }
                }

                // Default case: not a generic function call
                Ok(Expr::Call {
                    callee: Box::new(self.substitute_expression_globally(callee)?),
                    args: args
                        .iter()
                        .map(|arg| self.substitute_expression_globally(arg))
                        .collect::<Result<Vec<_>>>()?,
                })
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => Ok(Expr::VectorNew {
                element_type: substitute_type_in_string_globally(element_type),
                initial_values: initial_values
                    .iter()
                    .map(|val| self.substitute_expression_globally(val))
                    .collect::<Result<Vec<_>>>()?,
            }),
            Expr::Index {
                container,
                index,
                container_type,
            } => Ok(Expr::Index {
                container: Box::new(self.substitute_expression_globally(container)?),
                index: Box::new(self.substitute_expression_globally(index)?),
                container_type: *container_type,
            }),
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => Ok(Expr::MapNew {
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
            }),
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => Ok(Expr::StructNew {
                type_name: substitute_type_in_string_globally(type_name),
                fields: fields
                    .iter()
                    .map(|(name, expr)| {
                        Ok((name.clone(), self.substitute_expression_globally(expr)?))
                    })
                    .collect::<Result<Vec<_>>>()?,
                kind: *kind,
            }),
            Expr::FieldAccess { object, field } => Ok(Expr::FieldAccess {
                object: Box::new(self.substitute_expression_globally(object)?),
                field: field.clone(),
            }),
            Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => Ok(Expr::MethodCall {
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
            }),
        }
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

        monomorphizer.register_generic_function(generic_function);
        assert!(monomorphizer.generic_functions.contains_key("identity"));
    }
}
