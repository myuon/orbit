use crate::ast::{
    Decl, Expr, Function, Positioned, PositionedDecl, PositionedExpr, PositionedFunction,
    PositionedStmt, PositionedStructDecl, Program, StructDecl, Type,
};
use crate::ast_visitor::{Visitor, VisitorMut};
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
            // Check if this is a struct method (contains separator)
            if let Some(separator_pos) = self.symbol.find(Type::METHOD_SEPARATOR) {
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

/// AST visitor that collects monomorphization targets
pub struct MonomorphizationTargetCollector<'a> {
    targets: &'a mut Vec<MonomorphizationTarget>,
    visited: &'a mut HashSet<MonomorphizationTarget>,
    generic_functions: &'a mut HashMap<String, PositionedFunction>,
    generic_structs: &'a mut HashMap<String, PositionedStructDecl>,
}

impl<'a> MonomorphizationTargetCollector<'a> {
    pub fn new(
        targets: &'a mut Vec<MonomorphizationTarget>,
        visited: &'a mut HashSet<MonomorphizationTarget>,
        generic_functions: &'a mut HashMap<String, PositionedFunction>,
        generic_structs: &'a mut HashMap<String, PositionedStructDecl>,
    ) -> Self {
        Self {
            targets,
            visited,
            generic_functions,
            generic_structs,
        }
    }

    fn register_generic_function(&mut self, function: PositionedFunction) {
        if !function.value.type_params.is_empty() {
            self.generic_functions
                .insert(function.value.name.clone(), function);
        }
    }

    fn register_generic_struct(&mut self, struct_decl: PositionedStructDecl) {
        if !struct_decl.value.type_params.is_empty() {
            self.generic_structs
                .insert(struct_decl.value.name.clone(), struct_decl);
        }
    }

    fn add_target(&mut self, target: MonomorphizationTarget) {
        if !self.visited.contains(&target) {
            self.targets.push(target);
        }
    }

    fn check_string_usage_and_add_array_byte(&mut self, expr: &PositionedExpr) {
        match &expr.value {
            Expr::String(_) | Expr::PushString(_) => {
                self.add_array_byte_target();
            }
            Expr::Index {
                container_value_type,
                ..
            } => {
                if let Some(container_type) = container_value_type {
                    if matches!(container_type, Type::String) {
                        self.add_array_byte_target();
                    }
                }
            }
            Expr::Binary { left, right, .. } => {
                self.check_string_usage_and_add_array_byte(left);
                self.check_string_usage_and_add_array_byte(right);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.check_string_usage_and_add_array_byte(arg);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.check_string_usage_and_add_array_byte(object);
            }
            Expr::MethodCall { object, args, .. } => {
                if let Some(obj) = object {
                    self.check_string_usage_and_add_array_byte(obj);
                }
                for arg in args {
                    self.check_string_usage_and_add_array_byte(arg);
                }
            }
            Expr::StructNew { fields, .. } => {
                for (_, field_expr) in fields {
                    self.check_string_usage_and_add_array_byte(field_expr);
                }
            }
            Expr::Cast { expr, .. } => {
                self.check_string_usage_and_add_array_byte(expr);
            }
            Expr::Alloc { size, .. } => {
                self.check_string_usage_and_add_array_byte(size);
            }
            _ => {}
        }
    }

    fn add_array_byte_target(&mut self) {
        let struct_target = MonomorphizationTarget {
            symbol: "array".to_string(),
            args: vec![Type::Byte],
        };
        self.add_target(struct_target);
    }
}

impl<'a> Visitor for MonomorphizationTargetCollector<'a> {
    fn visit_decl(&mut self, decl: &PositionedDecl) -> Result<()> {
        match &decl.value {
            Decl::Function(func) => {
                // Register generic functions
                if !func.value.type_params.is_empty() {
                    self.register_generic_function(func.clone());
                }

                // Check if this is a method function (contains #) with generic struct types
                if func.value.name.contains('#') {
                    // Look for generic struct types in the parameters
                    for param in &func.value.params {
                        if let Some(ref param_type) = param.param_type {
                            if let Type::Struct { name, args } = param_type {
                                // Check if any args are type parameters and if the struct is generic
                                if args.iter().any(|arg| matches!(arg, Type::TypeParameter(_)))
                                    && self.generic_structs.contains_key(name)
                                {
                                    // Method functions don't have type_params, so register them directly
                                    self.generic_functions
                                        .insert(func.value.name.clone(), func.clone());
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            Decl::Struct(struct_decl) => {
                // Register generic structs
                if !struct_decl.value.type_params.is_empty() {
                    self.register_generic_struct(struct_decl.clone());
                }
            }
            _ => {}
        }

        // Continue with default traversal
        crate::ast_visitor::walk_decl(self, decl)
    }

    fn visit_expr(&mut self, expr: &PositionedExpr) -> Result<()> {
        // Check if we encounter any string usage and ensure array(byte) is available
        self.check_string_usage_and_add_array_byte(expr);

        match &expr.value {
            Expr::Call { callee, args } => {
                // Check if this is a generic function call
                if let Expr::Identifier(name) = &callee.value {
                    // Check if this is a method call with instantiated type name like "Pair(int, int)#set_first"
                    if name.contains('#') && name.contains('(') {
                        if let Some(hash_pos) = name.find('#') {
                            let struct_part = &name[..hash_pos];
                            let method_part = &name[hash_pos + 1..];

                            if let Some(paren_pos) = struct_part.find('(') {
                                let base_name = &struct_part[..paren_pos];
                                let args_part = &struct_part[paren_pos + 1..struct_part.len() - 1];

                                // Parse the type arguments from the method call
                                let type_args: Vec<Type> = if args_part.is_empty() {
                                    Vec::new()
                                } else {
                                    args_part
                                        .split(", ")
                                        .map(|arg| match arg.trim() {
                                            "int" => Type::Int,
                                            "byte" => Type::Byte,
                                            "bool" => Type::Boolean,
                                            _ => Type::TypeParameter(arg.to_string()),
                                        })
                                        .collect()
                                };

                                // Look for the corresponding generic method function
                                let generic_method_name = format!("{}#{}", base_name, method_part);
                                if self.generic_functions.contains_key(&generic_method_name) {
                                    self.add_target(MonomorphizationTarget {
                                        symbol: generic_method_name,
                                        args: type_args,
                                    });
                                }
                            }
                        }
                    } else {
                        // Regular generic function call
                        // Look for type arguments in the arguments
                        let mut type_args = Vec::new();
                        for arg in args {
                            if let Expr::TypeExpr { type_name } = &arg.value {
                                type_args.push(type_name.clone());
                            }
                        }

                        // Add as monomorphization target if we have type arguments and function is generic
                        if !type_args.is_empty() && self.generic_functions.contains_key(name) {
                            self.add_target(MonomorphizationTarget {
                                symbol: name.clone(),
                                args: type_args,
                            });
                        }
                    }
                }
            }
            Expr::StructNew { type_name, .. } => {
                // Check if this is a generic struct instantiation
                if let Type::Struct { name, args } = type_name {
                    // Only add as target if the struct is actually registered as generic
                    if self.generic_structs.contains_key(name) {
                        self.add_target(MonomorphizationTarget {
                            symbol: name.clone(),
                            args: args.clone(),
                        });
                    }
                }
            }
            Expr::MethodCall { object_type, .. } => {
                // Check if this is a type method call (object_type is set and object is None)
                if let Some(obj_type) = object_type {
                    if let Type::Struct {
                        name,
                        args: type_args,
                    } = obj_type
                    {
                        // Add the struct itself as a target for monomorphization
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
            _ => {}
        }

        // Continue with default traversal
        crate::ast_visitor::walk_expr(self, expr)
    }
}

/// AST visitor that performs type substitutions using a substitution map
pub struct TypeSubstitutionVisitor<'a> {
    substitutions: &'a HashMap<String, Type>,
}

impl<'a> TypeSubstitutionVisitor<'a> {
    pub fn new(substitutions: &'a HashMap<String, Type>) -> Self {
        Self { substitutions }
    }

    fn substitute_type(&self, type_ref: &Type) -> Type {
        type_ref.substitute(self.substitutions)
    }
}

impl<'a> VisitorMut for TypeSubstitutionVisitor<'a> {
    fn visit_expr(&mut self, expr: &mut PositionedExpr) -> Result<()> {
        match &mut expr.value {
            Expr::TypeExpr { type_name } => {
                *type_name = self.substitute_type(type_name);
            }
            Expr::Alloc { element_type, .. } => {
                *element_type = self.substitute_type(element_type);
            }
            Expr::Sizeof { type_name } => {
                *type_name = self.substitute_type(type_name);
            }
            Expr::Cast { target_type, .. } => {
                *target_type = self.substitute_type(target_type);
            }
            Expr::StructNew { type_name, .. } => {
                *type_name = self.substitute_type(type_name);
            }
            Expr::FieldAccess { object_type, .. } => {
                if let Some(obj_type) = object_type {
                    *obj_type = self.substitute_type(obj_type);
                }
            }
            Expr::MethodCall { object_type, .. } => {
                if let Some(obj_type) = object_type {
                    *obj_type = self.substitute_type(obj_type);
                }
            }
            _ => {}
        }

        // Continue with default traversal
        crate::ast_visitor::walk_expr_mut(self, expr)
    }

    fn visit_function(&mut self, func: &mut PositionedFunction) -> Result<()> {
        // Visit parameter types
        for param in &mut func.value.params {
            if let Some(param_type) = &mut param.param_type {
                *param_type = self.substitute_type(param_type);
            }
        }

        // Visit function body
        for stmt in &mut func.value.body {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }

    fn visit_type(&mut self, type_ref: &mut Type) -> Result<()> {
        *type_ref = self.substitute_type(type_ref);
        Ok(())
    }
}

/// AST visitor that performs global type substitutions (replaces generic instantiations with monomorphized names)
pub struct GlobalTypeSubstitutionVisitor<'a> {
    monomorphized_functions: &'a HashMap<String, PositionedFunction>,
    monomorphized_structs: &'a HashMap<String, PositionedStructDecl>,
}

impl<'a> GlobalTypeSubstitutionVisitor<'a> {
    pub fn new(
        monomorphized_functions: &'a HashMap<String, PositionedFunction>,
        monomorphized_structs: &'a HashMap<String, PositionedStructDecl>,
    ) -> Self {
        Self {
            monomorphized_functions,
            monomorphized_structs,
        }
    }

    fn substitute_type_globally(&self, type_ref: &Type) -> Type {
        match type_ref {
            Type::Struct { name, args } => {
                // If this is a generic type that has been monomorphized, use the instantiated name
                if !args.is_empty() {
                    let target = MonomorphizationTarget {
                        symbol: name.clone(),
                        args: args.clone(),
                    };
                    Type::Struct {
                        name: target.instantiated_name(),
                        args: vec![], // Monomorphized types have no type arguments
                    }
                } else {
                    type_ref.clone()
                }
            }
            _ => type_ref.clone(),
        }
    }
}

impl<'a> VisitorMut for GlobalTypeSubstitutionVisitor<'a> {
    fn visit_expr(&mut self, expr: &mut PositionedExpr) -> Result<()> {
        match &mut expr.value {
            Expr::Call { callee, args } => {
                // Check if this is a generic function call that needs to be replaced
                if let Expr::Identifier(func_name) = &callee.value {
                    // Look for generic functions that may need monomorphization
                    // We need to check if this is a call to a function that has type arguments
                    let mut type_args = Vec::new();
                    let mut remaining_args = Vec::new();

                    // Extract type arguments from the beginning of args
                    for arg in args.iter() {
                        if let Expr::TypeExpr { type_name } = &arg.value {
                            type_args.push(type_name.clone());
                        } else {
                            break;
                        }
                    }

                    // If we found type arguments, this might be a generic call
                    if !type_args.is_empty() {
                        let type_args_len = type_args.len();
                        let target = MonomorphizationTarget {
                            symbol: func_name.clone(),
                            args: type_args,
                        };
                        let monomorphized_name = target.instantiated_name();

                        // Check if a monomorphized version exists
                        if self
                            .monomorphized_functions
                            .contains_key(&monomorphized_name)
                        {
                            callee.value = Expr::Identifier(monomorphized_name);
                            // Keep only the non-type arguments
                            for arg in args.iter().skip(type_args_len) {
                                remaining_args.push(arg.clone());
                            }
                            *args = remaining_args;
                        }
                    }
                }
            }
            Expr::FieldAccess { object_type, .. } => {
                if let Some(obj_type) = object_type {
                    *obj_type = self.substitute_type_globally(obj_type);
                }
            }
            Expr::MethodCall { object_type, .. } => {
                if let Some(obj_type) = object_type {
                    *obj_type = self.substitute_type_globally(obj_type);
                }
            }
            _ => {}
        }

        // Continue with default traversal
        crate::ast_visitor::walk_expr_mut(self, expr)
    }

    fn visit_type(&mut self, type_ref: &mut Type) -> Result<()> {
        *type_ref = self.substitute_type_globally(type_ref);
        Ok(())
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

    /// Collect all monomorphization targets from a program using the AST visitor
    pub fn collect_targets(&mut self, program: &Program) -> Result<()> {
        // Use the visitor to register generic functions/structs and collect targets
        let mut collector = MonomorphizationTargetCollector::new(
            &mut self.targets,
            &mut self.visited,
            &mut self.generic_functions,
            &mut self.generic_structs,
        );
        collector.visit_program(program)?;

        Ok(())
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

        // Handle method functions differently from regular generic functions
        if function.value.name.contains('#') && function.value.type_params.is_empty() {
            // This is a method function like "Pair#set_first"
            // The target symbol should be the same as the function name (e.g., "Pair#set_first")
            // The target args are the struct type arguments (e.g., [Int, Int] for Pair(int, int))

            // Find the struct parameter to get the original type parameters
            let mut struct_type_params = Vec::new();
            for param in &function.value.params {
                if let Some(ref param_type) = param.param_type {
                    if let Type::Struct {
                        name: _struct_name,
                        args,
                    } = param_type
                    {
                        // Extract the type parameters from the struct
                        for arg in args {
                            if let Type::TypeParameter(param_name) = arg {
                                if !struct_type_params.contains(param_name) {
                                    struct_type_params.push(param_name.clone());
                                }
                            }
                        }
                        break; // Use the first struct parameter we find
                    }
                }
            }

            // Create substitutions from struct type parameters to target args
            if target.args.len() != struct_type_params.len() {
                bail!(
                    "Type argument mismatch for method function {}: expected {}, got {}",
                    function.value.name,
                    struct_type_params.len(),
                    target.args.len()
                );
            }

            for (param_name, arg) in struct_type_params.iter().zip(target.args.iter()) {
                substitutions.insert(param_name.clone(), arg.clone());
            }
        } else {
            // Regular generic function
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
                        .map(|t| t.substitute(&substitutions)),
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
                    field_type: field.field_type.substitute(&substitutions),
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
                                    .map(|t| t.substitute(&substitutions)),
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

        // Methods are now kept as part of the struct, not converted to global functions

        Ok(())
    }

    /// Substitute type parameters in statement list using visitor
    fn substitute_statements(
        &mut self,
        statements: &[PositionedStmt],
        substitutions: &HashMap<String, Type>,
    ) -> Result<Vec<PositionedStmt>> {
        let mut cloned_statements: Vec<PositionedStmt> = statements.to_vec();
        let mut visitor = TypeSubstitutionVisitor::new(substitutions);

        for stmt in &mut cloned_statements {
            visitor.visit_stmt(stmt)?;
        }

        Ok(cloned_statements)
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

        // Add all monomorphized structs (methods are already handled as separate functions)
        for monomorphized_struct in self.monomorphized_structs.values() {
            new_declarations.push(Positioned::with_unknown_span(Decl::Struct(
                monomorphized_struct.clone(),
            )));
        }

        Ok(Program {
            declarations: new_declarations,
        })
    }

    /// Substitute generic type instantiations globally using visitor
    fn substitute_statements_globally(
        &self,
        statements: &[PositionedStmt],
    ) -> Result<Vec<PositionedStmt>> {
        let mut cloned_statements: Vec<PositionedStmt> = statements.to_vec();
        let mut visitor = GlobalTypeSubstitutionVisitor::new(
            &self.monomorphized_functions,
            &self.monomorphized_structs,
        );

        for stmt in &mut cloned_statements {
            visitor.visit_stmt(stmt)?;
        }

        Ok(cloned_statements)
    }

    /// Substitute generic type instantiations in an expression globally using visitor
    fn substitute_expression_globally(
        &self,
        expression: &PositionedExpr,
    ) -> Result<PositionedExpr> {
        let mut cloned_expression = expression.clone();
        let mut visitor = GlobalTypeSubstitutionVisitor::new(
            &self.monomorphized_functions,
            &self.monomorphized_structs,
        );

        visitor.visit_expr(&mut cloned_expression)?;
        Ok(cloned_expression)
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
