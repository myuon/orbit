use crate::ast::{Decl, Expr, Function, Program, Stmt, StructDecl, StructNewKind};
use anyhow::Result;
use std::collections::{HashMap, HashSet};

/// Dead code elimination phase that removes unused functions, types, and globals
/// This phase occurs after monomorphization to eliminate duplicate and unreachable code
pub struct DeadCodeEliminator {
    /// Set of reachable function names
    reachable_functions: HashSet<String>,
    /// Set of reachable struct type names
    reachable_types: HashSet<String>,
    /// Set of reachable global variable names
    reachable_globals: HashSet<String>,
    /// Map of function names to their declarations for dependency analysis
    functions: HashMap<String, Function>,
    /// Map of struct names to their declarations for dependency analysis
    structs: HashMap<String, StructDecl>,
    /// Map of global variable names to their declarations for dependency analysis
    globals: HashMap<String, crate::ast::GlobalVariable>,
}

impl DeadCodeEliminator {
    pub fn new() -> Self {
        Self {
            reachable_functions: HashSet::new(),
            reachable_types: HashSet::new(),
            reachable_globals: HashSet::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            globals: HashMap::new(),
        }
    }

    /// Eliminate dead code from a program
    pub fn eliminate_dead_code(&mut self, program: Program) -> Result<Program> {
        // Phase 1: Collect all declarations
        self.collect_declarations(&program)?;

        // Phase 2: Mark reachable code starting from entry points
        self.mark_reachable_code()?;

        // Phase 3: Sweep unused declarations
        self.sweep_unused_code(program)
    }

    /// Collect all declarations for dependency analysis
    fn collect_declarations(&mut self, program: &Program) -> Result<()> {
        for decl in &program.declarations {
            match decl {
                Decl::Function(func) => {
                    self.functions.insert(func.name.clone(), func.clone());
                }
                Decl::Struct(struct_decl) => {
                    self.structs
                        .insert(struct_decl.name.clone(), struct_decl.clone());
                }
                Decl::GlobalVariable(global_var) => {
                    self.globals
                        .insert(global_var.name.clone(), global_var.clone());
                }
            }
        }
        Ok(())
    }

    /// Mark all reachable code starting from entry points
    fn mark_reachable_code(&mut self) -> Result<()> {
        // Start with the main function as the primary entry point
        if self.functions.contains_key("main") {
            self.mark_function_reachable("main")?;
        }

        // Process all global variable initializers as additional entry points
        for (global_name, global_var) in &self.globals.clone() {
            self.mark_global_reachable(global_name);
            self.mark_expr_dependencies(&global_var.value)?;
        }

        Ok(())
    }

    /// Mark a function as reachable and analyze its dependencies
    fn mark_function_reachable(&mut self, func_name: &str) -> Result<()> {
        if self.reachable_functions.contains(func_name) {
            return Ok(()); // Already processed
        }

        if let Some(function) = self.functions.get(func_name).cloned() {
            self.reachable_functions.insert(func_name.to_string());

            // Mark parameter types as reachable
            for param in &function.params {
                if let Some(type_name) = &param.type_name {
                    self.mark_type_reachable(type_name);
                }
            }

            // Analyze function body for dependencies
            for stmt in &function.body {
                self.mark_stmt_dependencies(stmt)?;
            }
        }

        Ok(())
    }

    /// Mark a type as reachable and analyze its dependencies
    fn mark_type_reachable(&mut self, type_name: &str) {
        if self.reachable_types.contains(type_name) {
            return; // Already processed
        }

        // Extract base type name from generic instantiations like "array(int)"
        let base_type = if let Some(paren_pos) = type_name.find('(') {
            &type_name[..paren_pos]
        } else {
            type_name
        };

        // Check if we have the exact type name first
        if let Some(struct_decl) = self.structs.get(type_name).cloned() {
            self.reachable_types.insert(type_name.to_string());

            // Mark field types as reachable
            for field in &struct_decl.fields {
                self.mark_type_reachable(&field.type_name);
            }

            // Mark methods as reachable if the type is used
            // Methods are converted to mangled function names during desugaring
            for method in &struct_decl.methods {
                let mangled_name = format!("{}_{}", type_name, method.name);
                if self.functions.contains_key(&mangled_name) {
                    let _ = self.mark_function_reachable(&mangled_name);
                }
            }
        } else if let Some(struct_decl) = self.structs.get(base_type).cloned() {
            // If not found, try with base type
            self.reachable_types.insert(type_name.to_string());
            self.reachable_types.insert(base_type.to_string());

            // Mark field types as reachable
            for field in &struct_decl.fields {
                self.mark_type_reachable(&field.type_name);
            }

            // Mark methods as reachable if the type is used
            // Methods are converted to mangled function names during desugaring
            for method in &struct_decl.methods {
                let mangled_name = format!("{}_{}", type_name, method.name);
                if self.functions.contains_key(&mangled_name) {
                    let _ = self.mark_function_reachable(&mangled_name);
                }
            }
        }
    }

    /// Mark a global variable as reachable
    fn mark_global_reachable(&mut self, global_name: &str) {
        self.reachable_globals.insert(global_name.to_string());
    }

    /// Analyze statement dependencies
    fn mark_stmt_dependencies(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let { value, .. } => {
                self.mark_expr_dependencies(value)?;
            }
            Stmt::Expression(expr) => {
                self.mark_expr_dependencies(expr)?;
            }
            Stmt::Return(expr) => {
                self.mark_expr_dependencies(expr)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.mark_expr_dependencies(condition)?;
                for stmt in then_branch {
                    self.mark_stmt_dependencies(stmt)?;
                }
                if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.mark_stmt_dependencies(stmt)?;
                    }
                }
            }
            Stmt::While { condition, body } => {
                self.mark_expr_dependencies(condition)?;
                for stmt in body {
                    self.mark_stmt_dependencies(stmt)?;
                }
            }
            Stmt::Assign { name, value } => {
                self.mark_global_reachable(name);
                self.mark_expr_dependencies(value)?;
            }
            Stmt::VectorPush { vector, value } => {
                self.mark_global_reachable(vector);
                self.mark_expr_dependencies(value)?;
            }
            Stmt::IndexAssign {
                container,
                index,
                value,
                ..
            } => {
                self.mark_global_reachable(container);
                self.mark_expr_dependencies(index)?;
                self.mark_expr_dependencies(value)?;
            }
            Stmt::FieldAssign {
                object,
                field: _,
                value,
            } => {
                self.mark_expr_dependencies(object)?;
                self.mark_expr_dependencies(value)?;
            }
        }
        Ok(())
    }

    /// Analyze expression dependencies
    fn mark_expr_dependencies(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Call { callee, args } => {
                // Mark function call as reachable
                if let Expr::Identifier(func_name) = callee.as_ref() {
                    self.mark_function_reachable(func_name)?;
                }
                self.mark_expr_dependencies(callee)?;
                for arg in args {
                    self.mark_expr_dependencies(arg)?;
                }
            }
            Expr::Identifier(name) => {
                // This could be a global variable reference
                if self.globals.contains_key(name) {
                    self.mark_global_reachable(name);
                }
            }
            Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => {
                if let Some(obj) = object {
                    self.mark_expr_dependencies(obj)?;
                }
                for arg in args {
                    self.mark_expr_dependencies(arg)?;
                }

                // For method calls, try to determine the mangled function name
                if let Some(type_name_str) = type_name {
                    let mangled_name = format!("{}_{}", type_name_str, method);
                    if self.functions.contains_key(&mangled_name) {
                        self.mark_function_reachable(&mangled_name)?;
                    }
                }
            }
            Expr::StructNew {
                type_name,
                fields,
                kind: _,
            } => {
                self.mark_type_reachable(type_name);
                // Mark the base type as reachable as well (for generic instantiations)
                if let Some(paren_pos) = type_name.find('(') {
                    let base_type = &type_name[..paren_pos];
                    self.mark_type_reachable(base_type);
                }
                for (_, field_expr) in fields {
                    self.mark_expr_dependencies(field_expr)?;
                }
            }
            Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                self.mark_type_reachable(element_type);
                for value in initial_values {
                    self.mark_expr_dependencies(value)?;
                }
            }
            Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => {
                self.mark_type_reachable(key_type);
                self.mark_type_reachable(value_type);
                for (key, value) in initial_pairs {
                    self.mark_expr_dependencies(key)?;
                    self.mark_expr_dependencies(value)?;
                }
            }
            Expr::Binary { left, right, .. } => {
                self.mark_expr_dependencies(left)?;
                self.mark_expr_dependencies(right)?;
            }
            Expr::Index {
                container, index, ..
            } => {
                self.mark_expr_dependencies(container)?;
                self.mark_expr_dependencies(index)?;
            }
            Expr::FieldAccess { object, .. } => {
                self.mark_expr_dependencies(object)?;
            }
            Expr::Alloc {
                element_type,
                kind: _,
                size,
                initial_values,
            } => {
                self.mark_type_reachable(element_type);
                if let Some(size_expr) = size {
                    self.mark_expr_dependencies(size_expr)?;
                }
                if let Some(values) = initial_values {
                    for value in values {
                        self.mark_expr_dependencies(value)?;
                    }
                }
            }
            Expr::TypeExpr { type_name } => {
                self.mark_type_reachable(type_name);
            }
            // Leaf expressions don't have dependencies
            Expr::Int(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Byte(_) => {}
        }
        Ok(())
    }

    /// Remove unused declarations from the program
    fn sweep_unused_code(&self, program: Program) -> Result<Program> {
        let mut new_declarations = Vec::new();

        for decl in program.declarations {
            match &decl {
                Decl::Function(func) => {
                    if self.reachable_functions.contains(&func.name) {
                        new_declarations.push(decl);
                    }
                }
                Decl::Struct(struct_decl) => {
                    if self.reachable_types.contains(&struct_decl.name) {
                        new_declarations.push(decl);
                    }
                }
                Decl::GlobalVariable(global_var) => {
                    if self.reachable_globals.contains(&global_var.name) {
                        new_declarations.push(decl);
                    }
                }
            }
        }

        Ok(Program {
            declarations: new_declarations,
        })
    }

    /// Get statistics about dead code elimination
    pub fn get_elimination_stats(&self) -> DeadCodeStats {
        DeadCodeStats {
            total_functions: self.functions.len(),
            reachable_functions: self.reachable_functions.len(),
            total_types: self.structs.len(),
            reachable_types: self.reachable_types.len(),
            total_globals: self.globals.len(),
            reachable_globals: self.reachable_globals.len(),
        }
    }
}

/// Statistics about dead code elimination
#[derive(Debug, Clone)]
pub struct DeadCodeStats {
    pub total_functions: usize,
    pub reachable_functions: usize,
    pub total_types: usize,
    pub reachable_types: usize,
    pub total_globals: usize,
    pub reachable_globals: usize,
}

impl DeadCodeStats {
    pub fn eliminated_functions(&self) -> usize {
        self.total_functions
            .saturating_sub(self.reachable_functions)
    }

    pub fn eliminated_types(&self) -> usize {
        self.total_types.saturating_sub(self.reachable_types)
    }

    pub fn eliminated_globals(&self) -> usize {
        self.total_globals.saturating_sub(self.reachable_globals)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dead_code_elimination_removes_unused_function() {
        let mut eliminator = DeadCodeEliminator::new();

        let program = Program {
            declarations: vec![
                Decl::Function(Function {
                    name: "main".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Stmt::Return(Expr::Int(0))],
                }),
                Decl::Function(Function {
                    name: "unused".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Stmt::Return(Expr::Int(42))],
                }),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // Only main function should remain
        assert_eq!(result.declarations.len(), 1);
        if let Decl::Function(func) = &result.declarations[0] {
            assert_eq!(func.name, "main");
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_dead_code_elimination_keeps_called_function() {
        let mut eliminator = DeadCodeEliminator::new();

        let program = Program {
            declarations: vec![
                Decl::Function(Function {
                    name: "main".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![
                        Stmt::Expression(Expr::Call {
                            callee: Box::new(Expr::Identifier("helper".to_string())),
                            args: vec![],
                        }),
                        Stmt::Return(Expr::Int(0)),
                    ],
                }),
                Decl::Function(Function {
                    name: "helper".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Stmt::Return(Expr::Int(42))],
                }),
                Decl::Function(Function {
                    name: "unused".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![Stmt::Return(Expr::Int(99))],
                }),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // main and helper should remain, unused should be eliminated
        assert_eq!(result.declarations.len(), 2);

        let function_names: Vec<String> = result
            .declarations
            .iter()
            .filter_map(|decl| {
                if let Decl::Function(func) = decl {
                    Some(func.name.clone())
                } else {
                    None
                }
            })
            .collect();

        assert!(function_names.contains(&"main".to_string()));
        assert!(function_names.contains(&"helper".to_string()));
        assert!(!function_names.contains(&"unused".to_string()));
    }

    #[test]
    fn test_dead_code_elimination_keeps_used_types() {
        let mut eliminator = DeadCodeEliminator::new();

        let program = Program {
            declarations: vec![
                Decl::Function(Function {
                    name: "main".to_string(),
                    type_params: vec![],
                    params: vec![],
                    body: vec![
                        Stmt::Let {
                            name: "point".to_string(),
                            value: Expr::StructNew {
                                type_name: "Point".to_string(),
                                fields: vec![
                                    ("x".to_string(), Expr::Int(1)),
                                    ("y".to_string(), Expr::Int(2)),
                                ],
                                kind: StructNewKind::Regular,
                            },
                        },
                        Stmt::Return(Expr::Int(0)),
                    ],
                }),
                Decl::Struct(StructDecl {
                    name: "Point".to_string(),
                    type_params: vec![],
                    fields: vec![
                        crate::ast::StructField {
                            name: "x".to_string(),
                            type_name: "int".to_string(),
                        },
                        crate::ast::StructField {
                            name: "y".to_string(),
                            type_name: "int".to_string(),
                        },
                    ],
                    methods: vec![],
                }),
                Decl::Struct(StructDecl {
                    name: "UnusedStruct".to_string(),
                    type_params: vec![],
                    fields: vec![],
                    methods: vec![],
                }),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // main and Point should remain, UnusedStruct should be eliminated
        assert_eq!(result.declarations.len(), 2);

        let has_point = result
            .declarations
            .iter()
            .any(|decl| matches!(decl, Decl::Struct(s) if s.name == "Point"));
        let has_unused = result
            .declarations
            .iter()
            .any(|decl| matches!(decl, Decl::Struct(s) if s.name == "UnusedStruct"));

        assert!(has_point);
        assert!(!has_unused);
    }
}
