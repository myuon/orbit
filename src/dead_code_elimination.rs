use crate::ast::{Decl, Expr, Function, PositionedExpr, Program, StructDecl, Type};
use crate::ast_visitor::{walk_decl, walk_expr, Visitor};
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
        let mut collector = DeclarationCollector::new(self);
        collector.visit_program(program)?;
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
            if let Some(ref value_expr) = global_var.value {
                let mut dependency_marker = DependencyMarker::new(self);
                dependency_marker.visit_expr(value_expr)?;
            }
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
                if let Some(type_name) = &param.param_type {
                    self.mark_type_reachable(type_name);
                }
            }

            // Analyze function body for dependencies using visitor
            for stmt in &function.body {
                let mut dependency_marker = DependencyMarker::new(self);
                dependency_marker.visit_stmt(stmt)?;
            }
        }

        Ok(())
    }

    /// Mark a type as reachable and analyze its dependencies
    fn extract_type_name(typ: &Type) -> Option<String> {
        match typ {
            Type::Struct { name, .. } => Some(name.clone()),
            Type::Pointer(inner) => Self::extract_type_name(inner),
            _ => None, // Primitive types don't need tracking
        }
    }

    fn mark_type_reachable(&mut self, typ: &Type) {
        if let Some(type_name) = Self::extract_type_name(typ) {
            if self.reachable_types.contains(&type_name) {
                return; // Already processed
            }

            // Extract base type name from generic instantiations like "array(int)"
            let base_type = if let Some(paren_pos) = type_name.find('(') {
                &type_name[..paren_pos]
            } else {
                &type_name
            };

            // Check if we have the exact type name first
            if let Some(struct_decl) = self.structs.get(&type_name).cloned() {
                self.reachable_types.insert(type_name.clone());

                // Mark field types as reachable
                for field in &struct_decl.fields {
                    self.mark_type_reachable(&field.field_type);
                }

                // Mark methods as reachable if the type is used
                // Methods are converted to mangled function names during desugaring
                for positioned_method in &struct_decl.methods {
                    let mangled_name = format!("{}_{}", type_name, positioned_method.value.name);
                    if self.functions.contains_key(&mangled_name) {
                        let _ = self.mark_function_reachable(&mangled_name);
                    }
                }
            } else if let Some(struct_decl) = self.structs.get(base_type).cloned() {
                // If not found, try with base type
                self.reachable_types.insert(type_name.clone());
                self.reachable_types.insert(base_type.to_string());

                // Mark field types as reachable
                for field in &struct_decl.fields {
                    self.mark_type_reachable(&field.field_type);
                }

                // Mark methods as reachable if the type is used
                // Methods are converted to mangled function names during desugaring
                for positioned_method in &struct_decl.methods {
                    let mangled_name = format!("{}_{}", type_name, positioned_method.value.name);
                    if self.functions.contains_key(&mangled_name) {
                        let _ = self.mark_function_reachable(&mangled_name);
                    }
                }
            }
        }
    }

    /// Mark a global variable as reachable
    fn mark_global_reachable(&mut self, global_name: &str) {
        self.reachable_globals.insert(global_name.to_string());
    }
}

/// Visitor for collecting all declarations in Phase 1
struct DeclarationCollector<'a> {
    eliminator: &'a mut DeadCodeEliminator,
}

impl<'a> DeclarationCollector<'a> {
    fn new(eliminator: &'a mut DeadCodeEliminator) -> Self {
        Self { eliminator }
    }
}

impl<'a> Visitor for DeclarationCollector<'a> {
    fn visit_decl(&mut self, positioned_decl: &crate::ast::PositionedDecl) -> Result<()> {
        match &positioned_decl.value {
            Decl::Function(positioned_func) => {
                self.eliminator.functions.insert(
                    positioned_func.value.name.clone(),
                    positioned_func.value.clone(),
                );
            }
            Decl::Struct(positioned_struct) => {
                self.eliminator.structs.insert(
                    positioned_struct.value.name.clone(),
                    positioned_struct.value.clone(),
                );
            }
            Decl::GlobalVariable(positioned_global) => {
                self.eliminator.globals.insert(
                    positioned_global.value.name.clone(),
                    positioned_global.value.clone(),
                );
            }
        }
        walk_decl(self, positioned_decl)?;
        Ok(())
    }
}

/// Visitor for marking dependencies in Phase 2
struct DependencyMarker<'a> {
    eliminator: &'a mut DeadCodeEliminator,
}

impl<'a> DependencyMarker<'a> {
    fn new(eliminator: &'a mut DeadCodeEliminator) -> Self {
        Self { eliminator }
    }
}

impl<'a> Visitor for DependencyMarker<'a> {
    fn visit_expr(&mut self, positioned_expr: &PositionedExpr) -> Result<()> {
        let expr = &positioned_expr.value;
        match expr {
            Expr::Call { callee, .. } => {
                // Mark function call as reachable
                if let Expr::Identifier(func_name) = &callee.value {
                    let _ = self.eliminator.mark_function_reachable(func_name);
                }
            }
            Expr::Identifier(name) => {
                // This could be a global variable reference
                if self.eliminator.globals.contains_key(name) {
                    self.eliminator.mark_global_reachable(name);
                }
            }
            Expr::MethodCall {
                object_type,
                method,
                ..
            } => {
                // For method calls, try to determine the mangled function name
                if let Some(obj_type) = object_type {
                    if let Type::Struct { name, args } = obj_type {
                        let type_name_str = if args.is_empty() {
                            name.clone()
                        } else {
                            let args_str = args
                                .iter()
                                .map(|t| t.to_string())
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("{}({})", name, args_str)
                        };
                        let mangled_name = format!("{}_{}", type_name_str, method);
                        if self.eliminator.functions.contains_key(&mangled_name) {
                            let _ = self.eliminator.mark_function_reachable(&mangled_name);
                        }
                    }
                }
            }
            Expr::StructNew { type_name, .. } => {
                self.eliminator.mark_type_reachable(type_name);

                // For any StructNew expression (both Pattern and Regular kind),
                // mark the exact type name as reachable to preserve the struct declaration
                let full_type_name = type_name.to_string();
                self.eliminator.reachable_types.insert(full_type_name);

                // Always mark base type for generic instantiations regardless of kind
                match type_name {
                    Type::Struct { name, .. } => {
                        let base_type = Type::Struct {
                            name: name.clone(),
                            args: vec![],
                        };
                        self.eliminator.mark_type_reachable(&base_type);
                    }
                    _ => {}
                }
            }
            Expr::Alloc { element_type, .. } => {
                self.eliminator.mark_type_reachable(element_type);
            }
            Expr::Sizeof { type_name } => {
                self.eliminator.mark_type_reachable(type_name);
            }
            Expr::Cast { target_type, .. } => {
                self.eliminator.mark_type_reachable(target_type);
            }
            Expr::TypeExpr { type_name } => {
                self.eliminator.mark_type_reachable(type_name);
            }
            _ => {}
        }
        walk_expr(self, positioned_expr)?;
        Ok(())
    }
}

impl DeadCodeEliminator {
    /// Remove unused declarations from the program
    fn sweep_unused_code(&self, program: Program) -> Result<Program> {
        let mut new_declarations = Vec::new();

        for positioned_decl in program.declarations {
            match &positioned_decl.value {
                Decl::Function(positioned_func) => {
                    if self
                        .reachable_functions
                        .contains(&positioned_func.value.name)
                    {
                        new_declarations.push(positioned_decl);
                    }
                }
                Decl::Struct(positioned_struct) => {
                    if self.reachable_types.contains(&positioned_struct.value.name) {
                        new_declarations.push(positioned_decl);
                    }
                }
                Decl::GlobalVariable(positioned_global) => {
                    if self
                        .reachable_globals
                        .contains(&positioned_global.value.name)
                    {
                        new_declarations.push(positioned_decl);
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
    use crate::ast::{Positioned, Stmt, StructNewKind};

    use super::*;

    #[test]
    fn test_dead_code_elimination_removes_unused_function() {
        let mut eliminator = DeadCodeEliminator::new();

        let program = Program {
            declarations: vec![
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "main".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![Positioned::with_unknown_span(Stmt::Return(
                            Positioned::with_unknown_span(Expr::Int(0)),
                        ))],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "unused".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![Positioned::with_unknown_span(Stmt::Return(
                            Positioned::with_unknown_span(Expr::Int(42)),
                        ))],
                    },
                ))),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // Only main function should remain
        assert_eq!(result.declarations.len(), 1);
        if let Decl::Function(func) = &result.declarations[0].value {
            assert_eq!(func.value.name, "main");
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_dead_code_elimination_keeps_called_function() {
        let mut eliminator = DeadCodeEliminator::new();

        let program = Program {
            declarations: vec![
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "main".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![
                            Positioned::with_unknown_span(Stmt::Expression(
                                Positioned::with_unknown_span(Expr::Call {
                                    callee: Box::new(Positioned::with_unknown_span(
                                        Expr::Identifier("helper".to_string()),
                                    )),
                                    args: vec![],
                                }),
                            )),
                            Positioned::with_unknown_span(Stmt::Return(
                                Positioned::with_unknown_span(Expr::Int(0)),
                            )),
                        ],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "helper".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![Positioned::with_unknown_span(Stmt::Return(
                            Positioned::with_unknown_span(Expr::Int(42)),
                        ))],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "unused".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![Positioned::with_unknown_span(Stmt::Return(
                            Positioned::with_unknown_span(Expr::Int(99)),
                        ))],
                    },
                ))),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // main and helper should remain, unused should be eliminated
        assert_eq!(result.declarations.len(), 2);

        let function_names: Vec<String> = result
            .declarations
            .iter()
            .filter_map(|positioned_decl| {
                if let Decl::Function(func) = &positioned_decl.value {
                    Some(func.value.name.clone())
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
                Positioned::with_unknown_span(Decl::Function(Positioned::with_unknown_span(
                    Function {
                        name: "main".to_string(),
                        type_params: vec![],
                        params: vec![],
                        body: vec![
                            Positioned::with_unknown_span(Stmt::Let {
                                name: "point".to_string(),
                                value: Positioned::with_unknown_span(Expr::StructNew {
                                    type_name: Type::Struct {
                                        name: "Point".to_string(),
                                        args: vec![],
                                    },
                                    fields: vec![
                                        (
                                            "x".to_string(),
                                            Positioned::with_unknown_span(Expr::Int(1)),
                                        ),
                                        (
                                            "y".to_string(),
                                            Positioned::with_unknown_span(Expr::Int(2)),
                                        ),
                                    ],
                                    kind: StructNewKind::Regular,
                                }),
                            }),
                            Positioned::with_unknown_span(Stmt::Return(
                                Positioned::with_unknown_span(Expr::Int(0)),
                            )),
                        ],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Struct(Positioned::with_unknown_span(
                    StructDecl {
                        name: "Point".to_string(),
                        type_params: vec![],
                        fields: vec![
                            crate::ast::StructField {
                                name: "x".to_string(),
                                field_type: Type::Int,
                            },
                            crate::ast::StructField {
                                name: "y".to_string(),
                                field_type: Type::Int,
                            },
                        ],
                        methods: vec![],
                    },
                ))),
                Positioned::with_unknown_span(Decl::Struct(Positioned::with_unknown_span(
                    StructDecl {
                        name: "UnusedStruct".to_string(),
                        type_params: vec![],
                        fields: vec![],
                        methods: vec![],
                    },
                ))),
            ],
        };

        let result = eliminator.eliminate_dead_code(program).unwrap();

        // main and Point should remain, UnusedStruct should be eliminated
        assert_eq!(result.declarations.len(), 2);

        let has_point = result
            .declarations
            .iter()
            .any(|positioned_decl| matches!(&positioned_decl.value, Decl::Struct(s) if s.value.name == "Point"));
        let has_unused = result
            .declarations
            .iter()
            .any(|positioned_decl| matches!(&positioned_decl.value, Decl::Struct(s) if s.value.name == "UnusedStruct"));

        assert!(has_point);
        assert!(!has_unused);
    }
}
