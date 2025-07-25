use crate::ast::*;

/// Immutable visitor trait for traversing the AST without modification
pub trait Visitor {
    fn visit_expr(&mut self, e: &PositionedExpr) {
        walk_expr(self, e);
    }

    fn visit_stmt(&mut self, s: &PositionedStmt) {
        walk_stmt(self, s);
    }

    fn visit_decl(&mut self, d: &PositionedDecl) {
        walk_decl(self, d);
    }

    fn visit_program(&mut self, p: &Program) {
        walk_program(self, p);
    }

    fn visit_function(&mut self, f: &PositionedFunction) {
        walk_function(self, f);
    }

    fn visit_struct_decl(&mut self, s: &PositionedStructDecl) {
        walk_struct_decl(self, s);
    }

    fn visit_global_variable(&mut self, g: &PositionedGlobalVariable) {
        walk_global_variable(self, g);
    }
}

/// Mutable visitor trait for traversing and modifying the AST
pub trait VisitorMut {
    fn visit_expr(&mut self, e: &mut PositionedExpr) {
        walk_expr_mut(self, e);
    }

    fn visit_stmt(&mut self, s: &mut PositionedStmt) {
        walk_stmt_mut(self, s);
    }

    fn visit_decl(&mut self, d: &mut PositionedDecl) {
        walk_decl_mut(self, d);
    }

    fn visit_program(&mut self, p: &mut Program) {
        walk_program_mut(self, p);
    }

    fn visit_function(&mut self, f: &mut PositionedFunction) {
        walk_function_mut(self, f);
    }

    fn visit_struct_decl(&mut self, s: &mut PositionedStructDecl) {
        walk_struct_decl_mut(self, s);
    }

    fn visit_global_variable(&mut self, g: &mut PositionedGlobalVariable) {
        walk_global_variable_mut(self, g);
    }
}

// Walk functions for immutable visitors
pub fn walk_expr<V: Visitor + ?Sized>(visitor: &mut V, expr: &PositionedExpr) {
    let expr = &expr.value;
    match expr {
        Expr::Int(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Byte(_) | Expr::Identifier(_) => {}
        Expr::Binary { left, right, .. } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        Expr::Call { callee, args } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Expr::Index { container, index, .. } => {
            visitor.visit_expr(container);
            visitor.visit_expr(index);
        }
        Expr::StructNew { fields, .. } => {
            for (_, field_expr) in fields {
                visitor.visit_expr(field_expr);
            }
        }
        Expr::FieldAccess { object, .. } => {
            visitor.visit_expr(object);
        }
        Expr::MethodCall { object, args, .. } => {
            if let Some(obj) = object {
                visitor.visit_expr(obj);
            }
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Expr::TypeExpr { .. } => {}
        Expr::Alloc { size, .. } => {
            visitor.visit_expr(size);
        }
        Expr::Sizeof { .. } => {}
        Expr::Cast { expr, .. } => {
            visitor.visit_expr(expr);
        }
        Expr::PushString(_) => {}
    }
}

pub fn walk_stmt<V: Visitor + ?Sized>(visitor: &mut V, stmt: &PositionedStmt) {
    let stmt = &stmt.value;
    match stmt {
        Stmt::Let { value, .. } => {
            visitor.visit_expr(value);
        }
        Stmt::Expression(expr) => {
            visitor.visit_expr(expr);
        }
        Stmt::Return(expr) => {
            visitor.visit_expr(expr);
        }
        Stmt::If { condition, then_branch, else_branch } => {
            visitor.visit_expr(condition);
            for stmt in then_branch {
                visitor.visit_stmt(stmt);
            }
            if let Some(else_stmts) = else_branch {
                for stmt in else_stmts {
                    visitor.visit_stmt(stmt);
                }
            }
        }
        Stmt::While { condition, body } => {
            visitor.visit_expr(condition);
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::Assign { lvalue, value } => {
            visitor.visit_expr(lvalue);
            visitor.visit_expr(value);
        }
        Stmt::VectorPush { value, .. } => {
            visitor.visit_expr(value);
        }
    }
}

pub fn walk_decl<V: Visitor + ?Sized>(visitor: &mut V, decl: &PositionedDecl) {
    let decl = &decl.value;
    match decl {
        Decl::Function(func) => {
            visitor.visit_function(func);
        }
        Decl::Struct(struct_decl) => {
            visitor.visit_struct_decl(struct_decl);
        }
        Decl::GlobalVariable(global_var) => {
            visitor.visit_global_variable(global_var);
        }
    }
}

pub fn walk_program<V: Visitor + ?Sized>(visitor: &mut V, program: &Program) {
    for decl in &program.declarations {
        visitor.visit_decl(decl);
    }
}

pub fn walk_function<V: Visitor + ?Sized>(visitor: &mut V, func: &PositionedFunction) {
    let func = &func.value;
    for stmt in &func.body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_struct_decl<V: Visitor + ?Sized>(visitor: &mut V, struct_decl: &PositionedStructDecl) {
    let struct_decl = &struct_decl.value;
    for method in &struct_decl.methods {
        visitor.visit_function(method);
    }
}

pub fn walk_global_variable<V: Visitor + ?Sized>(visitor: &mut V, global_var: &PositionedGlobalVariable) {
    let global_var = &global_var.value;
    if let Some(value) = &global_var.value {
        visitor.visit_expr(value);
    }
}

// Walk functions for mutable visitors
pub fn walk_expr_mut<V: VisitorMut + ?Sized>(visitor: &mut V, expr: &mut PositionedExpr) {
    let expr = &mut expr.value;
    match expr {
        Expr::Int(_) | Expr::Boolean(_) | Expr::String(_) | Expr::Byte(_) | Expr::Identifier(_) => {}
        Expr::Binary { left, right, .. } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        Expr::Call { callee, args } => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Expr::Index { container, index, .. } => {
            visitor.visit_expr(container);
            visitor.visit_expr(index);
        }
        Expr::StructNew { fields, .. } => {
            for (_, field_expr) in fields {
                visitor.visit_expr(field_expr);
            }
        }
        Expr::FieldAccess { object, .. } => {
            visitor.visit_expr(object);
        }
        Expr::MethodCall { object, args, .. } => {
            if let Some(obj) = object {
                visitor.visit_expr(obj);
            }
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        Expr::TypeExpr { .. } => {}
        Expr::Alloc { size, .. } => {
            visitor.visit_expr(size);
        }
        Expr::Sizeof { .. } => {}
        Expr::Cast { expr, .. } => {
            visitor.visit_expr(expr);
        }
        Expr::PushString(_) => {}
    }
}

pub fn walk_stmt_mut<V: VisitorMut + ?Sized>(visitor: &mut V, stmt: &mut PositionedStmt) {
    let stmt = &mut stmt.value;
    match stmt {
        Stmt::Let { value, .. } => {
            visitor.visit_expr(value);
        }
        Stmt::Expression(expr) => {
            visitor.visit_expr(expr);
        }
        Stmt::Return(expr) => {
            visitor.visit_expr(expr);
        }
        Stmt::If { condition, then_branch, else_branch } => {
            visitor.visit_expr(condition);
            for stmt in then_branch {
                visitor.visit_stmt(stmt);
            }
            if let Some(else_stmts) = else_branch {
                for stmt in else_stmts {
                    visitor.visit_stmt(stmt);
                }
            }
        }
        Stmt::While { condition, body } => {
            visitor.visit_expr(condition);
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        Stmt::Assign { lvalue, value } => {
            visitor.visit_expr(lvalue);
            visitor.visit_expr(value);
        }
        Stmt::VectorPush { value, .. } => {
            visitor.visit_expr(value);
        }
    }
}

pub fn walk_decl_mut<V: VisitorMut + ?Sized>(visitor: &mut V, decl: &mut PositionedDecl) {
    let decl = &mut decl.value;
    match decl {
        Decl::Function(func) => {
            visitor.visit_function(func);
        }
        Decl::Struct(struct_decl) => {
            visitor.visit_struct_decl(struct_decl);
        }
        Decl::GlobalVariable(global_var) => {
            visitor.visit_global_variable(global_var);
        }
    }
}

pub fn walk_program_mut<V: VisitorMut + ?Sized>(visitor: &mut V, program: &mut Program) {
    for decl in &mut program.declarations {
        visitor.visit_decl(decl);
    }
}

pub fn walk_function_mut<V: VisitorMut + ?Sized>(visitor: &mut V, func: &mut PositionedFunction) {
    let func = &mut func.value;
    for stmt in &mut func.body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_struct_decl_mut<V: VisitorMut + ?Sized>(visitor: &mut V, struct_decl: &mut PositionedStructDecl) {
    let struct_decl = &mut struct_decl.value;
    for method in &mut struct_decl.methods {
        visitor.visit_function(method);
    }
}

pub fn walk_global_variable_mut<V: VisitorMut + ?Sized>(visitor: &mut V, global_var: &mut PositionedGlobalVariable) {
    let global_var = &mut global_var.value;
    if let Some(value) = &mut global_var.value {
        visitor.visit_expr(value);
    }
}

