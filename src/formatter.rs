use crate::ast::{BinaryOp, Decl, Expr, Program, Stmt, StructNewKind};
use crate::ast_visitor::Visitor;
use std::io::Write;

/// A formatter visitor that formats the AST as it traverses, writing to a writer
struct FormatterVisitor<'a, W: Write> {
    writer: &'a mut W,
    indent_level: usize,
}

impl<'a, W: Write> FormatterVisitor<'a, W> {
    fn new(writer: &'a mut W) -> Self {
        Self {
            writer,
            indent_level: 0,
        }
    }


    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    fn write(&mut self, s: &str) -> std::io::Result<()> {
        self.writer.write_all(s.as_bytes())
    }

    fn writeln(&mut self, s: &str) -> std::io::Result<()> {
        self.writer.write_all(s.as_bytes())?;
        self.writer.write_all(b"\n")
    }

    fn format_binary_op(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::Modulo => "%",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
        }
    }

}

impl<'a, W: Write> Visitor for FormatterVisitor<'a, W> {
    fn visit_expr(&mut self, expr: &crate::ast::PositionedExpr) {
        match &expr.value {
            Expr::Int(n) => {
                let _ = self.write(&n.to_string());
            }
            Expr::String(s) => {
                let _ = self.write(&format!("\"{}\"", s.replace("\"", "\\\"").replace("\n", "\\n")));
            }
            Expr::PushString(s) => {
                let _ = self.write(&format!(
                    "pushString(\"{}\")",
                    s.replace("\"", "\\\"").replace("\n", "\\n")
                ));
            }
            Expr::Boolean(b) => {
                let _ = self.write(&b.to_string());
            }
            Expr::Byte(b) => {
                let _ = self.write(&b.to_string());
            }
            Expr::Identifier(name) => {
                let _ = self.write(name);
            }
            Expr::Binary { left, op, right } => {
                self.visit_expr(left);
                let _ = self.write(" ");
                let _ = self.write(self.format_binary_op(op));
                let _ = self.write(" ");
                self.visit_expr(right);
            }
            Expr::Call { callee, args } => {
                self.visit_expr(callee);
                let _ = self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        let _ = self.write(", ");
                    }
                    self.visit_expr(arg);
                }
                let _ = self.write(")");
            }
            Expr::Index { container, index, .. } => {
                self.visit_expr(container);
                let _ = self.write("[");
                self.visit_expr(index);
                let _ = self.write("]");
            }
            Expr::FieldAccess { object, field, .. } => {
                self.visit_expr(object);
                let _ = self.write(".");
                let _ = self.write(field);
            }
            Expr::StructNew { type_name, fields, kind } => {
                if kind == &StructNewKind::Pattern {
                    let _ = self.write(&format!("new(struct) {} {{ ", type_name));
                } else {
                    let _ = self.write(&format!("new {} {{ ", type_name));
                }
                for (i, (name, field_expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        let _ = self.write(", ");
                    }
                    let _ = self.write(&format!(".{} = ", name));
                    self.visit_expr(field_expr);
                }
                let _ = self.write(" }");
            }
            Expr::Alloc { element_type, size } => {
                let _ = self.write(&format!("alloc({}, ", element_type));
                self.visit_expr(size);
                let _ = self.write(")");
            }
            Expr::MethodCall { object, object_type, method, args } => {
                if let Some(obj) = object {
                    self.visit_expr(obj);
                    let _ = self.write(&format!(".{}(", method));
                } else if let Some(obj_type) = object_type {
                    let _ = self.write(&format!("(type {}).{}(", obj_type, method));
                } else {
                    let _ = self.write(&format!("<unknown>.{}(", method));
                }
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        let _ = self.write(", ");
                    }
                    self.visit_expr(arg);
                }
                let _ = self.write(")");
            }
            Expr::TypeExpr { type_name } => {
                let _ = self.write(&type_name.to_string());
            }
            Expr::Sizeof { type_name } => {
                let _ = self.write(&format!("sizeof({})", type_name.to_string()));
            }
            Expr::Cast { expr, target_type } => {
                self.visit_expr(expr);
                let _ = self.write(&format!(" as {}", target_type.to_string()));
            }
        }
    }
    fn visit_decl(&mut self, decl: &crate::ast::PositionedDecl) {
        match &decl.value {
            Decl::Function(func) => {
                let _ = self.write(&format!("fun {}(", func.value.name));

                // Format type parameters first
                if !func.value.type_params.is_empty() {
                    for (i, param) in func.value.type_params.iter().enumerate() {
                        if i > 0 {
                            let _ = self.write(", ");
                        }
                        let _ = self.write(&format!("{}: type", param));
                    }
                    if !func.value.params.is_empty() {
                        let _ = self.write(", ");
                    }
                }

                // Format regular parameters
                for (i, param) in func.value.params.iter().enumerate() {
                    if i > 0 {
                        let _ = self.write(", ");
                    }
                    let _ = self.write(&format!(
                        "{}: {}",
                        param.name,
                        param
                            .param_type
                            .as_ref()
                            .map(|t| t.to_string())
                            .unwrap_or_else(|| "unknown".to_string())
                    ));
                }

                let _ = self.writeln(") do");

                // Format function body with increased indentation
                self.indent_level += 1;
                for stmt in &func.value.body {
                    self.visit_stmt(stmt);
                    let _ = self.writeln("");
                }
                self.indent_level -= 1;

                let _ = self.writeln("end");
                let _ = self.writeln("");
            }
            Decl::Struct(struct_decl) => {
                let _ = self.write(&format!("type {}", struct_decl.value.name));

                if !struct_decl.value.type_params.is_empty() {
                    let _ = self.write("(");
                    for (i, param) in struct_decl.value.type_params.iter().enumerate() {
                        if i > 0 {
                            let _ = self.write(", ");
                        }
                        let _ = self.write(&format!("{}: type", param));
                    }
                    let _ = self.write(")");
                }

                let _ = self.writeln(" = struct {");

                for field in &struct_decl.value.fields {
                    let _ = self.writeln(&format!(
                        "    {}: {},",
                        field.name,
                        field.field_type.to_string()
                    ));
                }

                // Format methods if any exist
                if !struct_decl.value.methods.is_empty() {
                    let _ = self.writeln("");
                    for method in &struct_decl.value.methods {
                        let _ = self.write(&format!("    fun {}(", method.value.name));

                        // Format method parameters
                        for (i, param) in method.value.params.iter().enumerate() {
                            if i > 0 {
                                let _ = self.write(", ");
                            }
                            let _ = self.write(&format!(
                                "{}: {}",
                                param.name,
                                param
                                    .param_type
                                    .as_ref()
                                    .map(|t| t.to_string())
                                    .unwrap_or_else(|| "unknown".to_string())
                            ));
                        }

                        let _ = self.writeln(") do");

                        // Format method body with increased indentation
                        self.indent_level += 2;
                        for stmt in &method.value.body {
                            self.visit_stmt(stmt);
                            let _ = self.writeln("");
                        }
                        self.indent_level -= 2;

                        let _ = self.writeln("    end");
                        let _ = self.writeln("");
                    }
                }

                let _ = self.writeln("};");
                let _ = self.writeln("");
            }
            Decl::GlobalVariable(var) => {
                if let Some(ref value_expr) = var.value.value {
                    let _ = self.write(&format!("let {} = ", var.value.name));
                    self.visit_expr(value_expr);
                    let _ = self.writeln(";");
                } else {
                    let _ = self.writeln(&format!("let {};", var.value.name));
                }
                let _ = self.writeln("");
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &crate::ast::PositionedStmt) {
        let indent = self.indent();
        
        match &stmt.value {
            Stmt::Let { name, value } => {
                let _ = self.write(&format!("{}let {} = ", indent, name));
                self.visit_expr(value);
                let _ = self.write(";");
            }
            Stmt::Expression(expr) => {
                let _ = self.write(&indent);
                self.visit_expr(expr);
                let _ = self.write(";");
            }
            Stmt::Return(expr) => {
                let _ = self.write(&format!("{}return ", indent));
                self.visit_expr(expr);
                let _ = self.write(";");
            }
            Stmt::Assign { lvalue, value } => {
                let _ = self.write(&indent);
                self.visit_expr(lvalue);
                let _ = self.write(" = ");
                self.visit_expr(value);
                let _ = self.write(";");
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let _ = self.write(&format!("{}if ", indent));
                self.visit_expr(condition);
                let _ = self.writeln(" do");

                self.indent_level += 1;
                for stmt in then_branch {
                    self.visit_stmt(stmt);
                    let _ = self.writeln("");
                }
                self.indent_level -= 1;

                if let Some(else_branch) = else_branch {
                    let _ = self.writeln(&format!("{}else do", indent));
                    self.indent_level += 1;
                    for stmt in else_branch {
                        self.visit_stmt(stmt);
                        let _ = self.writeln("");
                    }
                    self.indent_level -= 1;
                }

                let _ = self.write(&format!("{}end", indent));
            }
            Stmt::While { condition, body } => {
                let _ = self.write(&format!("{}while ", indent));
                self.visit_expr(condition);
                let _ = self.writeln(" do");

                self.indent_level += 1;
                for stmt in body {
                    self.visit_stmt(stmt);
                    let _ = self.writeln("");
                }
                self.indent_level -= 1;

                let _ = self.write(&format!("{}end", indent));
            }
            Stmt::VectorPush { vector, value, .. } => {
                let _ = self.write(&format!("{}{}.push(", indent, vector));
                self.visit_expr(value);
                let _ = self.write(");");
            }
        }
    }
}

/// A formatter for Orbit programs that outputs code in readable format
pub struct OrbitFormatter;

impl OrbitFormatter {
    /// Create a new formatter instance
    pub fn new() -> Self {
        Self
    }

    /// Format a desugared program to a writer using visitor pattern
    pub fn format_desugared_program<W: Write>(
        &self,
        program: &Program,
        writer: &mut W,
    ) -> std::io::Result<()> {
        writeln!(writer, "// === Desugared Code Output ===")?;
        writeln!(
            writer,
            "// This shows the program after desugaring transformations"
        )?;
        writeln!(writer)?;

        let mut visitor = FormatterVisitor::new(writer);
        for decl in &program.declarations {
            visitor.visit_decl(decl);
        }

        Ok(())
    }

    /// Format a monomorphized program to a writer using visitor pattern
    pub fn format_monomorphized_program<W: Write>(
        &self,
        program: &Program,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let mut visitor = FormatterVisitor::new(writer);
        for decl in &program.declarations {
            visitor.visit_decl(decl);
        }

        Ok(())
    }

    /// Format a program to a writer using visitor pattern (generic method for any program)
    pub fn format_program<W: Write>(
        &self,
        program: &Program,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let mut visitor = FormatterVisitor::new(writer);
        for decl in &program.declarations {
            visitor.visit_decl(decl);
        }

        Ok(())
    }

}

impl Default for OrbitFormatter {
    fn default() -> Self {
        Self::new()
    }
}
