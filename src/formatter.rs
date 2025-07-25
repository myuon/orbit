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

    fn format_expression_to_string(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(n) => n.to_string(),
            Expr::String(s) => format!("\"{}\"", s.replace("\"", "\\\"").replace("\n", "\\n")),
            Expr::PushString(s) => format!(
                "pushString(\"{}\")",
                s.replace("\"", "\\\"").replace("\n", "\\n")
            ),
            Expr::Boolean(b) => b.to_string(),
            Expr::Byte(b) => b.to_string(),
            Expr::Identifier(name) => name.clone(),
            Expr::Binary { left, op, right } => {
                format!(
                    "{} {} {}",
                    self.format_expression_to_string(&left.value),
                    self.format_binary_op(op),
                    self.format_expression_to_string(&right.value)
                )
            }
            Expr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.format_expression_to_string(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.format_expression_to_string(&callee.value), args_str)
            }
            Expr::Index {
                container,
                index,
                ..
            } => {
                format!(
                    "{}[{}]",
                    self.format_expression_to_string(&container.value),
                    self.format_expression_to_string(&index.value)
                )
            }
            Expr::FieldAccess { object, field, .. } => {
                format!("{}.{}", self.format_expression_to_string(&object.value), field)
            }
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, expr)| {
                        format!(".{} = {}", name, self.format_expression_to_string(&expr.value))
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                if kind == &StructNewKind::Pattern {
                    format!("new(struct) {} {{ {} }}", type_name, fields_str)
                } else {
                    format!("new {} {{ {} }}", type_name, fields_str)
                }
            }
            Expr::Alloc { element_type, size } => {
                format!(
                    "alloc({}, {})",
                    element_type,
                    self.format_expression_to_string(&size.value)
                )
            }
            Expr::MethodCall {
                object,
                object_type,
                method,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.format_expression_to_string(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(obj) = object {
                    format!(
                        "{}.{}({})",
                        self.format_expression_to_string(&obj.value),
                        method,
                        args_str
                    )
                } else if let Some(obj_type) = object_type {
                    format!("(type {}).{}({})", obj_type, method, args_str)
                } else {
                    format!("<unknown>.{}({})", method, args_str)
                }
            }
            Expr::TypeExpr { type_name } => type_name.to_string(),
            Expr::Sizeof { type_name } => format!("sizeof({})", type_name.to_string()),
            Expr::Cast { expr, target_type } => format!(
                "{} as {}",
                self.format_expression_to_string(&expr.value),
                target_type.to_string()
            ),
        }
    }
}

impl<'a, W: Write> Visitor for FormatterVisitor<'a, W> {
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
                    let _ = self.writeln(&format!(
                        "let {} = {};",
                        var.value.name,
                        self.format_expression_to_string(&value_expr.value)
                    ));
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
                let _ = self.write(&format!(
                    "{}let {} = {};",
                    indent,
                    name,
                    self.format_expression_to_string(&value.value)
                ));
            }
            Stmt::Expression(expr) => {
                let _ = self.write(&format!("{}{};", indent, self.format_expression_to_string(&expr.value)));
            }
            Stmt::Return(expr) => {
                let _ = self.write(&format!(
                    "{}return {};",
                    indent,
                    self.format_expression_to_string(&expr.value)
                ));
            }
            Stmt::Assign { lvalue, value } => {
                let _ = self.write(&format!(
                    "{}{} = {};",
                    indent,
                    self.format_expression_to_string(&lvalue.value),
                    self.format_expression_to_string(&value.value)
                ));
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let _ = self.writeln(&format!(
                    "{}if {} do",
                    indent,
                    self.format_expression_to_string(&condition.value)
                ));

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
                let _ = self.writeln(&format!(
                    "{}while {} do",
                    indent,
                    self.format_expression_to_string(&condition.value)
                ));

                self.indent_level += 1;
                for stmt in body {
                    self.visit_stmt(stmt);
                    let _ = self.writeln("");
                }
                self.indent_level -= 1;

                let _ = self.write(&format!("{}end", indent));
            }
            Stmt::VectorPush { vector, value, .. } => {
                let _ = self.write(&format!(
                    "{}{}.push({});",
                    indent,
                    vector,
                    self.format_expression_to_string(&value.value)
                ));
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
