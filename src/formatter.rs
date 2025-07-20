use crate::ast::{BinaryOp, Decl, Expr, Program, Stmt, StructNewKind};
use std::io::Write;

/// A formatter for Orbit programs that outputs code in readable format
pub struct OrbitFormatter;

impl OrbitFormatter {
    /// Create a new formatter instance
    pub fn new() -> Self {
        Self
    }

    /// Format a desugared program to a writer
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

        for decl in &program.declarations {
            self.format_declaration(&decl.value, writer)?;
        }

        Ok(())
    }

    /// Format a monomorphized program to a writer
    pub fn format_monomorphized_program<W: Write>(
        &self,
        program: &Program,
        writer: &mut W,
    ) -> std::io::Result<()> {
        for decl in &program.declarations {
            self.format_declaration(&decl.value, writer)?;
        }

        Ok(())
    }

    /// Format a program to a writer (generic method for any program)
    pub fn format_program<W: Write>(
        &self,
        program: &Program,
        writer: &mut W,
    ) -> std::io::Result<()> {
        for decl in &program.declarations {
            self.format_declaration(&decl.value, writer)?;
        }

        Ok(())
    }

    /// Format a declaration to a writer
    fn format_declaration<W: Write>(&self, decl: &Decl, writer: &mut W) -> std::io::Result<()> {
        match decl {
            Decl::Function(func) => {
                write!(writer, "fun {}(", func.value.name)?;

                // Format type parameters first
                if !func.value.type_params.is_empty() {
                    for (i, param) in func.value.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }
                        write!(writer, "{}: type", param)?;
                    }
                    if !func.value.params.is_empty() {
                        write!(writer, ", ")?;
                    }
                }

                // Format regular parameters
                for (i, param) in func.value.params.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ", ")?;
                    }
                    write!(
                        writer,
                        "{}: {}",
                        param.name,
                        param
                            .param_type
                            .as_ref()
                            .map(|t| t.to_string())
                            .unwrap_or_else(|| "unknown".to_string())
                    )?;
                }

                writeln!(writer, ") do")?;

                // Format function body
                for stmt in &func.value.body {
                    self.format_statement(&stmt.value, 1, writer)?;
                    writeln!(writer)?;
                }

                writeln!(writer, "end")?;
                writeln!(writer)?;
            }
            Decl::Struct(struct_decl) => {
                write!(writer, "type {}", struct_decl.value.name)?;

                if !struct_decl.value.type_params.is_empty() {
                    write!(writer, "(")?;
                    for (i, param) in struct_decl.value.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }
                        write!(writer, "{}: type", param)?;
                    }
                    write!(writer, ")")?;
                }

                writeln!(writer, " = struct {{")?;

                for field in &struct_decl.value.fields {
                    writeln!(
                        writer,
                        "    {}: {},",
                        field.name,
                        field.field_type.to_string()
                    )?;
                }

                // Format methods if any exist
                if !struct_decl.value.methods.is_empty() {
                    writeln!(writer)?;
                    for method in &struct_decl.value.methods {
                        write!(writer, "    fun {}(", method.value.name)?;

                        // Format method parameters
                        for (i, param) in method.value.params.iter().enumerate() {
                            if i > 0 {
                                write!(writer, ", ")?;
                            }
                            write!(
                                writer,
                                "{}: {}",
                                param.name,
                                param
                                    .param_type
                                    .as_ref()
                                    .map(|t| t.to_string())
                                    .unwrap_or_else(|| "unknown".to_string())
                            )?;
                        }

                        writeln!(writer, ") do")?;

                        // Format method body
                        for stmt in &method.value.body {
                            self.format_statement(&stmt.value, 2, writer)?;
                            writeln!(writer)?;
                        }

                        writeln!(writer, "    end")?;
                        writeln!(writer)?;
                    }
                }

                writeln!(writer, "}};")?;
                writeln!(writer)?;
            }
            Decl::GlobalVariable(var) => {
                if let Some(ref value_expr) = var.value.value {
                    writeln!(
                        writer,
                        "let {} = {};",
                        var.value.name,
                        self.format_expression(&value_expr.value)
                    )?;
                } else {
                    writeln!(writer, "let {};", var.value.name)?;
                }
                writeln!(writer)?;
            }
        }

        Ok(())
    }

    /// Format a statement with proper indentation
    fn format_statement<W: Write>(
        &self,
        stmt: &Stmt,
        indent_level: usize,
        writer: &mut W,
    ) -> std::io::Result<()> {
        let indent = "    ".repeat(indent_level);

        match stmt {
            Stmt::Let { name, value } => {
                write!(
                    writer,
                    "{}let {} = {};",
                    indent,
                    name,
                    self.format_expression(&value.value)
                )?;
            }
            Stmt::Expression(expr) => {
                write!(writer, "{}{};", indent, self.format_expression(&expr.value))?;
            }
            Stmt::Return(expr) => {
                write!(
                    writer,
                    "{}return {};",
                    indent,
                    self.format_expression(&expr.value)
                )?;
            }
            Stmt::Assign { lvalue, value } => {
                write!(
                    writer,
                    "{}{} = {};",
                    indent,
                    self.format_expression(&lvalue.value),
                    self.format_expression(&value.value)
                )?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(
                    writer,
                    "{}if {} do",
                    indent,
                    self.format_expression(&condition.value)
                )?;

                for stmt in then_branch {
                    self.format_statement(&stmt.value, indent_level + 1, writer)?;
                    writeln!(writer)?;
                }

                if let Some(else_branch) = else_branch {
                    writeln!(writer, "{}else do", indent)?;
                    for stmt in else_branch {
                        self.format_statement(&stmt.value, indent_level + 1, writer)?;
                        writeln!(writer)?;
                    }
                }

                write!(writer, "{}end", indent)?;
            }
            Stmt::While { condition, body } => {
                writeln!(
                    writer,
                    "{}while {} do",
                    indent,
                    self.format_expression(&condition.value)
                )?;

                for stmt in body {
                    self.format_statement(&stmt.value, indent_level + 1, writer)?;
                    writeln!(writer)?;
                }

                write!(writer, "{}end", indent)?;
            }
            Stmt::VectorPush { vector, value, .. } => {
                write!(
                    writer,
                    "{}{}.push({});",
                    indent,
                    vector,
                    self.format_expression(&value.value)
                )?;
            }
        }

        Ok(())
    }

    /// Format an expression as readable code
    fn format_expression(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(n) => n.to_string(),
            Expr::String(s) => format!("\"{}\"", s),
            Expr::PushString(s) => format!("pushString(\"{}\")", s),
            Expr::Boolean(b) => b.to_string(),
            Expr::Byte(b) => b.to_string(),
            Expr::Identifier(name) => name.clone(),
            Expr::Binary { left, op, right } => {
                format!(
                    "{} {} {}",
                    self.format_expression(&left.value),
                    self.format_binary_op(op),
                    self.format_expression(&right.value)
                )
            }
            Expr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.format_expression(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.format_expression(&callee.value), args_str)
            }
            Expr::Index {
                container,
                index,
                container_type: _,
                container_value_type: _,
            } => {
                format!(
                    "{}[{}]",
                    self.format_expression(&container.value),
                    self.format_expression(&index.value)
                )
            }
            Expr::FieldAccess { object, field } => {
                format!("{}.{}", self.format_expression(&object.value), field)
            }
            Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, expr)| {
                        format!(".{} = {}", name, self.format_expression(&expr.value))
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
                    self.format_expression(&size.value)
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
                    .map(|arg| self.format_expression(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(obj) = object {
                    format!(
                        "{}.{}({})",
                        self.format_expression(&obj.value),
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
                self.format_expression(&expr.value),
                target_type.to_string()
            ),
        }
    }

    /// Format a binary operator
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

impl Default for OrbitFormatter {
    fn default() -> Self {
        Self::new()
    }
}
