use crate::ast::Program;
use crate::desugar::Desugarer;
use crate::lexer::Lexer;
use crate::monomorphization::Monomorphizer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use crate::typecheck::TypeChecker;
use crate::vm::VMCompiler;
use anyhow::Result;

/// Compiler configuration options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Enable IR dumping
    pub dump_ir: bool,
    /// Output file for IR dump
    pub dump_ir_output: Option<String>,
    /// Enable stack printing during execution
    pub print_stacks: bool,
    /// Enable stack printing for specific function calls
    pub print_stacks_on_call: Option<String>,
    /// Enable profiling
    pub enable_profiling: bool,
    /// Output file for profiling results
    pub profile_output: Option<String>,
    /// Enable automatic loading of standard library
    pub enable_load_std: bool,
    /// Enable dumping monomorphized code
    pub dump_monomorphized_code: bool,
    /// Output file for monomorphized code dump
    pub dump_monomorphized_code_output: Option<String>,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            dump_ir: false,
            dump_ir_output: None,
            print_stacks: false,
            print_stacks_on_call: None,
            enable_profiling: false,
            profile_output: None,
            enable_load_std: true,
            dump_monomorphized_code: false,
            dump_monomorphized_code_output: None,
        }
    }
}

/// The main compiler structure that orchestrates the compilation pipeline
pub struct Compiler {
    runtime: Runtime,
    options: CompilerOptions,
}

impl Compiler {
    /// Create a new compiler instance with default options
    pub fn new() -> Self {
        Self::new_with_options(CompilerOptions::default())
    }

    /// Create a new compiler instance with specific options
    pub fn new_with_options(options: CompilerOptions) -> Self {
        let runtime = if options.print_stacks || options.print_stacks_on_call.is_some() {
            Runtime::new_with_call_tracing(
                options.print_stacks,
                options.print_stacks_on_call.clone(),
            )
        } else {
            Runtime::new()
        };

        Compiler { runtime, options }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        let processed_code = self.preprocess_code(code)?;
        let tokens = self.tokenize(&processed_code)?;
        let program = self.parse(tokens)?;
        self.execute_program(program)
    }

    /// Execute Orbit source code from a file
    pub fn execute_file(&mut self, filename: &str) -> Result<Option<Value>> {
        let content = std::fs::read_to_string(filename)
            .map_err(|e| anyhow::anyhow!("Error reading file {}: {}", filename, e))?;
        self.execute(&content)
    }

    /// Preprocess the code by prepending standard library if enabled
    fn preprocess_code(&self, code: &str) -> Result<String> {
        if !self.options.enable_load_std {
            return Ok(code.to_string());
        }

        let std_lib_path = "lib/std.io";
        match std::fs::read_to_string(std_lib_path) {
            Ok(std_content) => {
                Ok(format!("{}\n{}", std_content, code))
            }
            Err(_) => {
                // If std.io doesn't exist, just return the original code
                Ok(code.to_string())
            }
        }
    }

    /// Execute a parsed program with all configured options
    fn execute_program(&mut self, program: Program) -> Result<Option<Value>> {
        // 1. Type inference phase: analyze types and set object_type information
        let mut type_checker = TypeChecker::new();
        let mut program_with_type_info = program;

        // First register struct types and functions
        type_checker.check_program(&program_with_type_info)?;
        // Then perform type inference to set object_type fields
        type_checker.infer_types(&mut program_with_type_info)?;

        // 2. Monomorphization phase: collect and instantiate generic types
        let mut monomorphizer = Monomorphizer::new();
        monomorphizer.collect_targets(&program_with_type_info)?;
        monomorphizer.monomorphize()?;
        
        // Generate the monomorphized program with concrete types
        let monomorphized_program = monomorphizer.generate_monomorphized_program(&program_with_type_info)?;

        // Handle monomorphized code dumping if requested (early, before potential errors)
        if self.options.dump_monomorphized_code {
            let dump_output = self.format_monomorphized_program(&monomorphized_program);
            if let Some(output_file) = &self.options.dump_monomorphized_code_output {
                std::fs::write(output_file, &dump_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write monomorphized code dump: {}", e))?;
            } else {
                println!("=== Monomorphized Code ===");
                println!("{}", dump_output);
                println!(); // Add a blank line before continuing
            }
        }

        // 3. Desugar phase: transform method calls to function calls using type info
        let mut desugarer = Desugarer::new();
        let desugared_program = desugarer.desugar_program(monomorphized_program)?;

        // 4. Final type checking on desugared program
        let mut final_type_checker = TypeChecker::new();
        final_type_checker.check_program(&desugared_program)?;

        // 5. Handle IR dumping if requested
        if self.options.dump_ir {
            let mut vm_compiler = VMCompiler::new();
            let _instructions = vm_compiler.compile_program(&desugared_program);
            
            if let Some(dump_ir_output) = &self.options.dump_ir_output {
                vm_compiler
                    .dump_ir_to_file(dump_ir_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;
            } else {
                // Default behavior: dump to stdout
                println!("{}", vm_compiler.dump_ir());
            }
        }

        // 6. Enable profiling if requested
        if self.options.enable_profiling {
            self.runtime.enable_profiling();
        }

        // 7. Execute the program
        let result = if self.options.print_stacks || self.options.print_stacks_on_call.is_some() {
            self.runtime
                .execute_program_with_options(&desugared_program, self.options.print_stacks)
        } else {
            self.runtime.execute_program(&desugared_program)
        };

        // 8. Handle profiling output
        if self.options.enable_profiling {
            if let Some(output_file) = &self.options.profile_output {
                self.runtime
                    .dump_profile_to_file(output_file)
                    .map_err(|e| anyhow::anyhow!("Failed to write profile: {}", e))?;
            } else {
                println!("{}", self.runtime.get_profile());
            }
        }

        result
    }

    /// Tokenize the source code
    fn tokenize(&self, code: &str) -> Result<Vec<crate::ast::Token>> {
        let mut lexer = Lexer::new(code);
        lexer.tokenize()
    }

    /// Parse tokens into a program
    fn parse(&self, tokens: Vec<crate::ast::Token>) -> Result<Program> {
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    /// Get a mutable reference to the runtime for direct manipulation
    pub fn runtime_mut(&mut self) -> &mut Runtime {
        &mut self.runtime
    }

    /// Format a monomorphized program as readable code
    fn format_monomorphized_program(&self, program: &Program) -> String {
        let mut output = String::new();
        
        for decl in &program.declarations {
            match decl {
                crate::ast::Decl::Function(func) => {
                    if !func.type_params.is_empty() {
                        output.push_str(&format!("// Generic function: {}\n", func.name));
                        output.push_str(&format!("fun {}(", func.name));
                        for (i, param) in func.type_params.iter().enumerate() {
                            if i > 0 { output.push_str(", "); }
                            output.push_str(&format!("{}: type", param));
                        }
                        for param in &func.params {
                            output.push_str(", ");
                            output.push_str(&format!("{}: {}", param.name, param.type_name.as_ref().unwrap_or(&"unknown".to_string())));
                        }
                        output.push_str(") do\n");
                        for stmt in &func.body {
                            output.push_str(&format!("    {}\n", self.format_statement(stmt, 1)));
                        }
                        output.push_str("end\n\n");
                    } else {
                        output.push_str(&format!("// Monomorphized function: {}\n", func.name));
                        output.push_str(&format!("fun {}(", func.name));
                        for (i, param) in func.params.iter().enumerate() {
                            if i > 0 { output.push_str(", "); }
                            output.push_str(&format!("{}: {}", param.name, param.type_name.as_ref().unwrap_or(&"unknown".to_string())));
                        }
                        output.push_str(") do\n");
                        for stmt in &func.body {
                            output.push_str(&format!("    {}\n", self.format_statement(stmt, 1)));
                        }
                        output.push_str("end\n\n");
                    }
                }
                crate::ast::Decl::Struct(struct_decl) => {
                    if !struct_decl.type_params.is_empty() {
                        output.push_str(&format!("// Generic struct: {}\n", struct_decl.name));
                        output.push_str(&format!("type {}(", struct_decl.name));
                        for (i, param) in struct_decl.type_params.iter().enumerate() {
                            if i > 0 { output.push_str(", "); }
                            output.push_str(&format!("{}: type", param));
                        }
                        output.push_str(") = struct {\n");
                    } else {
                        output.push_str(&format!("// Monomorphized struct: {}\n", struct_decl.name));
                        output.push_str(&format!("type {} = struct {{\n", struct_decl.name));
                    }
                    
                    for field in &struct_decl.fields {
                        output.push_str(&format!("    {}: {}\n", field.name, field.type_name));
                    }
                    output.push_str("};\n\n");
                }
                crate::ast::Decl::GlobalVariable(var) => {
                    output.push_str(&format!("// Global variable: {}\n", var.name));
                    output.push_str(&format!("let {} = /* ... */;\n\n", var.name));
                }
            }
        }
        
        output
    }

    /// Format a statement as readable code with proper indentation
    fn format_statement(&self, stmt: &crate::ast::Stmt, indent_level: usize) -> String {
        let indent = "    ".repeat(indent_level);
        match stmt {
            crate::ast::Stmt::Let { name, value } => {
                format!("{}let {} = {};", indent, name, self.format_expression(value))
            }
            crate::ast::Stmt::Expression(expr) => {
                format!("{}{};", indent, self.format_expression(expr))
            }
            crate::ast::Stmt::Return(expr) => {
                format!("{}return {};", indent, self.format_expression(expr))
            }
            crate::ast::Stmt::Assign { name, value } => {
                format!("{}{} = {};", indent, name, self.format_expression(value))
            }
            crate::ast::Stmt::IndexAssign { container, index, value, container_type: _ } => {
                format!("{}{}[{}] = {};", indent, container, self.format_expression(index), self.format_expression(value))
            }
            crate::ast::Stmt::FieldAssign { object, field, value } => {
                format!("{}{}.{} = {};", indent, self.format_expression(object), field, self.format_expression(value))
            }
            crate::ast::Stmt::If { condition, then_branch, else_branch } => {
                let mut result = format!("{}if {} do\n", indent, self.format_expression(condition));
                for stmt in then_branch {
                    result.push_str(&format!("{}\n", self.format_statement(stmt, indent_level + 1)));
                }
                if let Some(else_branch) = else_branch {
                    result.push_str(&format!("{}else do\n", indent));
                    for stmt in else_branch {
                        result.push_str(&format!("{}\n", self.format_statement(stmt, indent_level + 1)));
                    }
                }
                result.push_str(&format!("{}end", indent));
                result
            }
            crate::ast::Stmt::While { condition, body } => {
                let mut result = format!("{}while {} do\n", indent, self.format_expression(condition));
                for stmt in body {
                    result.push_str(&format!("{}\n", self.format_statement(stmt, indent_level + 1)));
                }
                result.push_str(&format!("{}end", indent));
                result
            }
            crate::ast::Stmt::VectorPush { vector, value } => {
                format!("{}{}.push({});", indent, vector, self.format_expression(value))
            }
        }
    }

    /// Format an expression as readable code
    fn format_expression(&self, expr: &crate::ast::Expr) -> String {
        match expr {
            crate::ast::Expr::Int(n) => n.to_string(),
            crate::ast::Expr::String(s) => format!("\"{}\"", s),
            crate::ast::Expr::Boolean(b) => b.to_string(),
            crate::ast::Expr::Identifier(name) => name.clone(),
            crate::ast::Expr::Binary { left, op, right } => {
                format!("{} {} {}", self.format_expression(left), self.format_binary_op(op), self.format_expression(right))
            }
            crate::ast::Expr::Call { callee, args } => {
                let args_str = args.iter()
                    .map(|arg| self.format_expression(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.format_expression(callee), args_str)
            }
            crate::ast::Expr::Index { container, index, container_type: _ } => {
                format!("{}[{}]", self.format_expression(container), self.format_expression(index))
            }
            crate::ast::Expr::FieldAccess { object, field } => {
                format!("{}.{}", self.format_expression(object), field)
            }
            crate::ast::Expr::StructNew { type_name, fields } => {
                let fields_str = fields.iter()
                    .map(|(name, expr)| format!(".{} = {}", name, self.format_expression(expr)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("new {} {{ {} }}", type_name, fields_str)
            }
            crate::ast::Expr::StructNewPattern { type_name, fields } => {
                let fields_str = fields.iter()
                    .map(|(name, expr)| format!(".{} = {}", name, self.format_expression(expr)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("new(struct) {} {{ {} }}", type_name, fields_str)
            }
            crate::ast::Expr::VectorNew { element_type, initial_values } => {
                let elements_str = initial_values.iter()
                    .map(|elem| self.format_expression(elem))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("vec({}, {})", element_type, elements_str)
            }
            crate::ast::Expr::MapNew { key_type, value_type, initial_pairs } => {
                let entries_str = initial_pairs.iter()
                    .map(|(key, value)| format!("{}: {}", self.format_expression(key), self.format_expression(value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("map({}, {}, {})", key_type, value_type, entries_str)
            }
            crate::ast::Expr::Alloc { element_type, size } => {
                format!("alloc({}, {})", element_type, self.format_expression(size))
            }
            crate::ast::Expr::MethodCall { object, method, args, object_type: _ } => {
                let args_str = args.iter()
                    .map(|arg| self.format_expression(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}.{}({})", self.format_expression(object), method, args_str)
            }
            crate::ast::Expr::TypeExpr { type_name } => {
                type_name.clone()
            }
            crate::ast::Expr::PointerAlloc { element_type, initial_values } => {
                let values_str = initial_values.iter()
                    .map(|val| self.format_expression(val))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("pointer({}, {})", element_type, values_str)
            }
        }
    }

    /// Format a binary operator
    fn format_binary_op(&self, op: &crate::ast::BinaryOp) -> &'static str {
        match op {
            crate::ast::BinaryOp::Add => "+",
            crate::ast::BinaryOp::Subtract => "-",
            crate::ast::BinaryOp::Multiply => "*",
            crate::ast::BinaryOp::Divide => "/",
            crate::ast::BinaryOp::Equal => "==",
            crate::ast::BinaryOp::NotEqual => "!=",
            crate::ast::BinaryOp::Less => "<",
            crate::ast::BinaryOp::LessEqual => "<=",
            crate::ast::BinaryOp::Greater => ">",
            crate::ast::BinaryOp::GreaterEqual => ">=",
        }
    }
}

/// Execute Orbit source code and return the result
pub fn execute_code(code: &str) -> Result<Option<Value>> {
    let mut compiler = Compiler::new();
    compiler.execute(code)
}

/// Execute Orbit source code and return both the result and captured stdout
pub fn execute_code_with_output(code: &str) -> Result<(Option<Value>, String)> {
    let mut compiler = Compiler::new();
    
    // Enable output capture
    compiler.runtime_mut().enable_output_capture();
    
    // Execute the code
    let result = compiler.execute(code)?;
    
    // Get the captured output
    let captured_output = compiler.runtime_mut().take_captured_output().unwrap_or_default();
    
    Ok((result, captured_output))
}
