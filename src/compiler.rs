use crate::ast::{Program, StructNewKind};
use crate::codegen::CodeGenerator;
use crate::dead_code_elimination::DeadCodeEliminator;
use crate::desugar::Desugarer;
use crate::label_resolution::LabelResolver;
use crate::lexer::Lexer;
use crate::monomorphization::Monomorphizer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use crate::typecheck::TypeChecker;
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
    /// Enable heap visualization during execution
    pub print_heaps: bool,
    /// Enable stack printing for specific function calls
    pub print_stacks_on_call: Option<String>,
    /// Enable profiling
    pub enable_profiling: bool,
    /// Output file for profiling results
    pub profile_output: Option<String>,
    /// Enable automatic loading of standard library
    pub enable_load_std: bool,
    /// Enable dumping desugared code
    pub dump_desugared_code: bool,
    /// Output file for desugared code dump
    pub dump_desugared_code_output: Option<String>,
    /// Enable dumping monomorphized code
    pub dump_monomorphized_code: bool,
    /// Output file for monomorphized code dump
    pub dump_monomorphized_code_output: Option<String>,
    /// Enable dumping dead code elimination results
    pub dump_dce_code: bool,
    /// Output file for dead code elimination dump
    pub dump_dce_code_output: Option<String>,
    /// Disable dead code elimination for debugging
    pub no_dead_code_elimination: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            dump_ir: false,
            dump_ir_output: None,
            print_stacks: false,
            print_heaps: false,
            print_stacks_on_call: None,
            enable_profiling: false,
            profile_output: None,
            enable_load_std: true,
            dump_desugared_code: false,
            dump_desugared_code_output: None,
            dump_monomorphized_code: false,
            dump_monomorphized_code_output: None,
            dump_dce_code: false,
            dump_dce_code_output: None,
            no_dead_code_elimination: false,
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
        let runtime = if options.print_stacks
            || options.print_heaps
            || options.print_stacks_on_call.is_some()
        {
            Runtime::new_with_debug_options(
                options.print_stacks,
                options.print_heaps,
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

        let std_lib_path = "lib/std.ob";
        match std::fs::read_to_string(std_lib_path) {
            Ok(std_content) => Ok(format!("{}\n{}", std_content, code)),
            Err(_) => {
                // If std.ob doesn't exist, just return the original code
                Ok(code.to_string())
            }
        }
    }

    /// Execute a parsed program with all configured options
    fn execute_program(&mut self, program: Program) -> Result<Option<Value>> {
        // 1. Type inference phase: analyze types and set type_name information
        let mut type_checker = TypeChecker::new();
        let mut program_with_type_info = program;

        // First register struct types and functions
        type_checker.check_program(&mut program_with_type_info)?;
        // Then perform type inference to set type_name fields
        type_checker.infer_types(&mut program_with_type_info)?;

        // 2. Monomorphization phase: collect and instantiate generic types
        let mut monomorphizer = Monomorphizer::new();
        monomorphizer.collect_targets(&program_with_type_info)?;
        monomorphizer.monomorphize()?;

        // Generate the monomorphized program with concrete types
        let monomorphized_program =
            monomorphizer.generate_monomorphized_program(&program_with_type_info)?;

        // Handle monomorphized code dumping if requested (early, before potential errors)
        if self.options.dump_monomorphized_code {
            let dump_output = self.format_monomorphized_program(&monomorphized_program);
            if let Some(output_file) = &self.options.dump_monomorphized_code_output {
                std::fs::write(output_file, &dump_output).map_err(|e| {
                    anyhow::anyhow!("Failed to write monomorphized code dump: {}", e)
                })?;
            } else {
                println!("=== Monomorphized Code ===");
                println!("{}", dump_output);
                println!(); // Add a blank line before continuing
            }
        }

        // 3. Desugar phase: transform method calls to function calls using type info
        let mut desugarer = Desugarer::new();
        let desugared_program = desugarer.desugar_program(monomorphized_program)?;

        // Handle desugared code dumping if requested
        if self.options.dump_desugared_code {
            let dump_output = self.format_desugared_program(&desugared_program);
            if let Some(output_file) = &self.options.dump_desugared_code_output {
                std::fs::write(output_file, &dump_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write desugared code dump: {}", e))?;
            } else {
                println!("=== Desugared Code ===");
                println!("{}", dump_output);
                println!(); // Add a blank line before continuing
            }
        }

        // 4. Dead code elimination phase: remove unused functions and types
        let mut final_program = if self.options.no_dead_code_elimination {
            desugared_program
        } else {
            let mut dce = DeadCodeEliminator::new();
            let dce_program = dce.eliminate_dead_code(desugared_program)?;

            // Handle dead code elimination dumping if requested
            if self.options.dump_dce_code {
                let dump_output = self.format_desugared_program(&dce_program);
                let stats = dce.get_elimination_stats();
                let mut output = format!("=== Dead Code Elimination Results ===\n");
                output.push_str(&format!(
                    "Functions eliminated: {} (kept {})\n",
                    stats.eliminated_functions(),
                    stats.reachable_functions
                ));
                output.push_str(&format!(
                    "Types eliminated: {} (kept {})\n",
                    stats.eliminated_types(),
                    stats.reachable_types
                ));
                output.push_str(&format!(
                    "Globals eliminated: {} (kept {})\n\n",
                    stats.eliminated_globals(),
                    stats.reachable_globals
                ));
                output.push_str(&dump_output);

                if let Some(output_file) = &self.options.dump_dce_code_output {
                    std::fs::write(output_file, &output)
                        .map_err(|e| anyhow::anyhow!("Failed to write DCE dump: {}", e))?;
                } else {
                    println!("{}", output);
                }
            }

            dce_program
        };

        // 5. Final type checking on final program
        // Reuse the original type checker to preserve struct information
        type_checker.check_program(&mut final_program)?;
        type_checker.infer_types(&mut final_program)?;

        // 6. Handle IR dumping if requested
        if self.options.dump_ir {
            let mut vm_compiler = CodeGenerator::new();
            let _instructions = vm_compiler.compile_program(&final_program);

            if let Some(dump_ir_output) = &self.options.dump_ir_output {
                vm_compiler
                    .dump_ir_to_file(dump_ir_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;
            } else {
                // Default behavior: dump to stdout
                println!("{}", vm_compiler.dump_ir());
            }
        }

        // 7. Enable profiling if requested
        if self.options.enable_profiling {
            self.runtime.enable_profiling();
        }

        // 8. Execute the program
        let result = if self.options.print_stacks || self.options.print_stacks_on_call.is_some() {
            self.runtime
                .execute_program_with_options(&final_program, self.options.print_stacks)
        } else {
            self.runtime.execute_program(&final_program)
        };

        // 9. Handle profiling output
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

    /// Compile Orbit source code to VM instructions without executing
    pub fn compile_to_instructions(&mut self, code: &str) -> Result<Vec<crate::vm::Instruction>> {
        let processed_code = self.preprocess_code(code)?;
        let tokens = self.tokenize(&processed_code)?;
        let program = self.parse(tokens)?;

        // Follow the same compilation pipeline as execute_program
        // 1. Type inference phase
        let mut type_checker = TypeChecker::new();
        let mut program_with_type_info = program;
        type_checker.check_program(&mut program_with_type_info)?;
        type_checker.infer_types(&mut program_with_type_info)?;

        // 2. Monomorphization phase
        let mut monomorphizer = Monomorphizer::new();
        monomorphizer.collect_targets(&program_with_type_info)?;
        monomorphizer.monomorphize()?;
        let monomorphized_program =
            monomorphizer.generate_monomorphized_program(&program_with_type_info)?;

        // 3. Desugar phase
        let mut desugarer = Desugarer::new();
        let desugared_program = desugarer.desugar_program(monomorphized_program)?;

        // 4. Dead code elimination phase
        let mut dce = DeadCodeEliminator::new();
        let mut final_program = dce.eliminate_dead_code(desugared_program)?;

        // 5. Final type checking
        let mut final_type_checker = TypeChecker::new();
        final_type_checker.check_program(&mut final_program)?;

        // 6. Compile to VM bytecode
        let mut vm_compiler = CodeGenerator::new();
        let instructions = vm_compiler.compile_program(&final_program);

        // 7. Label resolution phase
        let mut label_resolver = LabelResolver::new();
        let resolved_instructions = label_resolver.resolve_labels(instructions);

        Ok(resolved_instructions)
    }

    /// Format a desugared program as readable code
    fn format_desugared_program(&self, program: &Program) -> String {
        let mut output = String::new();
        output.push_str("// === Desugared Code Output ===\n");
        output.push_str("// This shows the program after desugaring transformations\n\n");

        for decl in &program.declarations {
            match &decl.value {
                crate::ast::Decl::Function(func) => {
                    output.push_str(&format!("fun {}(", func.value.name));
                    for (i, param) in func.value.params.iter().enumerate() {
                        if i > 0 {
                            output.push_str(", ");
                        }
                        output.push_str(&format!(
                            "{}: {}",
                            param.name,
                            param.type_name.as_ref().unwrap_or(&"unknown".to_string())
                        ));
                    }
                    output.push_str(") do\n");
                    for stmt in &func.value.body {
                        output.push_str(&format!("{}\n", self.format_statement(&stmt.value, 1)));
                    }
                    output.push_str("end\n\n");
                }
                crate::ast::Decl::Struct(struct_decl) => {
                    output.push_str(&format!("type {}", struct_decl.value.name));
                    if !struct_decl.value.type_params.is_empty() {
                        output.push('(');
                        for (i, param) in struct_decl.value.type_params.iter().enumerate() {
                            if i > 0 {
                                output.push_str(", ");
                            }
                            output.push_str(&format!("{}: type", param));
                        }
                        output.push(')');
                    }
                    output.push_str(" = struct {\n");
                    for field in &struct_decl.value.fields {
                        output.push_str(&format!("    {}: {},\n", field.name, field.type_name));
                    }
                    output.push_str("};\n\n");
                }
                crate::ast::Decl::GlobalVariable(var) => {
                    output.push_str(&format!(
                        "let {} = {};\n\n",
                        var.value.name,
                        self.format_expression(&var.value.value.value)
                    ));
                }
            }
        }

        output
    }

    /// Format a monomorphized program as readable code
    fn format_monomorphized_program(&self, program: &Program) -> String {
        let mut output = String::new();

        for decl in &program.declarations {
            match &decl.value {
                crate::ast::Decl::Function(func) => {
                    if !func.value.type_params.is_empty() {
                        output.push_str(&format!("fun {}(", func.value.name));
                        for (i, param) in func.value.type_params.iter().enumerate() {
                            if i > 0 {
                                output.push_str(", ");
                            }
                            output.push_str(&format!("{}: type", param));
                        }
                        for param in &func.value.params {
                            output.push_str(", ");
                            output.push_str(&format!(
                                "{}: {}",
                                param.name,
                                param.type_name.as_ref().unwrap_or(&"unknown".to_string())
                            ));
                        }
                        output.push_str(") do\n");
                        for stmt in &func.value.body {
                            output.push_str(&format!("    {}\n", self.format_statement(&stmt.value, 1)));
                        }
                        output.push_str("end\n\n");
                    } else {
                        output.push_str(&format!("fun {}(", func.value.name));
                        for (i, param) in func.value.params.iter().enumerate() {
                            if i > 0 {
                                output.push_str(", ");
                            }
                            output.push_str(&format!(
                                "{}: {}",
                                param.name,
                                param.type_name.as_ref().unwrap_or(&"unknown".to_string())
                            ));
                        }
                        output.push_str(") do\n");
                        for stmt in &func.value.body {
                            output.push_str(&format!("{}\n", self.format_statement(&stmt.value, 1)));
                        }
                        output.push_str("end\n\n");
                    }
                }
                crate::ast::Decl::Struct(struct_decl) => {
                    if !struct_decl.value.type_params.is_empty() {
                        output.push_str(&format!("type {}(", struct_decl.value.name));
                        for (i, param) in struct_decl.value.type_params.iter().enumerate() {
                            if i > 0 {
                                output.push_str(", ");
                            }
                            output.push_str(&format!("{}: type", param));
                        }
                        output.push_str(") = struct {\n");
                    } else {
                        output.push_str(&format!("type {} = struct {{\n", struct_decl.value.name));
                    }

                    for field in &struct_decl.value.fields {
                        output.push_str(&format!("    {}: {}\n", field.name, field.type_name));
                    }
                    output.push_str("};\n\n");
                }
                crate::ast::Decl::GlobalVariable(var) => {
                    output.push_str(&format!("let {} = /* ... */;\n\n", var.value.name));
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
                format!(
                    "{}let {} = {};",
                    indent,
                    name,
                    self.format_expression(&value.value)
                )
            }
            crate::ast::Stmt::Expression(expr) => {
                format!("{}{};", indent, self.format_expression(&expr.value))
            }
            crate::ast::Stmt::Return(expr) => {
                format!("{}return {};", indent, self.format_expression(&expr.value))
            }
            crate::ast::Stmt::Assign { lvalue, value } => {
                format!(
                    "{}{} = {};",
                    indent,
                    self.format_expression(&lvalue.value),
                    self.format_expression(&value.value)
                )
            }
            crate::ast::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut result = format!("{}if {} do\n", indent, self.format_expression(&condition.value));
                for stmt in then_branch {
                    result.push_str(&format!(
                        "{}\n",
                        self.format_statement(&stmt.value, indent_level + 1)
                    ));
                }
                if let Some(else_branch) = else_branch {
                    result.push_str(&format!("{}else do\n", indent));
                    for stmt in else_branch {
                        result.push_str(&format!(
                            "{}\n",
                            self.format_statement(&stmt.value, indent_level + 1)
                        ));
                    }
                }
                result.push_str(&format!("{}end", indent));
                result
            }
            crate::ast::Stmt::While { condition, body } => {
                let mut result =
                    format!("{}while {} do\n", indent, self.format_expression(&condition.value));
                for stmt in body {
                    result.push_str(&format!(
                        "{}\n",
                        self.format_statement(&stmt.value, indent_level + 1)
                    ));
                }
                result.push_str(&format!("{}end", indent));
                result
            }
            crate::ast::Stmt::VectorPush { vector, value, .. } => {
                format!(
                    "{}{}.push({});",
                    indent,
                    vector,
                    self.format_expression(&value.value)
                )
            }
        }
    }

    /// Format an expression as readable code
    fn format_expression(&self, expr: &crate::ast::Expr) -> String {
        match expr {
            crate::ast::Expr::Int(n) => n.to_string(),
            crate::ast::Expr::String(s) => format!("\"{}\"", s),
            crate::ast::Expr::Boolean(b) => b.to_string(),
            crate::ast::Expr::Byte(b) => b.to_string(),
            crate::ast::Expr::Identifier(name) => name.clone(),
            crate::ast::Expr::Binary { left, op, right } => {
                format!(
                    "{} {} {}",
                    self.format_expression(&left.value),
                    self.format_binary_op(op),
                    self.format_expression(&right.value)
                )
            }
            crate::ast::Expr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.format_expression(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", self.format_expression(&callee.value), args_str)
            }
            crate::ast::Expr::Index {
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
            crate::ast::Expr::FieldAccess { object, field } => {
                format!("{}.{}", self.format_expression(&object.value), field)
            }
            crate::ast::Expr::StructNew {
                type_name,
                fields,
                kind,
            } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, expr)| format!(".{} = {}", name, self.format_expression(&expr.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                if kind == &StructNewKind::Pattern {
                    format!("new(struct) {} {{ {} }}", type_name, fields_str)
                } else {
                    format!("new {} {{ {} }}", type_name, fields_str)
                }
            }
            crate::ast::Expr::VectorNew {
                element_type,
                initial_values,
            } => {
                let elements_str = initial_values
                    .iter()
                    .map(|elem| self.format_expression(&elem.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("vec({}, {})", element_type, elements_str)
            }
            crate::ast::Expr::MapNew {
                key_type,
                value_type,
                initial_pairs,
            } => {
                let entries_str = initial_pairs
                    .iter()
                    .map(|(key, value)| {
                        format!(
                            "{}: {}",
                            self.format_expression(&key.value),
                            self.format_expression(&value.value)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("map({}, {}, {})", key_type, value_type, entries_str)
            }
            crate::ast::Expr::Alloc { element_type, size } => {
                format!("alloc({}, {})", element_type, self.format_expression(&size.value))
            }
            crate::ast::Expr::MethodCall {
                object,
                type_name,
                method,
                args,
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.format_expression(&arg.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(obj) = object {
                    format!("{}.{}({})", self.format_expression(&obj.value), method, args_str)
                } else if let Some(type_name) = type_name {
                    format!("(type {}).{}({})", type_name, method, args_str)
                } else {
                    format!("<unknown>.{}({})", method, args_str)
                }
            }
            crate::ast::Expr::TypeExpr { type_name } => type_name.to_string(),
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
    let captured_output = compiler
        .runtime_mut()
        .take_captured_output()
        .unwrap_or_default();

    Ok((result, captured_output))
}
