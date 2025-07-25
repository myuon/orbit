use crate::ast::{PositionedError, Program};
use crate::codegen::CodeGenerator;
use crate::dead_code_elimination::DeadCodeEliminator;
use crate::desugar::Desugarer;
use crate::diagnostics::{Diagnostic, PositionCalculator};
use crate::formatter::OrbitFormatter;
use crate::label_resolution::LabelResolver;
use crate::lexer::Lexer;
use crate::monomorphization::Monomorphizer;
use crate::parser::Parser;
use crate::runtime::{Runtime, Value};
use crate::typecheck::TypeChecker;
use anyhow::{Context, Result};
use std::io::Write;
use std::time::Instant;

/// Compiler configuration options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Enable IR dumping (before label resolution)
    pub dump_ir: bool,
    /// Output file for IR dump
    pub dump_ir_output: Option<String>,
    /// Enable IR dumping after label resolution
    pub dump_ir_nolabel: bool,
    /// Output file for IR dump after label resolution
    pub dump_ir_nolabel_output: Option<String>,
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
    /// Print timing information for each compiler phase
    pub print_timings: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            dump_ir: false,
            dump_ir_output: None,
            dump_ir_nolabel: false,
            dump_ir_nolabel_output: None,
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
            print_timings: false,
        }
    }
}

/// The main compiler structure that orchestrates the compilation pipeline
pub struct Compiler {
    runtime: Runtime,
    options: CompilerOptions,
    /// Position calculator for converting byte offsets to line/column
    position_calculator: Option<PositionCalculator>,
    /// Current filename being processed
    current_filename: String,
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

        Compiler {
            runtime,
            options,
            position_calculator: None,
            current_filename: "<unknown>".to_string(),
        }
    }

    /// Compile and execute Orbit source code, returning the last value
    pub fn execute(&mut self, code: &str) -> Result<Option<Value>> {
        self.execute_with_filename(code, "<input>")
    }

    /// Compile and execute Orbit source code with a specific filename, returning the last value
    pub fn execute_with_filename(&mut self, code: &str, filename: &str) -> Result<Option<Value>> {
        self.current_filename = filename.to_string();

        let (processed_code, std_lib_content) = self
            .preprocess_code_with_std_info(code)
            .with_context(|| "Preprocessing phase: Failed to preprocess code")?;

        // Set up position calculator
        self.position_calculator = Some(PositionCalculator::new(
            processed_code.clone(),
            std_lib_content,
        ));

        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };
        let tokens = self.tokenize(&processed_code)?;
        if let Some(phase_start) = phase_start {
            eprintln!("Lexing: {}ms", phase_start.elapsed().as_millis());
        }

        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };
        let program = self.parse(tokens)?;
        if let Some(phase_start) = phase_start {
            eprintln!("Parsing: {}ms", phase_start.elapsed().as_millis());
        }

        self.execute_program(program)
    }

    /// Execute Orbit source code from a file
    pub fn execute_file(&mut self, filename: &str) -> Result<Option<Value>> {
        let content = std::fs::read_to_string(filename)
            .with_context(|| format!("File reading phase: Failed to read file {}", filename))?;
        self.execute_with_filename(&content, filename)
            .with_context(|| format!("Failed to execute file {}", filename))
    }

    /// Preprocess the code by prepending standard library if enabled
    fn preprocess_code(&self, code: &str) -> Result<String> {
        let (processed_code, _) = self.preprocess_code_with_std_info(code)?;
        Ok(processed_code)
    }

    /// Preprocess the code and return both the processed code and std lib content
    fn preprocess_code_with_std_info(&self, code: &str) -> Result<(String, Option<String>)> {
        if !self.options.enable_load_std {
            return Ok((code.to_string(), None));
        }

        let std_lib_path = "lib/std.ob";
        match std::fs::read_to_string(std_lib_path) {
            Ok(std_content) => Ok((format!("{}\n{}", std_content, code), Some(std_content))),
            Err(_) => {
                // If std.ob doesn't exist, just return the original code
                Ok((code.to_string(), None))
            }
        }
    }

    /// Execute a parsed program with all configured options
    fn execute_program(&mut self, program: Program) -> Result<Option<Value>> {
        let start_time = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };
        // 1. Type inference phase: analyze types and set type_name information
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut type_checker = TypeChecker::new();
        let mut program_with_type_info = program;

        // First register struct types and functions
        type_checker
            .check_program(&mut program_with_type_info)
            .map_err(|err| {
                let formatted_error = self.format_error_with_position(&err);
                anyhow::anyhow!("Type checking phase: {}", formatted_error)
            })?;
        // Then perform type inference to set type_name fields
        type_checker
            .infer_types(&mut program_with_type_info)
            .map_err(|err| {
                let formatted_error = self.format_error_with_position(&err);
                anyhow::anyhow!("Type checking phase: {}", formatted_error)
            })?;

        if let Some(phase_start) = phase_start {
            eprintln!("Type checking: {}ms", phase_start.elapsed().as_millis());
        }

        // 2. Monomorphization phase: collect and instantiate generic types
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut monomorphizer = Monomorphizer::new();
        monomorphizer
            .collect_targets(&program_with_type_info)
            .with_context(|| "Monomorphization phase: Failed to collect targets")?;
        monomorphizer
            .monomorphize()
            .with_context(|| "Monomorphization phase: Failed to monomorphize")?;

        // Generate the monomorphized program with concrete types
        let monomorphized_program = monomorphizer
            .generate_monomorphized_program(&program_with_type_info)
            .with_context(|| "Monomorphization phase: Failed to generate monomorphized program")?;

        if let Some(phase_start) = phase_start {
            eprintln!("Monomorphization: {}ms", phase_start.elapsed().as_millis());
        }

        // Handle monomorphized code dumping if requested (early, before potential errors)
        if self.options.dump_monomorphized_code {
            let formatter = OrbitFormatter::new();
            if let Some(output_file) = &self.options.dump_monomorphized_code_output {
                let mut file = std::fs::File::create(output_file).map_err(|e| {
                    anyhow::anyhow!("Failed to create monomorphized code dump file: {}", e)
                })?;
                formatter
                    .format_monomorphized_program(&monomorphized_program, &mut file)
                    .map_err(|e| {
                        anyhow::anyhow!("Failed to write monomorphized code dump: {}", e)
                    })?;
            } else {
                println!("=== Monomorphized Code ===");
                let mut stdout = std::io::stdout();
                formatter
                    .format_monomorphized_program(&monomorphized_program, &mut stdout)
                    .map_err(|e| {
                        anyhow::anyhow!("Failed to write monomorphized code to stdout: {}", e)
                    })?;
                println!(); // Add a blank line before continuing
            }
        }

        // 3. Desugar phase: transform method calls to function calls using type info
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut desugarer = Desugarer::new();
        let desugared_program = desugarer
            .desugar_program(monomorphized_program)
            .with_context(|| "Desugar phase: Failed to desugar program")?;

        if let Some(phase_start) = phase_start {
            eprintln!("Desugaring: {}ms", phase_start.elapsed().as_millis());
        }

        // Handle desugared code dumping if requested
        if self.options.dump_desugared_code {
            let formatter = OrbitFormatter::new();
            if let Some(output_file) = &self.options.dump_desugared_code_output {
                let mut file = std::fs::File::create(output_file).map_err(|e| {
                    anyhow::anyhow!("Failed to create desugared code dump file: {}", e)
                })?;
                formatter
                    .format_desugared_program(&desugared_program, &mut file)
                    .map_err(|e| anyhow::anyhow!("Failed to write desugared code dump: {}", e))?;
            } else {
                println!("=== Desugared Code ===");
                let mut stdout = std::io::stdout();
                formatter
                    .format_desugared_program(&desugared_program, &mut stdout)
                    .map_err(|e| {
                        anyhow::anyhow!("Failed to write desugared code to stdout: {}", e)
                    })?;
                println!(); // Add a blank line before continuing
            }
        }

        // 4. Dead code elimination phase: remove unused functions and types
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut final_program = if self.options.no_dead_code_elimination {
            desugared_program
        } else {
            let mut dce = DeadCodeEliminator::new();
            let dce_program = dce
                .eliminate_dead_code(desugared_program)
                .with_context(|| "Dead code elimination phase: Failed to eliminate dead code")?;

            // Handle dead code elimination dumping if requested
            if self.options.dump_dce_code {
                let formatter = OrbitFormatter::new();
                let stats = dce.get_elimination_stats();

                if let Some(output_file) = &self.options.dump_dce_code_output {
                    let mut file = std::fs::File::create(output_file)
                        .map_err(|e| anyhow::anyhow!("Failed to create DCE dump file: {}", e))?;

                    writeln!(file, "=== Dead Code Elimination Results ===")?;
                    writeln!(
                        file,
                        "Functions eliminated: {} (kept {})",
                        stats.eliminated_functions(),
                        stats.reachable_functions
                    )?;
                    writeln!(
                        file,
                        "Types eliminated: {} (kept {})",
                        stats.eliminated_types(),
                        stats.reachable_types
                    )?;
                    writeln!(
                        file,
                        "Globals eliminated: {} (kept {})\n",
                        stats.eliminated_globals(),
                        stats.reachable_globals
                    )?;

                    formatter
                        .format_program(&dce_program, &mut file)
                        .map_err(|e| anyhow::anyhow!("Failed to write DCE dump: {}", e))?;
                } else {
                    println!("=== Dead Code Elimination Results ===");
                    println!(
                        "Functions eliminated: {} (kept {})",
                        stats.eliminated_functions(),
                        stats.reachable_functions
                    );
                    println!(
                        "Types eliminated: {} (kept {})",
                        stats.eliminated_types(),
                        stats.reachable_types
                    );
                    println!(
                        "Globals eliminated: {} (kept {})\n",
                        stats.eliminated_globals(),
                        stats.reachable_globals
                    );

                    let mut stdout = std::io::stdout();
                    formatter
                        .format_program(&dce_program, &mut stdout)
                        .map_err(|e| {
                            anyhow::anyhow!("Failed to write DCE dump to stdout: {}", e)
                        })?;
                }
            }

            dce_program
        };

        if let Some(phase_start) = phase_start {
            eprintln!(
                "Dead code elimination: {}ms",
                phase_start.elapsed().as_millis()
            );
        }

        // 5. Final type checking on final program
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        // Create a fresh type checker to avoid duplicate struct definitions
        let mut final_type_checker = TypeChecker::new();
        final_type_checker
            .check_program(&mut final_program)
            .with_context(|| "Final type checking phase: Failed to check program")?;
        final_type_checker
            .infer_types(&mut final_program)
            .with_context(|| "Final type checking phase: Failed to infer types")?;

        if let Some(phase_start) = phase_start {
            eprintln!(
                "Final type checking: {}ms",
                phase_start.elapsed().as_millis()
            );
        }

        // 6. Code generation phase: compile to VM bytecode
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut vm_compiler = CodeGenerator::new();
        let instructions = vm_compiler.compile_program(&final_program);

        if let Some(phase_start) = phase_start {
            eprintln!("Code generation: {}ms", phase_start.elapsed().as_millis());
        }

        // Handle IR dumping before label resolution if requested
        if self.options.dump_ir {
            if let Some(dump_ir_output) = &self.options.dump_ir_output {
                vm_compiler
                    .dump_ir_to_file(dump_ir_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write IR dump: {}", e))?;
            } else {
                // Default behavior: dump to stdout
                println!("=== IR Before Label Resolution ===");
                println!("{}", vm_compiler.dump_ir());
            }
        }

        // 7. Label resolution phase: resolve jump labels to actual addresses
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let mut label_resolver = LabelResolver::new();
        let resolved_instructions = label_resolver
            .resolve_labels(instructions)
            .map_err(|e| anyhow::anyhow!("Label resolution error: {}", e))?;

        if let Some(phase_start) = phase_start {
            eprintln!("Label resolution: {}ms", phase_start.elapsed().as_millis());
        }

        // Handle IR dumping after label resolution if requested
        if self.options.dump_ir_nolabel {
            if let Some(dump_ir_nolabel_output) = &self.options.dump_ir_nolabel_output {
                // Create a temporary CodeGenerator to dump resolved instructions
                let mut temp_vm_compiler = CodeGenerator::new();
                temp_vm_compiler.load_instructions(resolved_instructions.clone());
                temp_vm_compiler
                    .dump_ir_to_file(dump_ir_nolabel_output)
                    .map_err(|e| anyhow::anyhow!("Failed to write IR dump (no label): {}", e))?;
            } else {
                // Default behavior: dump to stdout
                println!("=== IR After Label Resolution ===");
                let mut temp_vm_compiler = CodeGenerator::new();
                temp_vm_compiler.load_instructions(resolved_instructions.clone());
                println!("{}", temp_vm_compiler.dump_ir());
            }
        }

        // 8. Enable profiling if requested
        if self.options.enable_profiling {
            self.runtime.enable_profiling();
        }

        // 9. Execute the program using resolved instructions
        let phase_start = if self.options.print_timings {
            Some(Instant::now())
        } else {
            None
        };

        let result = if self.options.print_stacks || self.options.print_stacks_on_call.is_some() {
            self.runtime
                .execute_instructions_with_options(
                    &resolved_instructions,
                    self.options.print_stacks,
                )
                .with_context(|| "Execution phase: Failed to execute program with debug options")
        } else {
            self.runtime
                .execute_instructions(&resolved_instructions)
                .with_context(|| "Execution phase: Failed to execute program")
        };

        if let Some(phase_start) = phase_start {
            eprintln!("Execution: {}ms", phase_start.elapsed().as_millis());
        }

        // 10. Handle profiling output
        if self.options.enable_profiling {
            if let Some(output_file) = &self.options.profile_output {
                self.runtime
                    .dump_profile_to_file(output_file)
                    .map_err(|e| anyhow::anyhow!("Failed to write profile: {}", e))?;
            } else {
                println!("{}", self.runtime.get_profile());
            }
        }

        // Print total compilation time if requested
        if let Some(start_time) = start_time {
            eprintln!("Total: {}ms", start_time.elapsed().as_millis());
        }

        result
    }

    /// Convert a byte position to a diagnostic location
    pub fn position_to_location(
        &self,
        byte_offset: usize,
    ) -> Option<crate::diagnostics::SourceLocation> {
        self.position_calculator
            .as_ref()
            .map(|calc| calc.position_to_location(byte_offset, &self.current_filename))
    }

    /// Create a diagnostic from a span and message
    pub fn create_diagnostic_from_span(
        &self,
        span: &crate::ast::Span,
        message: String,
    ) -> Diagnostic {
        if let (Some(start_pos), Some(calc)) = (span.start, &self.position_calculator) {
            let location = calc.position_to_location(start_pos, &self.current_filename);
            Diagnostic::error_with_span(message, span.clone(), Some(location))
        } else {
            Diagnostic::error(message)
        }
    }

    /// Format a parse error with position information
    pub fn format_parse_error(&self, error: &anyhow::Error) -> String {
        let error_msg = error.to_string();

        // Look for "at position X" pattern in parse errors
        if let Some(pos_start) = error_msg.find("at position ") {
            if let Some(pos_str) = error_msg[pos_start + 12..].split_whitespace().next() {
                if let Ok(position) = pos_str.parse::<usize>() {
                    if let Some(location) = self.position_to_location(position) {
                        // Extract the actual error message before "at position"
                        let actual_msg = error_msg[..pos_start].trim();

                        let diagnostic = Diagnostic::error_at(actual_msg.to_string(), location);

                        // Use the position calculator to format with correct source context
                        if let Some(calc) = &self.position_calculator {
                            return diagnostic.format_with_calculator(calc);
                        }
                    }
                }
            }
        }

        // If no position found, use the general error formatter
        self.format_error_with_position(error)
    }

    /// Extract PositionedError from an anyhow::Error if it exists
    fn try_extract_positioned_error(&self, error: &anyhow::Error) -> Option<PositionedError> {
        // Try to downcast to PositionedError
        if let Some(positioned_error) = error.downcast_ref::<PositionedError>() {
            return Some(positioned_error.clone());
        }

        // Check if the error chain contains a PositionedError
        let mut current = error.source();
        while let Some(source) = current {
            if let Some(positioned_error) = source.downcast_ref::<PositionedError>() {
                return Some(positioned_error.clone());
            }
            current = source.source();
        }

        None
    }

    /// Format an error with position information
    pub fn format_error_with_position(&self, error: &anyhow::Error) -> String {
        // First try to extract structured PositionedError
        if let Some(positioned_error) = self.try_extract_positioned_error(error) {
            return self.format_positioned_error(&positioned_error);
        }

        // Fallback to standard error formatting
        format!("error: {}", error.to_string())
    }

    /// Format a positioned error using span information
    fn format_positioned_error(&self, positioned_error: &PositionedError) -> String {
        let error_msg = &positioned_error.value;
        let span = &positioned_error.span;

        // If we have span information and a position calculator, create a diagnostic
        if let (Some(start_pos), Some(calc)) = (span.start, &self.position_calculator) {
            let location = calc.position_to_location(start_pos, &self.current_filename);
            let diagnostic =
                Diagnostic::error_with_span(error_msg.clone(), span.clone(), Some(location));
            diagnostic.format_with_calculator(calc)
        } else {
            // Fallback to standard error formatting
            format!("error: {}", error_msg)
        }
    }

    /// Tokenize the source code
    fn tokenize(&self, code: &str) -> Result<Vec<crate::ast::Token>> {
        let mut lexer = Lexer::new(code);
        lexer
            .tokenize()
            .with_context(|| "Lexing phase: Failed to tokenize source code")
    }

    /// Parse tokens into a program
    fn parse(&self, tokens: Vec<crate::ast::Token>) -> Result<Program> {
        let mut parser = Parser::new(tokens);
        parser.parse_program().map_err(|err| {
            // Try to format the error with position information for parse errors
            let formatted_error = self.format_parse_error(&err);
            anyhow::anyhow!("Parsing phase: {}", formatted_error)
        })
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
        let resolved_instructions = label_resolver
            .resolve_labels(instructions)
            .map_err(|e| anyhow::anyhow!("Label resolution error: {}", e))?;

        Ok(resolved_instructions)
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
