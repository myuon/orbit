# 15. Command Line Interface (CLI) Specification

## Overview

The Orbit compiler provides a command-line interface for compiling and executing Orbit source files. The CLI supports various options for debugging, profiling, and development workflow.

## Basic Usage

```bash
orbit [OPTIONS] <file.ob>
```

The compiler takes a single Orbit source file (`.ob` extension) as input and executes it directly through the virtual machine.

## Command Line Options

### Required Arguments

- `<file.ob>` - Path to the Orbit source file to compile and execute. The file must have a `.ob` extension and contain valid Orbit code.

### Optional Flags

#### Debugging Options

- `--print-stacks` - Enable stack trace printing during execution. Shows the complete stack state after each VM instruction, useful for debugging program flow and identifying issues.

- `--print-stacks-on-call=<function_name>` - Enable stack trace printing only when calling a specific function. This reduces output volume while still providing detailed debugging information for the function of interest.

#### IR and Compilation Options

- `--dump-ir=<file>` - Dump the compiled intermediate representation (IR) to the specified file. The IR shows the VM bytecode instructions generated from the source code, useful for understanding compilation output and debugging compiler issues.

#### Profiling Options

- `--profile` - Enable execution profiling and print performance results to stdout. Collects timing information for instructions and function calls.

- `--profile-output=<file>` - Enable profiling and save the results to the specified file instead of printing to stdout.

#### Execution Control

- `--timeout=<time>` - Set execution timeout to prevent infinite loops or runaway programs. Time can be specified in seconds (`10s`), minutes (`5m`), or as a plain number (defaults to seconds). Program execution will be forcibly terminated after the specified duration.

#### Help

- `-h, --help` - Display help information and usage instructions.

## Examples

### Basic Execution

```bash
orbit hello_world.ob
```

Execute a simple Orbit program and display the result.

### Debugging with Stack Traces

```bash
orbit fibonacci.ob --print-stacks
```

Execute the program with full stack tracing enabled, showing the stack state after each instruction.

### Function-Specific Debugging

```bash
orbit fibonacci.ob --print-stacks-on-call=fibonacci
```

Show stack traces only when the `fibonacci` function is called, reducing output noise.

### IR Dump

```bash
orbit program.ob --dump-ir=program.ir
```

Compile the program and save the generated VM bytecode to `program.ir` for inspection.

### Profiling

```bash
orbit benchmark.ob --profile
```

Execute the program with profiling enabled and display performance metrics.

```bash
orbit benchmark.ob --profile-output=results.prof
```

Execute with profiling and save results to a file.

### Execution Control

```bash
orbit recursive_program.ob --timeout=10s
```

Execute the program with a 10-second timeout to prevent infinite recursion.

```bash
orbit long_running.ob --timeout=5m
```

Set a 5-minute timeout for programs that may take a long time to complete.

```bash
orbit fibonacci.ob --timeout=30
```

Use a 30-second timeout (time unit defaults to seconds when not specified).

### Combined Options

```bash
orbit complex_program.ob --dump-ir=debug.ir --print-stacks-on-call=main --profile --timeout=60s
```

Combine multiple debugging and analysis options for comprehensive program analysis with a 60-second timeout.

```bash
orbit debug_program.ob --timeout=10s --print-stacks --dump-ir=crash.ir
```

Debug a problematic program with timeout protection, full stack traces, and IR output for analysis.

## Exit Codes

- `0` - Successful execution
- `1` - Compilation error, runtime error, or invalid command line arguments

## Error Handling

### Command Line Errors

- **Missing file argument**: `Error: No input file specified`
- **Unknown option**: `Error: Unknown option: --invalid-flag`
- **Invalid option format**: `Error: --dump-ir option requires a filename`
- **Invalid timeout format**: `Error: Invalid timeout value: abc`

### File Errors

- **File not found**: `Error: No such file or directory`
- **Invalid file extension**: Files must have `.ob` extension
- **Permission errors**: Standard file system permission error messages

### Compilation Errors

- **Syntax errors**: Line and column information with error description
- **Type errors**: Type mismatch information with context
- **Semantic errors**: Variable not found, function not defined, etc.

### Runtime Errors

- **Stack overflow**: `Error: Stack overflow`
- **Division by zero**: `Error: Division by zero`
- **Array out of bounds**: `Error: Index out of bounds`
- **Execution timeout**: `Error: Execution timed out after 10 seconds`

## Integration with Development Workflow

### Testing

The CLI is designed to work well with testing frameworks and build systems:

```bash
# Run test and check exit code
orbit test_program.ob && echo "Test passed" || echo "Test failed"

# Compare output with expected results
orbit test_program.ob > actual_output.txt
diff expected_output.txt actual_output.txt
```

### Build Systems

Example integration with a Makefile:

```makefile
ORBIT = orbit
SOURCES = $(wildcard *.ob)
IR_FILES = $(SOURCES:.ob=.ir)

# Execute all programs
test: $(SOURCES)
	@for file in $(SOURCES); do \
		echo "Testing $$file"; \
		$(ORBIT) $$file; \
	done

# Generate IR for all sources
ir: $(IR_FILES)

%.ir: %.ob
	$(ORBIT) --dump-ir=$@ $<

# Profile specific program
profile:
	$(ORBIT) benchmark.ob --profile-output=benchmark.prof

.PHONY: test ir profile
```

### IDE Integration

The CLI output format is designed to be parseable by IDEs and editors:

- Error messages include file names, line numbers, and column information
- Stack traces show instruction addresses and function names
- Profiling output uses structured format for tool integration

## Implementation Notes

### Configuration

The CLI parser is implemented in `src/main.rs` with the following structure:

```rust
struct Config {
    filename: String,
    dump_ir: Option<String>,
    print_stacks: bool,
    print_stacks_on_call: Option<String>,
    profile: bool,
    profile_output: Option<String>,
    timeout: Option<u64>, // timeout in seconds
}
```

### Option Processing

- Options are processed in order from left to right
- Boolean flags can appear multiple times (last occurrence wins)
- File path options validate the provided path format
- Help flag short-circuits other processing

### Output Formats

#### Standard Output

Program return values are printed to stdout in their natural representation:

- Numbers: decimal format (e.g., `42`, `3.14`)
- Booleans: `true` or `false`
- Strings: quoted format (e.g., `"hello world"`)

#### Error Output

All error messages and debugging information go to stderr, keeping stdout clean for program output.

#### IR Output Format

The intermediate representation dump includes:

- Header with compilation metadata
- Numbered instruction listing
- Function labels and boundaries
- Comments explaining instruction purpose

#### Profiling Output Format

Profiling results include:

- Instruction execution counts and timings
- Function call statistics
- Total execution time
- Memory usage statistics (future enhancement)

#### Timeout Implementation

The timeout functionality is implemented using:

- **Tokio Runtime**: Async runtime for timeout management
- **Spawn Blocking**: Execution in a separate thread to allow cancellation
- **Duration Parsing**: Support for multiple time units (seconds, minutes)
- **Graceful Termination**: Clean process exit when timeout occurs

Timeout formats supported:
- `10s` - 10 seconds
- `5m` - 5 minutes  
- `30` - 30 seconds (default unit)

The implementation ensures that:
- Normal program execution is not affected by timeout overhead
- Programs that complete within the timeout run normally
- Infinite loops and runaway recursion are safely terminated
- Clear error messages indicate when timeout occurs

## Future Enhancements

### Planned Features

- `--verbose` flag for detailed compilation progress
- `--optimize` flag for enabling optimizations
- `--target` flag for specifying output format (bytecode, native)
- `--watch` flag for development mode with file watching
- `--check` flag for syntax checking without execution
- `--memory-limit` flag for setting memory usage limits
- `--instruction-limit` flag for limiting instruction execution count

### Configuration Files

Future versions may support configuration files for default options:

```toml
# orbit.toml
[debug]
print_stacks = false
default_dump_ir = "debug/"

[profile]
enabled = false
output_dir = "profiling/"

[execution]
default_timeout = "60s"
max_timeout = "300s"
```

## Standards Compliance

The CLI follows standard Unix conventions:

- Options use double-dash prefix (`--option`)
- Short options use single-dash prefix (`-h`)
- Help is available via `-h` and `--help`
- Exit codes follow standard conventions (0 for success, 1 for error)
- Error messages go to stderr, output goes to stdout