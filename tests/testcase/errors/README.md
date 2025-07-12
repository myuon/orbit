# Error Test Cases

This directory contains test cases that are expected to fail with specific error messages.

## File Structure

Each error test consists of two files:
- `{test_name}.ob` - The Orbit source code that should produce an error
- `{test_name}.stderr` - The expected error message fragment

## How It Works

The test runner:
1. Executes the `.ob` file using the Orbit compiler
2. Expects the execution to fail
3. Checks that the actual error message contains the text from the `.stderr` file
4. The match is done using substring matching, so the `.stderr` file only needs to contain a key fragment

## Test Categories

- **Type Errors**: `type_mismatch.ob` - Tests type system validation
- **Bounds Checking**: `string_out_of_bounds.ob` - Tests runtime bounds validation  
- **Immutability**: `string_immutable_assignment.ob` - Tests immutability enforcement
- **Syntax Errors**: `syntax_error.ob` - Tests parser error handling
- **Variable Resolution**: `undefined_variable.ob` - Tests variable scope validation

## Adding New Error Tests

1. Create a `.ob` file with code that should fail
2. Create a corresponding `.stderr` file with a key phrase from the expected error
3. Run `cargo test test_program_files` to verify the test works