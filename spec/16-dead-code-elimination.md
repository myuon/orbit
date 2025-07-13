# 16. Dead Code Elimination

## Overview

Dead code elimination is a compiler optimization phase that removes unused functions, types, and global variables from the program. This phase occurs after monomorphization and before code generation to ensure only reachable code is included in the final program.

## Purpose

The dead code elimination phase serves several important purposes:

1. **Eliminate Duplicate Functions**: Remove generic function templates that have been replaced by monomorphized concrete instances
2. **Reduce Code Size**: Remove unreachable functions and unused types to minimize the generated code
3. **Improve Performance**: Reduce memory usage and improve cache locality by eliminating dead code
4. **Clean Up Monomorphization Artifacts**: Remove unused generic instantiations and duplicate method declarations

## Algorithm

The dead code elimination algorithm uses a mark-and-sweep approach:

### Phase 1: Mark Reachable Code

1. **Start from Entry Points**: Begin with the `main` function as the primary entry point
2. **Follow Function Calls**: Recursively mark all functions called from reachable code
3. **Mark Referenced Types**: Mark all types used in reachable functions
4. **Mark Global Variables**: Mark all global variables accessed from reachable code
5. **Handle Method Calls**: Mark struct methods that are called from reachable code

### Phase 2: Sweep Unused Code

1. **Remove Unmarked Functions**: Delete all function declarations that weren't marked as reachable
2. **Remove Unused Types**: Delete struct declarations that aren't referenced by reachable code
3. **Remove Unused Globals**: Delete global variable declarations that aren't accessed

## Implementation Details

### Dependency Analysis

The dependency analyzer tracks the following relationships:

- **Function → Function**: Direct function calls (`foo()`)
- **Function → Type**: Type usage in parameters, return types, and local variables
- **Function → Global**: Global variable access
- **Type → Type**: Field types in struct declarations
- **Method → Type**: Implicit `self` parameter type for methods

### Special Cases

#### Entry Points

The following are always considered entry points and marked as reachable:

- `main` function
- Functions with `#[export]` attributes (if implemented)
- Functions referenced in global variable initializers

#### Generic Functions

Generic function templates are removed if all their concrete instantiations are present:

- Original `Container(T).method` is removed
- Concrete `Container(int).method` is kept if reachable

#### Built-in Functions

Built-in functions and types from the standard library are subject to dead code elimination like user code.

## Debugging Support

Dead code elimination can be controlled with compiler flags:

- `--dump-eliminated-code`: Show what code was eliminated
- `--no-dead-code-elimination`: Disable the optimization for debugging

## Examples

### Before Dead Code Elimination

```orbit
// Generic function template (from standard library)
fun array(T)_new(length: int): array(T) do
    return new(struct) array(T) { .data = alloc(T, length), .length = length };
end

// Monomorphized concrete instance
fun array(int)_new(length: int): array(int) do
    return new(struct) array(int) { .data = alloc(int, length), .length = length };
end

// Unused function
fun unused_helper(): int do
    return 42;
end

fun main() do
    let arr = array(int)_new(10);
    return 0;
end
```

### After Dead Code Elimination

```orbit
// Only the concrete instance is kept
fun array(int)_new(length: int): array(int) do
    return new(struct) array(int) { .data = alloc(int, length), .length = length };
end

fun main() do
    let arr = array(int)_new(10);
    return 0;
end
```

The generic template `array(T)_new` and the unused function `unused_helper` are eliminated because they are not reachable from the `main` function.

## Integration with Other Phases

Dead code elimination works in conjunction with other compiler phases:

- **After Monomorphization**: Operates on the fully monomorphized program where all generics have been instantiated
- **Before Code Generation**: Ensures only necessary code is passed to the code generator
- **With Type Checking**: Relies on type information to understand dependencies between declarations

## Performance Considerations

Dead code elimination has minimal impact on compilation time:

- **Time Complexity**: O(n) where n is the number of declarations
- **Space Complexity**: O(n) for the reachability marking
- **Benefits**: Significantly reduces code generation time and runtime memory usage