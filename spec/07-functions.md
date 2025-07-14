# 7. Functions

## Function Overview

Functions in Orbit are first-class constructs that encapsulate reusable code. They can take parameters, return values, and be called from other functions.

## Function Declaration

### Basic Function Syntax

```orbit
fun function_name(parameter_list): return_type do
    // function body
    return value;
end
```

### Simple Function Example

```orbit
fun add(a: int, b: int): int do
    return a + b;
end
```

### Function Without Return Value

```orbit
fun print_hello() do
    // No return type specified
    return;  // Optional explicit return
end
```

## Function Parameters

### Parameter Declaration

Parameters are declared with name and type:

```orbit
fun greet(name: [*]byte, age: int): [*]byte do
    return "Hello";
end
```

### Parameter Passing

All parameters are passed by value:

```orbit
fun modify_value(x: int): int do
    x = x + 10;  // Modifies local copy
    return x;
end

let original = 5;
let result = modify_value(original);  // result = 15, original = 5
```

### Reference Parameters

Pointers can be used to simulate reference parameters:

```orbit
fun modify_through_pointer(ptr: [*]int) do
    *ptr = *ptr + 10;  // Modifies value through pointer
    return;
end
```

## Function Return Values

### Return Type Declaration

```orbit
fun calculate_square(x: int): int do
    return x * x;
end
```

### Multiple Return Paths

```orbit
fun classify_number(x: int): [*]byte do
    if (x > 0) do
        return "positive";
    else do
        if (x < 0) do
            return "negative";
        else do
            return "zero";
        end
    end
end
```

### Return Value Requirements

- Functions with return types must return a value on all execution paths
- Functions without return types can use `return;` or omit return statements

## Function Calls

### Simple Function Call

```orbit
let result = add(10, 5);  // result = 15
```

### Nested Function Calls

```orbit
let result = add(multiply(3, 4), subtract(10, 2));  // result = 20
```

### Function Call as Statement

```orbit
print_message("Hello, World!");  // Function call as statement
```

## Function Scope

### Local Variables

Variables declared inside functions are local to that function:

```orbit
fun example() do
    let local_var = 42;  // Local to example()
    return local_var;
end
```

### Global Variable Access

Functions can access and modify global variables:

```orbit
let global_counter = 0;

fun increment(): int do
    global_counter = global_counter + 1;
    return global_counter;
end
```

### Parameter Scope

Parameters are scoped to the function body:

```orbit
fun process(data: [*]byte, size: int): int do
    // data and size are accessible throughout the function
    let i = 0;
    while (i < size) do
        // process data[i]
        i = i + 1;
    end
    return 0;
end
```

## Generic Functions

### Generic Function Declaration

```orbit
fun generic_function(type_param: type, other_params...): return_type do
    // function body
end
```

### Generic Function Example

```orbit
fun get_first(A: type, B: type, first: A, second: B): A do
    return first;
end
```

### Generic Function Calls

```orbit
let result = get_first(int, [*]byte, 42, "hello");  // result = 42
let text = get_first([*]byte, int, "world", 100);   // text = "world"
```

## Function Overloading

Currently, Orbit does not support function overloading. Each function must have a unique name.

## Recursive Functions

Functions can call themselves recursively:

```orbit
fun factorial(n: int): int do
    if (n <= 1) do
        return 1;
    else do
        return n * factorial(n - 1);
    end
end
```

### Tail Recursion

```orbit
fun factorial_tail(n: int, accumulator: int): int do
    if (n <= 1) do
        return accumulator;
    else do
        return factorial_tail(n - 1, n * accumulator);
    end
end
```

## Function Pointers

Currently, Orbit does not support function pointers as first-class values, but this is planned for future versions.

## Main Function

The `main` function is the entry point of Orbit programs:

```orbit
fun main(): int do
    // Program logic
    return 0;  // Exit code
end
```

### Main Function Requirements

- Must be named `main`
- Should return an `int` (exit code)
- Takes no parameters (currently)

## Function Examples

### Mathematical Functions

```orbit
fun power(base: int, exponent: int): int do
    if (exponent == 0) do
        return 1;
    else do
        let result = 1;
        let i = 0;
        while (i < exponent) do
            result = result * base;
            i = i + 1;
        end
        return result;
    end
end
```

### Data Processing Functions

```orbit
fun find_max(numbers: vec(int)): int do
    let max_val = numbers[0];
    let i = 1;
    while (i < numbers.length()) do
        if (numbers[i] > max_val) do
            max_val = numbers[i];
        end
        i = i + 1;
    end
    return max_val;
end
```

### String Processing Functions

```orbit
fun string_length(str: [*]byte): int do
    let length = 0;
    while (str[length] != 0) do  // Null terminator
        length = length + 1;
    end
    return length;
end
```

## Function Compilation

### Monomorphization

Generic functions are monomorphized - a separate copy is generated for each set of type arguments used:

```orbit
fun identity(T: type, value: T): T do
    return value;
end

// Generates two separate functions:
let int_val = identity(int, 42);        // identity_int
let str_val = identity([*]byte, "hi");  // identity_ptr_byte
```

### Optimization

The compiler performs various optimizations:

- Function inlining for small functions
- Dead code elimination
- Constant folding
- Tail call optimization (planned)

## Function Calling Convention

### Stack Management

- Parameters are passed on the stack
- Local variables are allocated on the stack
- Return values are passed through registers or stack

### Memory Layout

```
Stack Layout (grows downward):
[Return Address]
[Previous Frame Pointer]
[Local Variables]
[Function Parameters]
```

## Best Practices

1. **Single Responsibility**: Each function should have a single, well-defined purpose
2. **Descriptive Names**: Use clear, descriptive function names
3. **Parameter Validation**: Validate parameters when necessary
4. **Return Value Consistency**: Always return a value from functions that declare a return type
5. **Avoid Deep Nesting**: Keep function complexity manageable
6. **Pure Functions**: Prefer functions without side effects when possible

## Function Limitations

Current limitations (may be addressed in future versions):

- No function overloading
- No default parameters
- No variadic functions
- No closures
- No anonymous functions
- No higher-order functions (functions as values)