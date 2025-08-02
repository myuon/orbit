# 12. Standard Library

## Standard Library Overview

The Orbit standard library provides essential functionality through built-in functions and runtime services. Currently, the standard library is minimal but includes core operations for the built-in data structures and system interaction.

## Built-in Types and Operations

### Vector Operations

Vectors provide several built-in methods:

#### Length
```orbit
let vec = new vec(int) {};
vec <- 1;
vec <- 2;
vec <- 3;

let count = vec.length();  // Returns 3
```

#### Indexing
```orbit
let vec = new vec(int) {};
vec <- 10;
vec <- 20;
vec <- 30;

let first = vec[0];   // 10
let second = vec[1];  // 20
let third = vec[2];   // 30

// Assignment
vec[1] = 25;  // vec is now [10, 25, 30]
```

#### Push Operation
```orbit
let vec = new vec(int) {};
vec <- 42;     // Add element to end
vec <- 100;    // Add another element
```

### Map Operations

Maps support key-value operations:

#### Setting and Getting Values
```orbit
let map = new map([*]byte, int) {};
map["key1"] = 100;
map["key2"] = 200;

let value1 = map["key1"];  // 100
let value2 = map["key2"];  // 200
```

#### Key Existence (Planned)
```orbit
// Planned functionality - not yet implemented
let exists = map.contains("key1");  // true
let missing = map.contains("key3"); // false
```

### String Operations

Currently, strings are `[*]byte` and require manual operations:

#### Character Access
```orbit
let text = "Hello";
let first_char = text[0];  // 'H' as byte (72)
let second_char = text[1]; // 'e' as byte (101)
```

#### Custom String Functions
```orbit
fun string_length(str: [*]byte): int do
    let length = 0;
    while (str[length] != 0) do
        length = length + 1;
    end
    return length;
end

fun string_equal(a: [*]byte, b: [*]byte): bool do
    let i = 0;
    while (a[i] != 0 && b[i] != 0) do
        if (a[i] != b[i]) do
            return false;
        end
        i = i + 1;
    end
    return a[i] == b[i];
end
```

## Arithmetic Operations

### Basic Arithmetic
```orbit
let sum = 10 + 5;      // 15
let diff = 20 - 8;     // 12
let product = 4 * 6;   // 24
let remainder = 17 % 5; // 2
```

### Comparison Operations
```orbit
let equal = (10 == 10);      // true
let not_equal = (5 != 3);    // true
let less = (5 < 10);         // true
let greater = (15 > 10);     // true
let less_eq = (10 <= 10);    // true
let greater_eq = (20 >= 15); // true
```

## Type Operations

### Type Casting
```orbit
let x: int = 65;
let ch: byte = x as byte;  // Explicit cast to byte
```

### Size Operations
```orbit
let int_size = sizeof int;       // 8
let bool_size = sizeof bool;     // 1
let point_size = sizeof Point;   // Size of Point struct
```

## Memory Operations (Low-level)

### Memory Access
The runtime provides low-level memory operations for system programming:

```orbit
// These are internal runtime functions, not directly accessible:
// - loadMemory(size, address)
// - storeMemory(size, address, value)
// - setMemory(address, data)
```

## Utility Functions (User-Defined)

Since the standard library is minimal, common operations must be implemented by users:

### Mathematical Functions
```orbit
fun abs(x: int): int do
    if (x < 0) do
        return -x;
    else do
        return x;
    end
end

fun max(a: int, b: int): int do
    if (a > b) do
        return a;
    else do
        return b;
    end
end

fun min(a: int, b: int): int do
    if (a < b) do
        return a;
    else do
        return b;
    end
end

fun power(base: int, exponent: int): int do
    let result = 1;
    let i = 0;
    while (i < exponent) do
        result = result * base;
        i = i + 1;
    end
    return result;
end
```

### Array Utilities
```orbit
fun vec_contains(vec: vec(int), target: int): bool do
    let i = 0;
    while (i < vec.length()) do
        if (vec[i] == target) do
            return true;
        end
        i = i + 1;
    end
    return false;
end

fun vec_sum(vec: vec(int)): int do
    let total = 0;
    let i = 0;
    while (i < vec.length()) do
        total = total + vec[i];
        i = i + 1;
    end
    return total;
end

fun vec_max(vec: vec(int)): int do
    let max_val = vec[0];
    let i = 1;
    while (i < vec.length()) do
        if (vec[i] > max_val) do
            max_val = vec[i];
        end
        i = i + 1;
    end
    return max_val;
end
```

### String Utilities
```orbit
fun string_copy(dest: [*]byte, src: [*]byte, max_len: int) do
    let i = 0;
    while (i < max_len - 1 && src[i] != 0) do
        dest[i] = src[i];
        i = i + 1;
    end
    dest[i] = 0;  // Null terminator
    return;
end

fun string_compare(a: [*]byte, b: [*]byte): int do
    let i = 0;
    while (a[i] != 0 && b[i] != 0) do
        if (a[i] < b[i]) do
            return -1;
        else do
            if (a[i] > b[i]) do
                return 1;
            end
        end
        i = i + 1;
    end
    
    if (a[i] == 0 && b[i] == 0) do
        return 0;   // Equal
    else do
        if (a[i] == 0) do
            return -1;  // a is shorter
        else do
            return 1;   // b is shorter
        end
    end
end
```

## System Calls

Orbit provides low-level system interaction through syscalls. These are implemented at the runtime level and allow direct system operations.

### Write Syscall

The write syscall (syscall number 1) outputs data to a file descriptor:

```orbit
// syscall(1, fd, buffer, length) -> bytes_written
// Syscall function signature (conceptual):
// fun syscall(number: int, fd: int, buffer: [*]byte, length: int): int

// Example usage for writing to stdout (fd = 1):
let message = "Hello, World!";
let bytes_written = syscall(1, 1, message, 13);
```

#### Parameters
- `syscall_number`: Always 1 for write syscall
- `fd`: File descriptor (1 for stdout, 2 for stderr)
- `buffer`: Reference to string data to write
- `length`: Number of bytes to write

#### Return Value
Returns the actual number of bytes written, which may be less than requested if the buffer is shorter than the specified length.

#### Implementation Details
- The syscall validates that the file descriptor is a number
- The buffer must be a string reference (heap-allocated string)
- The actual write length is the minimum of the requested length and the string length
- Output is written to stdout and flushed immediately
- In test environments, output may be captured for verification

### Usage Examples

```orbit
// Write a simple message
let msg = "Hello";
let result = syscall(1, 1, msg, 5);  // Writes "Hello", returns 5

// Partial write (length > string length)
let short = "Hi";
let result = syscall(1, 1, short, 10);  // Writes "Hi", returns 2

// Write to stderr
let error_msg = "Error occurred";
let result = syscall(1, 2, error_msg, 13);  // Writes to stderr
```

### Error Conditions

The write syscall returns errors for:
- Invalid file descriptor (must be a number)
- Invalid buffer (must be a string reference)
- Invalid length (must be a number)
- Invalid heap references

### Future Syscalls

Additional syscalls are planned for future implementation:
- Read syscall (syscall number 0)
- File operations (open, close)
- Process management
- Memory operations

## I/O Operations (Future)

Currently, Orbit has limited I/O capabilities beyond syscalls. Planned I/O operations include:

### Console I/O (Planned)
```orbit
// Planned syntax - not yet implemented
fun print(message: [*]byte) do
    // Output to console
end

fun println(message: [*]byte) do
    // Output to console with newline
end

fun read_line(): [*]byte do
    // Read line from console
end
```

### File I/O (Planned)
```orbit
// Planned syntax - not yet implemented
type File = struct {
    // File handle implementation
};

fun open_file(path: [*]byte, mode: [*]byte): File do
    // Open file for reading/writing
end

fun read_file(file: File): [*]byte do
    // Read entire file content
end

fun write_file(file: File, content: [*]byte) do
    // Write content to file
end

fun close_file(file: File) do
    // Close file handle
end
```

## Error Handling (Future)

Error handling is planned for future versions:

### Result Type (Planned)
```orbit
// Planned syntax - not yet implemented
type Result(T: type, E: type) = struct {
    is_ok: bool,
    value: T,
    error: E
};

fun try_divide(a: int, b: int): Result(int, [*]byte) do
    if (b == 0) do
        return new Result(int, [*]byte) {
            .is_ok = false,
            .value = 0,
            .error = "Division by zero"
        };
    else do
        return new Result(int, [*]byte) {
            .is_ok = true,
            .value = a / b,
            .error = ""
        };
    end
end
```

## Runtime Services

### Memory Management
The runtime provides automatic memory management for built-in types:

- Vector memory allocation and growth
- Map hash table management
- String literal storage
- Garbage collection (planned)

### Type Information
Runtime type information is available for:

- Type sizes (`sizeof`)
- Type compatibility checking
- Generic type instantiation

### Debugging Support
The runtime provides debugging capabilities:

- Stack trace generation
- Memory inspection
- Instruction stepping
- Breakpoint support

## Performance Utilities

### Profiling (Built-in)
The compiler includes profiling support:

```orbit
// Profiling is automatic when enabled
// Output includes:
// - Function call frequencies
// - Memory allocation patterns  
// - Execution time analysis
```

### Optimization Hints
The compiler performs various optimizations:

- Function inlining
- Dead code elimination
- Constant folding
- Loop optimization

## Future Standard Library

Planned additions to the standard library:

### Collections
- `Set(T)` - Unordered unique elements
- `Deque(T)` - Double-ended queue
- `PriorityQueue(T)` - Heap-based priority queue
- `Tree(T)` - Balanced binary tree

### Algorithms
- Sorting algorithms (`sort`, `stable_sort`)
- Searching algorithms (`binary_search`, `find`)
- Functional operations (`map`, `filter`, `reduce`)
- Iterator interface

### System Programming
- File system operations
- Network programming
- Process management
- Threading and concurrency

### Mathematics
- Floating-point arithmetic
- Trigonometric functions
- Random number generation
- Statistical functions

### Text Processing
- Regular expressions
- String formatting
- Unicode support
- Text parsing utilities

## Best Practices

1. **Implement Common Utilities**: Create reusable utility functions for common operations
2. **Error Checking**: Validate inputs and handle edge cases
3. **Performance**: Consider performance implications of operations
4. **Documentation**: Document custom utility functions clearly
5. **Testing**: Test utility functions thoroughly
6. **Naming**: Use consistent naming conventions for library functions

## Current Limitations

- No standard I/O functions
- No file system operations
- No network programming support
- No threading or concurrency primitives
- No regular expressions
- No floating-point arithmetic
- Limited string manipulation functions

The standard library is expected to grow significantly as the language matures and more functionality is needed for practical applications.