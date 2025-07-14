# 11. Memory Management

## Memory Management Overview

Orbit uses explicit memory management with automatic cleanup for built-in data structures. The language provides low-level control when needed while maintaining safety through its type system.

## Memory Layout

### Stack Memory

Local variables and function parameters are allocated on the stack:

```orbit
fun example() do
    let x = 42;        // Stack allocated
    let y = true;      // Stack allocated
    let z = x + 10;    // Stack allocated
    return z;
end  // All variables deallocated when function returns
```

### Heap Memory

Dynamic data structures are allocated on the heap:

```orbit
let vec = new vec(int) {};     // Vector header on stack, data on heap
let map = new map([*]byte, int) {};  // Map on heap
let point = new Point { .x = 10, .y = 20 };  // Struct on heap
```

### Global Memory

Global variables have static storage duration:

```orbit
let global_counter = 0;  // Allocated in global memory section
let global_data = new vec(int) {};  // Header in global, data on heap
```

## Memory Sections

The runtime organizes memory into distinct sections:

### Meta Section (0-24 bytes)

Contains runtime metadata:
- Global section pointer
- Data section pointer  
- Heap section pointer
- Current heap pointer

### Global Section

Contains global variable storage.

### Data Section

Contains constant data and string literals.

### Heap Section

Contains dynamically allocated objects with automatic management.

## Pointers

### Pointer Types

Pointers are represented using the `[*]T` syntax:

```orbit
let str: [*]byte = "Hello";        // Pointer to byte array
let int_ptr: [*]int = &some_int;   // Pointer to integer
```

### Pointer Operations

Currently limited pointer operations are supported:

```orbit
// Pointer assignment
let ptr1: [*]byte = "hello";
let ptr2: [*]byte = ptr1;

// Pointer dereferencing (through indexing)
let first_char = ptr1[0];  // Dereference and access first element
```

### String Pointers

String literals are automatically converted to `[*]byte`:

```orbit
let message: [*]byte = "Hello, World!";
let first_char = message[0];  // 'H' as byte
```

## Automatic Memory Management

### Built-in Data Structures

Vectors, maps, and other built-in types manage their own memory:

```orbit
fun create_vector(): vec(int) do
    let v = new vec(int) {};  // Heap allocation
    v <- 1;
    v <- 2;
    v <- 3;
    return v;  // Vector ownership transferred
end  // Original vector reference goes out of scope, but data persists
```

### Scope-Based Cleanup

Local variables are automatically cleaned up when leaving scope:

```orbit
fun example() do
    let vec = new vec(int) {};  // Allocation
    vec <- 42;
    
    if (true) do
        let temp = 100;  // Local allocation
        vec <- temp;
    end  // 'temp' cleaned up here
    
    return vec[1];
end  // 'vec' cleaned up here (vector data may be deallocated)
```

## Manual Memory Management

### Explicit Allocation

For advanced use cases, explicit memory allocation may be needed:

```orbit
// Future syntax - not yet fully implemented
let ptr = allocate(int, 10);  // Allocate array of 10 ints
// ... use ptr ...
deallocate(ptr);  // Manual deallocation
```

### Raw Pointers

Raw pointer operations for system programming:

```orbit
// Low-level pointer arithmetic (limited support)
let base_addr: [*]byte = get_memory_base();
let offset_addr = base_addr + 8;  // Move 8 bytes forward
```

## Memory Safety

### Type Safety

The type system prevents many memory errors:

```orbit
let int_var = 42;
let str_var = "hello";
// int_var = str_var;  // Compile-time error: type mismatch
```

### Bounds Checking

Array accesses are bounds-checked at runtime:

```orbit
let vec = new vec(int) {};
vec <- 10;
vec <- 20;

let valid = vec[0];    // OK: index 0 exists
let invalid = vec[5];  // Runtime error: index out of bounds
```

### Null Pointer Safety

Pointers are checked for validity:

```orbit
let ptr: [*]int = null;
// let value = ptr[0];  // Runtime error: null pointer dereference
```

## Memory Allocation Patterns

### Stack Allocation

Preferred for small, short-lived data:

```orbit
fun calculate(a: int, b: int): int do
    let temp = a * 2;     // Stack allocated
    let result = temp + b; // Stack allocated
    return result;
end
```

### Heap Allocation

Required for dynamic data and large objects:

```orbit
fun create_large_data(): vec(int) do
    let data = new vec(int) {};  // Heap allocated
    let i = 0;
    while (i < 10000) do
        data <- i;  // May trigger heap reallocations
        i = i + 1;
    end
    return data;
end
```

### Global Allocation

For application-wide state:

```orbit
let application_config = new map([*]byte, [*]byte) {};
let user_session_data = new vec(Session) {};

fun initialize_application() do
    application_config["version"] = "1.0.0";
    application_config["debug"] = "false";
    return;
end
```

## Memory Layout Examples

### Struct Memory Layout

```orbit
type Point = struct {
    x: int,    // Offset 0, 8 bytes
    y: int     // Offset 8, 8 bytes
};           // Total: 16 bytes

type Mixed = struct {
    flag: bool,    // Offset 0, 1 byte
    // 7 bytes padding
    value: int,    // Offset 8, 8 bytes
    code: byte     // Offset 16, 1 byte
    // 7 bytes padding for alignment
};               // Total: 24 bytes
```

### Vector Memory Layout

```orbit
// Vector structure (conceptual):
type vec(T) = struct {
    data: [*]T,      // Pointer to heap-allocated array
    length: int,     // Current number of elements
    capacity: int    // Allocated capacity
};

let v = new vec(int) {};
// v header is stack/global allocated
// v.data points to heap-allocated array
```

## Memory Debugging

### Memory Inspection

The debugger provides memory inspection capabilities:

```orbit
// In debug mode, memory can be inspected
// Debug output shows:
// - Heap layout
// - Stack frames
// - Memory usage statistics
```

### Memory Profiling

The profiler tracks memory allocation patterns:

```orbit
// Profiler output includes:
// - Allocation hotspots
// - Memory usage over time
// - Potential memory leaks
```

## Size Operations

### Type Sizes

Use `sizeof` to determine type sizes:

```orbit
let int_size = sizeof int;        // 8 bytes
let bool_size = sizeof bool;      // 1 byte
let ptr_size = sizeof [*]byte;    // 8 bytes (64-bit pointer)
let point_size = sizeof Point;    // 16 bytes
```

### Runtime Size Information

```orbit
let vec = new vec(int) {};
vec <- 1;
vec <- 2;
vec <- 3;

let element_count = vec.length();     // 3 elements
let memory_usage = element_count * sizeof int;  // 24 bytes for data
```

## Memory Performance Considerations

### Allocation Overhead

Consider allocation patterns for performance:

```orbit
// Efficient: single allocation
let vec = new vec(int) {};
let i = 0;
while (i < 1000) do
    vec <- i;  // Amortized O(1), occasional reallocation
    i = i + 1;
end

// Inefficient: many small allocations
let vecs = new vec(vec(int)) {};
let i = 0;
while (i < 1000) do
    let small_vec = new vec(int) {};  // 1000 separate allocations
    small_vec <- i;
    vecs <- small_vec;
    i = i + 1;
end
```

### Cache-Friendly Patterns

Structure data for cache efficiency:

```orbit
// Cache-friendly: contiguous data
type ParticlesSoA = struct {  // Structure of Arrays
    positions_x: vec(int),
    positions_y: vec(int),
    velocities_x: vec(int),
    velocities_y: vec(int)
};

// Less cache-friendly: scattered data
type Particle = struct {
    position_x: int,
    position_y: int,
    velocity_x: int,
    velocity_y: int
};
type ParticlesAoS = struct {   // Array of Structures
    particles: vec(Particle)
};
```

## Memory Best Practices

1. **Prefer Stack Allocation**: Use stack allocation for small, short-lived data
2. **Minimize Heap Allocations**: Reduce frequency of dynamic allocations
3. **Reuse Data Structures**: Reuse vectors and maps when possible
4. **Consider Data Layout**: Structure data for cache efficiency
5. **Profile Memory Usage**: Monitor allocation patterns in performance-critical code
6. **Avoid Memory Leaks**: Ensure proper cleanup of resources

## Current Limitations

- No garbage collector (planned for future)
- Limited pointer arithmetic
- No reference counting
- No weak references
- No custom allocators
- No RAII (Resource Acquisition Is Initialization) patterns

## Future Memory Features

Planned enhancements:

- **Garbage Collection**: Automatic memory management option
- **Reference Counting**: Smart pointer types
- **Custom Allocators**: User-defined allocation strategies
- **Memory Pools**: Specialized allocation for specific use cases
- **Weak References**: Non-owning references
- **RAII**: Automatic resource management
- **Memory-Mapped Files**: Efficient file I/O
- **Shared Memory**: Inter-process communication