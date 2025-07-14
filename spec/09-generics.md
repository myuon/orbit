# 9. Generics

## Generics Overview

Orbit's generic system allows for writing reusable code that works with multiple types while maintaining type safety and performance. Generics are implemented through monomorphization at compile time.

## Generic Type Parameters

### Type Parameter Declaration

Type parameters are declared using the `type` keyword:

```orbit
type Container(T: type) = struct {
    value: T
};
```

### Multiple Type Parameters

```orbit
type Pair(A: type, B: type) = struct {
    first: A,
    second: B
};

type Triple(A: type, B: type, C: type) = struct {
    first: A,
    second: B,
    third: C
};
```

## Generic Structs

### Basic Generic Struct

```orbit
type Box(T: type) = struct {
    contents: T,
    
    fun get_contents(self: Box(T)): T do
        return self.contents;
    end
    
    fun set_contents(self: Box(T), new_contents: T) do
        self.contents = new_contents;
        return;
    end
};
```

### Generic Struct Instantiation

```orbit
let int_box = new Box(int) { .contents = 42 };
let string_box = new Box([*]byte) { .contents = "hello" };
let bool_box = new Box(bool) { .contents = true };
```

### Generic Struct with Complex Types

```orbit
type Result(T: type, E: type) = struct {
    is_success: bool,
    value: T,
    error: E
};

let success_result = new Result(int, [*]byte) {
    .is_success = true,
    .value = 42,
    .error = ""
};

let error_result = new Result(int, [*]byte) {
    .is_success = false,
    .value = 0,
    .error = "computation failed"
};
```

## Generic Functions

### Basic Generic Function

```orbit
fun identity(T: type, value: T): T do
    return value;
end
```

### Generic Function with Multiple Parameters

```orbit
fun swap(A: type, B: type, a: A, b: B): Pair(B, A) do
    return new Pair(B, A) { .first = b, .second = a };
end
```

### Generic Function Calls

```orbit
let int_val = identity(int, 42);
let string_val = identity([*]byte, "hello");

let swapped = swap(int, [*]byte, 10, "world");
// swapped.first is "world", swapped.second is 10
```

## Generic Methods

### Methods in Generic Structs

```orbit
type Collection(T: type) = struct {
    items: vec(T),
    count: int,
    
    fun add(self: Collection(T), item: T) do
        self.items <- item;
        self.count = self.count + 1;
        return;
    end
    
    fun get(self: Collection(T), index: int): T do
        return self.items[index];
    end
    
    fun size(self: Collection(T)): int do
        return self.count;
    end
};
```

### Generic Method Usage

```orbit
let int_collection = new Collection(int) { 
    .items = new vec(int) {}, 
    .count = 0 
};

int_collection.add(10);
int_collection.add(20);
int_collection.add(30);

let first_item = int_collection.get(0);  // 10
let collection_size = int_collection.size();  // 3
```

## Type Constraints

Currently, Orbit does not support explicit type constraints, but this is planned for future versions.

### Future Constraint Syntax (Planned)

```orbit
// Planned syntax - not yet implemented
fun compare(T: type where T: Comparable, a: T, b: T): bool do
    return a.compare(b) < 0;
end
```

## Generic Type Inference

### Partial Type Inference

In some contexts, type parameters can be inferred:

```orbit
// Currently, all type parameters must be explicit
let result = identity(int, 42);  // Required
// let result = identity(42);    // Not yet supported
```

### Future Inference (Planned)

```orbit
// Planned syntax - type inference
let result = identity(42);        // T inferred as int
let pair = make_pair(10, "hi");   // A=int, B=[*]byte inferred
```

## Monomorphization

### Compile-Time Instantiation

Generic types and functions are monomorphized at compile time:

```orbit
fun process(T: type, value: T): T do
    return value;
end

// Usage creates separate compiled functions:
let int_result = process(int, 42);        // process_int
let str_result = process([*]byte, "hi");  // process_ptr_byte
```

### Performance Implications

- No runtime overhead for generics
- Each type instantiation creates a separate compiled function/type
- Binary size may increase with many instantiations
- Maximum performance through specialization

## Built-in Generic Types

### Vector

```orbit
let int_vec = new vec(int) {};
let string_vec = new vec([*]byte) {};

int_vec <- 1;
int_vec <- 2;
string_vec <- "hello";
string_vec <- "world";
```

### Map

```orbit
let string_to_int = new map([*]byte, int) {};
let int_to_bool = new map(int, bool) {};

string_to_int["key"] = 42;
int_to_bool[1] = true;
```

### Slice (Planned)

```orbit
// Planned syntax
let slice = numbers[0..5];  // slice(int)
```

## Complex Generic Examples

### Generic Stack

```orbit
type Stack(T: type) = struct {
    items: vec(T),
    
    fun push(self: Stack(T), item: T) do
        self.items <- item;
        return;
    end
    
    fun pop(self: Stack(T)): T do
        let index = self.items.length() - 1;
        let item = self.items[index];
        // Note: actual removal would require vec.remove()
        return item;
    end
    
    fun is_empty(self: Stack(T)): bool do
        return self.items.length() == 0;
    end
};
```

### Generic Binary Tree Node

```orbit
type TreeNode(T: type) = struct {
    value: T,
    left: [*]TreeNode(T),   // Pointer to left child
    right: [*]TreeNode(T),  // Pointer to right child
    
    fun is_leaf(self: TreeNode(T)): bool do
        return self.left == null && self.right == null;
    end
};
```

### Generic Function with Multiple Type Parameters

```orbit
fun combine(A: type, B: type, C: type, 
           f: fun(A, B): C, a: A, b: B): C do
    return f(a, b);
end

// Usage (when function pointers are supported):
// let result = combine(int, int, int, add, 10, 20);
```

## Generic Type Aliases

### Type Alias with Generics

```orbit
type StringMap(V: type) = map([*]byte, V);
type IntPair = Pair(int, int);

let user_scores = new StringMap(int) {};
let coordinates = new IntPair { .first = 10, .second = 20 };
```

## Advanced Generic Patterns

### Generic Container with Operations

```orbit
type Buffer(T: type) = struct {
    data: vec(T),
    capacity: int,
    
    fun create(T: type, cap: int): Buffer(T) do
        return new Buffer(T) {
            .data = new vec(T) {},
            .capacity = cap
        };
    end
    
    fun is_full(self: Buffer(T)): bool do
        return self.data.length() >= self.capacity;
    end
    
    fun add_if_space(self: Buffer(T), item: T): bool do
        if (!self.is_full()) do
            self.data <- item;
            return true;
        else do
            return false;
        end
    end
};
```

### Generic Utility Functions

```orbit
fun map_transform(A: type, B: type, 
                  items: vec(A), 
                  transform: fun(A): B): vec(B) do
    let result = new vec(B) {};
    let i = 0;
    while (i < items.length()) do
        let transformed = transform(items[i]);
        result <- transformed;
        i = i + 1;
    end
    return result;
end
```

## Generic Compilation Process

### Type Substitution

During compilation, type parameters are replaced with concrete types:

```orbit
// Generic definition
type Box(T: type) = struct { value: T };

// Instantiation
let int_box = new Box(int) { .value = 42 };

// Compiler generates:
// type Box_int = struct { value: int };
```

### Method Monomorphization

```orbit
// Generic method
fun Container(T: type).add(self: Container(T), item: T) do
    self.items <- item;
end

// Usage
let int_container = new Container(int) { .items = new vec(int) {} };
int_container.add(42);

// Compiler generates:
// fun Container_int_add(self: Container_int, item: int) do
//     self.items <- item;
// end
```

## Best Practices

1. **Meaningful Type Parameter Names**: Use descriptive names like `T`, `K`, `V`
2. **Minimal Type Parameters**: Use only as many type parameters as needed
3. **Generic When Beneficial**: Use generics for truly reusable code
4. **Document Type Requirements**: Clearly document what types are expected
5. **Consider Performance**: Be aware of monomorphization impact on binary size

## Current Limitations

- No type constraints/bounds
- No type inference for function calls
- No higher-kinded types
- No generic type aliases that introduce new parameters
- No associated types
- No default type parameters

## Future Generic Features

Planned enhancements:

- Type constraints and bounds
- Type inference for generic function calls
- Associated types for structs
- Default type parameters
- Higher-kinded types
- Generic closures
- Trait/interface system with generics