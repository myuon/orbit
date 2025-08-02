# 5. Expressions

## Expression Overview

Expressions in Orbit are constructs that evaluate to a value. They can be composed using operators, function calls, and other expression forms.

## Literal Expressions

### Integer Literals

```orbit
42          // decimal integer
0xFF        // hexadecimal integer
```

### Boolean Literals

```orbit
true
false
```

### String Literals

```orbit
"Hello, World!"
"Orbit Programming Language"
```

## Variable Expressions

Variable names evaluate to the value stored in the variable:

```orbit
let x = 42;
let y = x;  // x is a variable expression
```

## Binary Expressions

### Arithmetic Operators

```orbit
let a = 10 + 5;     // Addition: 15
let b = 20 - 8;     // Subtraction: 12
let c = 4 * 6;      // Multiplication: 24
let d = 15 % 4;     // Modulo: 3
```

### Comparison Operators

```orbit
let equal = (10 == 10);        // true
let less = (5 < 10);           // true
let greater = (15 > 10);       // true
let lessEqual = (10 <= 10);    // true
let greaterEqual = (20 >= 15); // true
```

### Operator Precedence

From highest to lowest precedence:

1. Function calls, indexing, field access
2. Multiplication (`*`), Modulo (`%`)
3. Addition (`+`), Subtraction (`-`)
4. Comparison operators (`<`, `>`, `<=`, `>=`)
5. Equality operators (`==`)
6. Assignment (`=`)

```orbit
let result = 2 + 3 * 4;  // Evaluates to 14 (not 20)
let comp = 5 < 3 + 4;    // Evaluates to true (5 < 7)
```

## Function Call Expressions

```orbit
fun add(a: int, b: int): int do
    return a + b;
end

let result = add(10, 5);  // Function call expression
```

### Generic Function Calls

```orbit
fun get_first(A: type, B: type, a: A, b: B): A do
    return a;
end

let first = get_first(int, [*]byte, 42, "hello");
```

## Indexing Expressions

### Array/Vector Indexing

```orbit
let vec = new vec(int) {};
vec <- 10;
vec <- 20;
let first = vec[0];  // Indexing expression: 10
```

### String Indexing

```orbit
let str = "Hello";
let char = str[1];  // Indexing expression: 'e' (as byte)
```

### Map Indexing

```orbit
let map = new map([*]byte, int) {};
map["key"] = 42;
let value = map["key"];  // Indexing expression: 42
```

## Field Access Expressions

```orbit
type Point = struct {
    x: int,
    y: int
};

let p = new Point { .x = 10, .y = 20 };
let x_coord = p.x;  // Field access expression: 10
```

## Method Call Expressions

```orbit
type Point = struct {
    x: int,
    y: int,
    
    fun distance_from_origin(self: Point): int do
        return self.x * self.x + self.y * self.y;
    end
};

let p = new Point { .x = 3, .y = 4 };
let dist = p.distance_from_origin();  // Method call expression
```

## Object Construction Expressions

### Struct Construction

#### Standard Struct Construction

```orbit
let point = new Point { .x = 10, .y = 20 };
```

#### Pattern-Based Struct Construction

The `new(struct)` pattern provides an explicit way to construct structs that prepares for future operator overloading support:

```orbit
let point = new(struct) Point { .x = 10, .y = 20 };
```

This syntax is equivalent to the standard construction but makes the struct instantiation explicit. This pattern is designed to support future constructor overloading where `new T { ... }` may invoke custom constructors while `new(struct) T { ... }` always performs direct struct instantiation.

#### Example Usage in Methods

```orbit
type array(T: type) = struct {
    data: [*]T,
    length: int,

    fun _new(length: int): array(T) do
        return new(struct) array(T) { 
            .data = new pointer(T) {}, 
            .length = length 
        };
    end
};
```

### Generic Type Construction

```orbit
let pair = new Pair(int, [*]byte) {
    .first = 42,
    .second = "hello"
};

// Pattern-based construction also works with generics
let pair_explicit = new(struct) Pair(int, [*]byte) {
    .first = 42,
    .second = "hello"
};
```

### Built-in Type Construction

```orbit
let vec = new vec(int) {};
let map = new map([*]byte, int) {};
```

### Memory Allocation

For pointer allocation, use the `alloc` function:

```orbit
let ptr = alloc(10);  // Allocates 10 elements, returns [*]byte
```

The `alloc` function:
- Takes a size expression as argument
- Returns a pointer type `[*]byte` 
- Initializes all elements to zero
- Provides heap-allocated memory

**Note**: The `new(struct)` pattern applies only to user-defined struct types. Built-in types like `vec(T)` and `map(K, V)` use the standard `new` syntax. For memory allocation, use `alloc(size)` instead of `new pointer(T)` which is not supported.

## Type Cast Expressions

```orbit
let x: int = 65;
let ch: byte = x as byte;  // Type cast expression
```

## Type Expressions

In generic contexts, types themselves can be expressions:

```orbit
type Container(T: type) = struct {
    value: T
};

let int_container = new Container(int) { .value = 42 };
```

## Size Expressions

```orbit
let int_size = sizeof int;        // Size expression: 8
let point_size = sizeof Point;    // Size expression: 16
```

## Conditional Expressions

```orbit
let result = if (x > 0) do
    return "positive";
else do
    return "non-positive";
end
```

## Block Expressions

Blocks can be used as expressions:

```orbit
let result = do
    let temp = 42;
    return temp * 2;
end
```

## Expression Evaluation Rules

### Left-to-Right Evaluation

Binary operators are evaluated left-to-right for operators of the same precedence:

```orbit
let result = 10 - 5 - 2;  // Evaluates as (10 - 5) - 2 = 3
```

### Short-Circuit Evaluation

Currently not implemented, but planned for logical operators.

### Side Effects

Function calls may have side effects:

```orbit
fun increment_global(): int do
    global_counter = global_counter + 1;
    return global_counter;
end

let x = increment_global() + increment_global();  // Side effects occur
```

## Expression Context

### L-values vs R-values

L-values (can appear on left side of assignment):
- Variable names
- Field access (`obj.field`)
- Array indexing (`arr[i]`)

R-values (can appear on right side of assignment):
- All expressions
- Literal values
- Function calls

```orbit
let x = 10;       // x is l-value, 10 is r-value
x = 20;           // x is l-value, 20 is r-value
arr[0] = 42;      // arr[0] is l-value, 42 is r-value
```

## Expression Examples

### Complex Arithmetic

```orbit
let result = (a + b) * (c - d) / e;
```

### Nested Function Calls

```orbit
let result = max(min(a, b), min(c, d));
```

### Chained Field Access

```orbit
let distance = player.position.x * player.position.x + 
               player.position.y * player.position.y;
```

### Mixed Type Operations

```orbit
let result = vec.length() * 2 + base_offset;
```