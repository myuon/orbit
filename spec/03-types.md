# 3. Types

## Type System Overview

Orbit has a strong, static type system where all types are known at compile time. The type system ensures memory safety and enables aggressive optimizations through the compiler pipeline.

## Primitive Types

### Integer Type

The `int` type represents a 64-bit signed integer.

```orbit
let x: int = 42;
let y = 100;  // Type inferred as int
```

**Size**: 8 bytes
**Range**: -2^63 to 2^63-1

### Boolean Type

The `bool` type represents a boolean value.

```orbit
let isActive: bool = true;
let isComplete = false;  // Type inferred as bool
```

**Size**: 1 byte
**Values**: `true` or `false`

### Byte Type

The `byte` type represents a single 8-bit unsigned integer.

```orbit
let ch: byte = 65;  // ASCII 'A'
```

**Size**: 1 byte
**Range**: 0 to 255

## Pointer Types

### Pointer Type

Pointers are represented using the `[*]T` syntax where `T` is the pointed-to type.

```orbit
let str: [*]byte = "Hello";  // Pointer to byte array (string)
let ptr: [*]int = &someInt;  // Pointer to int
```

**Size**: 8 bytes (64-bit pointer)

String literals are automatically typed as `[*]byte`.

## Composite Types

### Struct Types

Structs are user-defined types that group related data together.

```orbit
type Point = struct {
    x: int,
    y: int
};
```

**Size**: Sum of all field sizes (8 bytes for Point)

### Function Types

Functions have types that specify their parameter types and return type.

```orbit
fun add(a: int, b: int): int do
    return a + b;
end
```

The type of `add` is `fun(int, int): int`.

## Generic Types

Orbit supports generic types that are parameterized by other types.

```orbit
type Pair(A: type, B: type) = struct {
    first: A,
    second: B
};
```

Generic types are instantiated with concrete types:

```orbit
let p: Pair(int, [*]byte) = new Pair(int, [*]byte) {
    .first = 42,
    .second = "hello"
};
```

## Built-in Generic Types

### Vector Type

`vec(T)` represents a dynamic array of elements of type `T`.

```orbit
let numbers: vec(int) = new vec(int) {};
```

### Map Type

`map(K, V)` represents a hash map with keys of type `K` and values of type `V`.

```orbit
let ages: map([*]byte, int) = new map([*]byte, int) {};
```

### Slice Type

`slice(T)` represents a view into an array or vector.

```orbit
let s: slice(int) = numbers[0..5];
```

## Type Inference

Orbit supports type inference for variable declarations using `let`:

```orbit
let x = 42;           // Inferred as int
let name = "Alice";   // Inferred as [*]byte
let isValid = true;   // Inferred as bool
```

## Type Annotations

Explicit type annotations can be provided:

```orbit
let x: int = 42;
let name: [*]byte = "Alice";
let point: Point = new Point { .x = 10, .y = 20 };
```

## Type Compatibility

### Assignment Compatibility

Values can be assigned to variables of compatible types:

```orbit
let x: int = 42;
let y: int = x;  // OK: same type
```

### Type Casting

Explicit type casting is supported using the `as` operator:

```orbit
let x: int = 42;
let b: byte = x as byte;  // Explicit cast
```

## Type Checking Rules

1. **Strong Typing**: No implicit type conversions except for literals
2. **Static Typing**: All types must be known at compile time
3. **Type Safety**: Operations must be defined for the given types
4. **Generic Monomorphization**: Generic types are instantiated for each concrete type used

## Special Types

### Unknown Type

The `unknown` type is used internally by the compiler during type inference but is not available to user code.

### Type Type

The `type` type represents type parameters in generic definitions:

```orbit
type Container(T: type) = struct {
    value: T
};
```

## Memory Layout

Types have well-defined memory layouts:

- `int`: 8 bytes, aligned to 8-byte boundary
- `bool`: 1 byte
- `byte`: 1 byte
- `[*]T`: 8 bytes (pointer size)
- Structs: Sum of field sizes with appropriate padding

## Type Size Operator

The `sizeof` operator returns the size of a type in bytes:

```orbit
let intSize = sizeof int;      // 8
let boolSize = sizeof bool;    // 1
let pointSize = sizeof Point;  // 16 (two ints)
```