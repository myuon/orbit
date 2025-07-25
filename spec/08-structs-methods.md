# 8. Structs and Methods

## Struct Overview

Structs in Orbit are user-defined types that group related data together. They support both data fields and methods, enabling object-oriented programming patterns.

## Struct Declaration

### Basic Struct Syntax

```orbit
type StructName = struct {
    field1: Type1,
    field2: Type2,
    // ... more fields
};
```

### Simple Struct Example

```orbit
type Point = struct {
    x: int,
    y: int
};
```

### Struct with Multiple Field Types

```orbit
type Person = struct {
    name: [*]byte,
    age: int,
    is_active: bool
};
```

## Struct Instantiation

### Creating Struct Instances

#### Standard Instantiation

```orbit
let point = new Point { .x = 10, .y = 20 };
let person = new Person { 
    .name = "Alice", 
    .age = 30, 
    .is_active = true 
};
```

#### Pattern-Based Instantiation

The `new(struct)` pattern provides explicit struct construction:

```orbit
let point = new(struct) Point { .x = 10, .y = 20 };
let person = new(struct) Person { 
    .name = "Alice", 
    .age = 30, 
    .is_active = true 
};
```

Both syntaxes are functionally equivalent in the current implementation. The `new(struct)` pattern is designed to prepare for future operator overloading support where:

- `new TypeName { ... }` may invoke custom constructors
- `new(struct) TypeName { ... }` always performs direct struct field initialization

This distinction allows library authors to provide custom constructors while still offering access to raw struct construction when needed.

### Field Initialization

All fields must be initialized when creating a struct instance:

```orbit
type Rectangle = struct {
    width: int,
    height: int,
    color: [*]byte
};

let rect = new Rectangle {
    .width = 100,
    .height = 50,
    .color = "red"
};
```

## Field Access

### Reading Field Values

```orbit
let point = new Point { .x = 10, .y = 20 };
let x_coordinate = point.x;  // 10
let y_coordinate = point.y;  // 20
```

### Modifying Field Values

```orbit
let point = new Point { .x = 10, .y = 20 };
point.x = 15;  // Modify x field
point.y = 25;  // Modify y field
```

### Nested Field Access

```orbit
type Position = struct {
    x: int,
    y: int
};

type Player = struct {
    name: [*]byte,
    position: Position
};

let player = new Player {
    .name = "Hero",
    .position = new Position { .x = 100, .y = 200 }
};

let x_pos = player.position.x;  // Nested field access
player.position.y = 250;        // Nested field modification
```

## Methods

### Method Declaration

Methods are functions declared within a struct that operate on struct instances:

```orbit
type Point = struct {
    x: int,
    y: int,
    
    fun distance_from_origin(self: Point): int do
        return self.x * self.x + self.y * self.y;
    end
};
```

### Method Parameters

The first parameter of a method is typically `self` representing the struct instance:

```orbit
type Rectangle = struct {
    width: int,
    height: int,
    
    fun area(self: Rectangle): int do
        return self.width * self.height;
    end
    
    fun perimeter(self: Rectangle): int do
        return 2 * (self.width + self.height);
    end
};
```

### Method with Additional Parameters

```orbit
type Point = struct {
    x: int,
    y: int,
    
    fun move(self: Point, dx: int, dy: int) do
        self.x = self.x + dx;
        self.y = self.y + dy;
        return;
    end
};
```

## Method Calls

### Calling Methods

```orbit
let point = new Point { .x = 3, .y = 4 };
let distance = point.distance_from_origin();  // Method call

let rect = new Rectangle { .width = 10, .height = 5 };
let area = rect.area();        // 50
let perimeter = rect.perimeter(); // 30
```

### Method Calls with Parameters

```orbit
let point = new Point { .x = 10, .y = 20 };
point.move(5, -3);  // Move by (5, -3)
// point.x is now 15, point.y is now 17
```

## Generic Structs

### Generic Struct Declaration

```orbit
type Pair(A: type, B: type) = struct {
    first: A,
    second: B
};
```

### Generic Struct with Methods

```orbit
type Container(T: type) = struct {
    value: T,
    
    fun get_value(self: Container(T)): T do
        return self.value;
    end
    
    fun set_value(self: Container(T), new_value: T) do
        self.value = new_value;
        return;
    end
};
```

### Generic Struct Instantiation

```orbit
let int_pair = new Pair(int, int) { .first = 10, .second = 20 };
let string_int_pair = new Pair([*]byte, int) { 
    .first = "hello", 
    .second = 42 
};

let int_container = new Container(int) { .value = 100 };
let value = int_container.get_value();  // 100
int_container.set_value(200);
```

## Generic Methods

### Generic Method Declaration

```orbit
type Pair(A: type, B: type) = struct {
    first: A,
    second: B,
    
    fun swap(self: Pair(A, B)): Pair(B, A) do
        return new Pair(B, A) { 
            .first = self.second, 
            .second = self.first 
        };
    end
};
```

### Type Parameters in Methods

```orbit
type Collection(T: type) = struct {
    items: vec(T),
    
    fun add_item(self: Collection(T), item: T) do
        self.items <- item;
        return;
    end
    
    fun get_item(self: Collection(T), index: int): T do
        return self.items[index];
    end
};
```

## Struct Memory Layout

### Field Layout

Struct fields are laid out in memory in declaration order:

```orbit
type Example = struct {
    a: int,    // Offset 0, size 8
    b: bool,   // Offset 8, size 1
    c: int     // Offset 16, size 8 (aligned)
};
// Total size: 24 bytes (with padding)
```

### Struct Size

```orbit
let point_size = sizeof Point;     // Size of Point struct
let person_size = sizeof Person;   // Size of Person struct
```

## Struct Copying

### Value Semantics

Structs have value semantics - assignment creates a copy:

```orbit
let point1 = new Point { .x = 10, .y = 20 };
let point2 = point1;  // Creates a copy
point2.x = 30;        // Doesn't affect point1
```

### Struct Assignment

```orbit
let point1 = new Point { .x = 10, .y = 20 };
let point2 = new Point { .x = 0, .y = 0 };
point2 = point1;  // Copy point1 to point2
```

## Advanced Struct Features

### Struct with Complex Fields

```orbit
type GameState = struct {
    players: vec(Player),
    current_level: int,
    score: int,
    
    fun add_player(self: GameState, name: [*]byte) do
        let new_player = new Player {
            .name = name,
            .position = new Position { .x = 0, .y = 0 }
        };
        self.players <- new_player;
        return;
    end
};
```

### Struct Method Chaining

Currently, method chaining is not directly supported, but can be simulated by returning the struct:

```orbit
type Builder = struct {
    value: int,
    
    fun set_value(self: Builder, v: int): Builder do
        self.value = v;
        return self;
    end
    
    fun multiply(self: Builder, factor: int): Builder do
        self.value = self.value * factor;
        return self;
    end
};
```

## Struct Examples

### Mathematical Vector

```orbit
type Vector2D = struct {
    x: int,
    y: int,
    
    fun magnitude_squared(self: Vector2D): int do
        return self.x * self.x + self.y * self.y;
    end
    
    fun add(self: Vector2D, other: Vector2D): Vector2D do
        return new Vector2D {
            .x = self.x + other.x,
            .y = self.y + other.y
        };
    end
    
    fun scale(self: Vector2D, factor: int): Vector2D do
        return new Vector2D {
            .x = self.x * factor,
            .y = self.y * factor
        };
    end
};
```

### Data Container

```orbit
type Database = struct {
    users: map([*]byte, int),
    sessions: vec([*]byte),
    
    fun add_user(self: Database, username: [*]byte, id: int) do
        self.users[username] = id;
        return;
    end
    
    fun get_user_id(self: Database, username: [*]byte): int do
        return self.users[username];
    end
    
    fun create_session(self: Database, session_id: [*]byte) do
        self.sessions <- session_id;
        return;
    end
};
```

## Best Practices

1. **Encapsulation**: Use methods to manipulate struct data rather than direct field access
2. **Consistent Naming**: Use consistent naming conventions for fields and methods
3. **Single Responsibility**: Each struct should represent a single concept
4. **Immutability**: Consider making structs immutable when possible
5. **Documentation**: Document the purpose and usage of complex structs
6. **Field Order**: Order fields by importance or logical grouping

## Current Limitations

- No inheritance (struct extension is planned)
- No private fields (all fields are public)
- No static methods
- No custom constructors (beyond `new(struct)` pattern)
- No destructors
- No operator overloading for structs (though `new(struct)` prepares for this)

## Future Features

Planned features for future versions:

- **Constructor Overloading**: The `new(struct)` pattern prepares for future support where `new TypeName { ... }` can invoke custom constructors while `new(struct) TypeName { ... }` always performs direct field initialization
- Struct inheritance/extension
- Private fields and methods
- Static methods
- Operator overloading beyond constructors
- Trait/interface system

## Syntax Variants

### Current Struct Construction

Both syntaxes are currently supported and functionally equivalent:

```orbit
// Standard syntax
let obj1 = new MyStruct { .field1 = value1, .field2 = value2 };

// Pattern-based syntax (future-proof for constructor overloading)
let obj2 = new(struct) MyStruct { .field1 = value1, .field2 = value2 };
```

The `new(struct)` pattern is recommended in library code where future constructor customization may be desired, as it guarantees direct struct field initialization regardless of future language features.

**Important**: The `new(struct)` pattern applies only to user-defined struct types. Built-in types such as `vec(T)`, `map(K, V)`, and `pointer(T)` are not structs and should continue using the standard `new` syntax:

```orbit
// Correct: Use new(struct) for user-defined structs
let point = new(struct) Point { .x = 10, .y = 20 };

// Correct: Use new for built-in types (these are not structs)
let vec = new vec(int) {};
let map = new map(string, int) {};

// Correct: Use alloc for memory allocation
let ptr = alloc(10); // Allocates 10 elements

// Incorrect: Do not use new(struct) with built-in types or alloc
// let ptr = new(struct) pointer(int) {}; // This is wrong!
// let ptr = new pointer(int) {};         // This is not supported!
```