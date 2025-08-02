# 4. Variables and Constants

## Variable Declarations

Variables in Orbit are declared using the `let` keyword. All variables must be initialized when declared.

### Basic Variable Declaration

```orbit
let x = 42;           // Type inferred as int
let name = "Alice";   // Type inferred as [*]byte
let isActive = true;  // Type inferred as bool
```

### Explicit Type Annotations

```orbit
let x: int = 42;
let name: [*]byte = "Alice";
let count: int = 0;
```

### Variable Initialization

All variables must be initialized at declaration:

```orbit
let x = 10;      // Valid
let y: int;      // Invalid: uninitialized variable
```

## Variable Assignment

After declaration, variables can be assigned new values:

```orbit
let x = 10;
x = 20;          // OK: x is now 20
x = x + 5;       // OK: x is now 25
```

## Global Variables

Global variables are declared at the module level and can be accessed from any function:

```orbit
let global_counter = 0;

fun increment() do
    global_counter = global_counter + 1;
    return 0;
end

fun main() do
    increment();
    return global_counter;  // Returns 1
end
```

### Global Variable Rules

1. Global variables must be initialized with a constant value
2. Global variables have module-wide scope
3. Global variables can be modified by any function
4. Global variables are initialized before program execution

## Scoping Rules

### Local Scope

Variables declared inside functions have local scope:

```orbit
fun example() do
    let x = 10;     // Local to example()
    return x;
end

fun main() do
    let x = 20;     // Different variable, local to main()
    return x;
end
```

### Block Scope

Variables declared in blocks are scoped to that block:

```orbit
fun example() do
    let x = 10;
    
    if (x > 5) do
        let y = 20;  // Local to if block
        return y;
    end
    
    // y is not accessible here
    return x;
end
```

### Function Parameter Scope

Function parameters are scoped to the function body:

```orbit
fun add(a: int, b: int): int do
    // a and b are accessible throughout the function
    return a + b;
end
```

## Variable Shadowing

**Note**: Variable shadowing is not currently implemented but is planned for future versions.

## Constants

Currently, Orbit does not have a separate constant declaration syntax. All variables declared with `let` are mutable after initialization.

### Future Constant Support

A `const` keyword may be added in future versions:

```orbit
// Planned syntax (not yet implemented)
const PI = 3.14159;
const MAX_SIZE = 1000;
```

## Memory Management

### Stack Allocation

Local variables are allocated on the stack:

```orbit
fun example() do
    let x = 42;     // Allocated on stack
    let arr = new vec(int) {};  // vec header on stack, data on heap
    return x;
end  // x is deallocated when function returns
```

### Heap Allocation

Dynamic data structures are allocated on the heap:

```orbit
let vec = new vec(int) {};    // Dynamic allocation
let map = new map([*]byte, int) {};  // Dynamic allocation
```

## Variable Lifetime

### Local Variables

Local variables have automatic storage duration - they are created when declared and destroyed when leaving scope:

```orbit
fun example() do
    let x = 10;     // x created here
    if (true) do
        let y = 20;  // y created here
        // y destroyed at end of if block
    end
    // x destroyed at end of function
end
```

### Global Variables

Global variables have static storage duration - they exist for the entire program execution:

```orbit
let global_var = 42;  // Exists for entire program

fun main() do
    global_var = 100;  // Modifies global state
    return global_var;
end
```

## Type Constraints

### Mutable vs Immutable

All variables are currently mutable after initialization:

```orbit
let x = 10;
x = 20;  // OK: variables are mutable
```

### Type Stability

Variable types cannot change after declaration:

```orbit
let x = 10;      // x is int
x = 20;          // OK: still int
x = "hello";     // Error: cannot assign string to int
```

## Best Practices

1. **Initialize Variables**: Always initialize variables at declaration
2. **Use Type Annotations**: When the type is not obvious from context
3. **Minimize Global State**: Use global variables sparingly
4. **Descriptive Names**: Use meaningful variable names
5. **Scope Appropriately**: Declare variables in the smallest scope needed

## Examples

### Local Variables

```orbit
fun calculate_area(width: int, height: int): int do
    let area = width * height;
    return area;
end
```

### Global State Management

```orbit
let game_score = 0;
let player_lives = 3;

fun update_score(points: int) do
    game_score = game_score + points;
    return 0;
end

fun lose_life() do
    if (player_lives > 0) do
        player_lives = player_lives - 1;
    end
    return player_lives;
end
```

### Complex Data Structures

```orbit
type Player = struct {
    name: [*]byte,
    score: int,
    level: int
};

fun create_player(name: [*]byte): Player do
    let player = new Player {
        .name = name,
        .score = 0,
        .level = 1
    };
    return player;
end
```