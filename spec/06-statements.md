# 6. Statements

## Statement Overview

Statements in Orbit are constructs that perform actions but do not return values. They control the flow of execution and manage program state.

## Expression Statements

Any expression can be used as a statement by terminating it with a semicolon:

```orbit
42;                    // Expression statement (though not useful)
add(10, 20);          // Function call statement
vec <- 42;            // Push operation statement
```

## Variable Declaration Statements

### Let Statements

```orbit
let x = 42;
let name = "Alice";
let point = new Point { .x = 10, .y = 20 };
```

All variable declarations are statements and must end with a semicolon.

## Assignment Statements

### Simple Assignment

```orbit
x = 42;
name = "Bob";
point.x = 15;
```

### Indexed Assignment

```orbit
vec[0] = 100;
map["key"] = 42;
str[5] = 'X' as byte;
```

### Push Assignment

The `<-` operator is used to push elements to vectors:

```orbit
vec <- 42;        // Push 42 to vector
vec <- x + y;     // Push expression result to vector
```

## Return Statements

Return statements exit functions and optionally return a value:

```orbit
fun add(a: int, b: int): int do
    return a + b;     // Return with value
end

fun print_hello() do
    return;           // Return without value (implicit)
end
```

### Return Value Requirements

- Functions with a return type must return a value
- Functions without a return type (`void`) should not return a value
- Missing return statements in non-void functions result in compilation errors

## Control Flow Statements

### If Statements

```orbit
if (condition) do
    // statements
end

if (x > 0) do
    positive_count = positive_count + 1;
end
```

### If-Else Statements

```orbit
if (condition) do
    // statements for true case
else do
    // statements for false case
end

if (x > 0) do
    result = "positive";
else do
    result = "non-positive";
end
```

### Nested If Statements

```orbit
if (x > 0) do
    if (x > 100) do
        category = "large";
    else do
        category = "small";
    end
else do
    category = "negative";
end
```

### While Loops

```orbit
while (condition) do
    // loop body
end

let i = 0;
while (i < 10) do
    sum = sum + i;
    i = i + 1;
end
```

### Nested Loops

```orbit
let i = 0;
while (i < 3) do
    let j = 0;
    while (j < 3) do
        matrix[i][j] = i * j;
        j = j + 1;
    end
    i = i + 1;
end
```

## Block Statements

Blocks group multiple statements together:

```orbit
do
    let temp = x;
    x = y;
    y = temp;
end
```

Blocks create their own scope for variable declarations.

## Statement Termination

### Semicolon Rules

Most statements must be terminated with a semicolon:

```orbit
let x = 42;           // Required semicolon
x = x + 1;            // Required semicolon
vec <- 42;            // Required semicolon
```

### Block Statement Termination

Control flow statements with blocks do not require semicolons:

```orbit
if (condition) do
    // statements
end                   // No semicolon needed

while (condition) do
    // statements
end                   // No semicolon needed
```

## Statement Examples

### Simple Sequential Statements

```orbit
let count = 0;
let sum = 0;
let average = 0;

count = 10;
sum = 55;
average = sum / count;
```

### Control Flow Example

```orbit
let numbers = new vec(int) {};
let i = 0;

while (i < 10) do
    numbers <- i * i;
    i = i + 1;
end

let target = 25;
let found = false;
let index = 0;

while (index < numbers.length() && !found) do
    if (numbers[index] == target) do
        found = true;
    else do
        index = index + 1;
    end
end
```

### Data Structure Manipulation

```orbit
let scores = new map([*]byte, int) {};
let players = new vec([*]byte) {};

players <- "Alice";
players <- "Bob";
players <- "Charlie";

scores["Alice"] = 100;
scores["Bob"] = 85;
scores["Charlie"] = 92;

let total = 0;
let i = 0;
while (i < players.length()) do
    total = total + scores[players[i]];
    i = i + 1;
end
```

## Statement Nesting Rules

### Scope Nesting

Statements can be nested within blocks, creating nested scopes:

```orbit
let x = 10;           // Outer scope
if (x > 5) do
    let y = 20;       // Inner scope
    x = x + y;        // Can access outer scope
end
// y is not accessible here
```

### Control Flow Nesting

Control flow statements can be nested arbitrarily:

```orbit
while (running) do
    if (user_input == "quit") do
        running = false;
    else do
        if (user_input == "save") do
            save_data();
        else do
            process_input(user_input);
        end
    end
end
```

## Statement Execution Order

Statements are executed sequentially in the order they appear:

```orbit
let x = 1;            // Executed first
let y = 2;            // Executed second
let z = x + y;        // Executed third, z = 3
```

## Side Effects in Statements

Statements can have side effects through:

### Global Variable Modification

```orbit
let global_counter = 0;

fun increment() do
    global_counter = global_counter + 1;  // Side effect
    return 0;
end
```

### Data Structure Modification

```orbit
let shared_vec = new vec(int) {};

fun add_element(value: int) do
    shared_vec <- value;  // Side effect
    return 0;
end
```

### Function Calls with Side Effects

```orbit
print_message("Hello");    // Side effect: output
log_event("user_login");   // Side effect: logging
```

## Error Handling

Currently, Orbit does not have built-in error handling statements (try/catch). Error handling is planned for future versions.

## Future Statement Types

Planned for future versions:

- `break` and `continue` statements for loop control
- `match` statements for pattern matching
- `try`/`catch` statements for error handling
- `defer` statements for cleanup actions