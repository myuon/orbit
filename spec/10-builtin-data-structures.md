# 10. Built-in Data Structures

## Overview

Orbit provides several built-in generic data structures that are fundamental to most programs. These data structures are implemented efficiently and integrate seamlessly with the type system.

## Vectors

### Vector Type

`vec(T)` is a dynamic array that can grow and shrink at runtime.

```orbit
let numbers = new vec(int) {};
let names = new vec([*]byte) {};
```

### Vector Operations

#### Adding Elements

Use the push operator `<-` to add elements:

```orbit
let vec = new vec(int) {};
vec <- 42;
vec <- 100;
vec <- 7;
```

#### Accessing Elements

Access elements by index using square brackets:

```orbit
let first = vec[0];   // 42
let second = vec[1];  // 100
let third = vec[2];   // 7
```

#### Modifying Elements

```orbit
vec[0] = 50;  // Change first element to 50
vec[1] = vec[1] + 10;  // Increment second element
```

#### Vector Length

```orbit
let count = vec.length();  // Get number of elements
```

### Vector Examples

#### Building a Vector

```orbit
let squares = new vec(int) {};
let i = 1;
while (i <= 10) do
    squares <- i * i;
    i = i + 1;
end
// squares now contains [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

#### Vector Processing

```orbit
let numbers = new vec(int) {};
numbers <- 10;
numbers <- 20;
numbers <- 30;

let sum = 0;
let i = 0;
while (i < numbers.length()) do
    sum = sum + numbers[i];
    i = i + 1;
end
// sum is 60
```

#### Vector of Strings

```orbit
let words = new vec([*]byte) {};
words <- "hello";
words <- "world";
words <- "orbit";

let first_word = words[0];  // "hello"
```

## Maps

### Map Type

`map(K, V)` is a hash map that associates keys of type `K` with values of type `V`.

```orbit
let ages = new map([*]byte, int) {};
let scores = new map(int, [*]byte) {};
```

### Map Operations

#### Setting Values

```orbit
let ages = new map([*]byte, int) {};
ages["Alice"] = 30;
ages["Bob"] = 25;
ages["Charlie"] = 35;
```

#### Getting Values

```orbit
let alice_age = ages["Alice"];  // 30
let bob_age = ages["Bob"];      // 25
```

#### Updating Values

```orbit
ages["Alice"] = 31;  // Update Alice's age
ages["David"] = 28;  // Add new entry
```

### Map Examples

#### String to Integer Mapping

```orbit
let word_counts = new map([*]byte, int) {};
word_counts["hello"] = 5;
word_counts["world"] = 3;
word_counts["orbit"] = 2;

let hello_count = word_counts["hello"];  // 5
```

#### Integer to String Mapping

```orbit
let grade_letters = new map(int, [*]byte) {};
grade_letters[90] = "A";
grade_letters[80] = "B";
grade_letters[70] = "C";
grade_letters[60] = "D";

let excellent = grade_letters[90];  // "A"
```

#### Complex Value Types

```orbit
type Student = struct {
    name: [*]byte,
    gpa: int  // Scaled by 100, so 350 = 3.50 GPA
};

let students = new map(int, Student) {};
students[12345] = new Student { .name = "Alice", .gpa = 385 };
students[12346] = new Student { .name = "Bob", .gpa = 372 };

let alice = students[12345];
let alice_gpa = alice.gpa;  // 385
```

## Slices

### Slice Overview

Slices provide a view into a contiguous sequence of elements, such as part of a vector or array. Slices are planned for future implementation.

### Planned Slice Syntax

```orbit
// Planned syntax - not yet implemented
let numbers = new vec(int) {};
// ... populate numbers ...

let first_half = numbers[0..5];    // slice of first 5 elements
let second_half = numbers[5..];    // slice from index 5 to end
let middle = numbers[2..7];        // slice from index 2 to 6
```

## String Handling

### Strings as Byte Arrays

Strings in Orbit are represented as `[*]byte` (pointer to byte array):

```orbit
let message = "Hello, World!";
let first_char = message[0];  // 'H' as byte (72)
let length = string_length(message);  // Custom function needed
```

### String Operations

#### Character Access

```orbit
let text = "Orbit";
let o = text[0];  // 79 (ASCII for 'O')
let r = text[1];  // 114 (ASCII for 'r')
```

#### String Length Function

```orbit
fun string_length(str: [*]byte): int do
    let length = 0;
    while (str[length] != 0) do  // Null terminator
        length = length + 1;
    end
    return length;
end
```

#### String Comparison Function

```orbit
fun string_equal(str1: [*]byte, str2: [*]byte): bool do
    let i = 0;
    while (str1[i] != 0 && str2[i] != 0) do
        if (str1[i] != str2[i]) do
            return false;
        end
        i = i + 1;
    end
    return str1[i] == str2[i];  // Both should be null terminators
end
```

## Arrays (Fixed-Size)

### Array Overview

Fixed-size arrays are planned for future implementation as a complement to dynamic vectors.

### Planned Array Syntax

```orbit
// Planned syntax - not yet implemented
let fixed_array: [10]int = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
let matrix: [3][3]int = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
```

## Data Structure Combinations

### Vector of Maps

```orbit
let player_inventories = new vec(map([*]byte, int)) {};

let alice_inventory = new map([*]byte, int) {};
alice_inventory["sword"] = 1;
alice_inventory["potion"] = 5;
player_inventories <- alice_inventory;

let bob_inventory = new map([*]byte, int) {};
bob_inventory["bow"] = 1;
bob_inventory["arrow"] = 50;
player_inventories <- bob_inventory;

let alice_potions = player_inventories[0]["potion"];  // 5
```

### Map of Vectors

```orbit
let category_items = new map([*]byte, vec([*]byte)) {};

let fruits = new vec([*]byte) {};
fruits <- "apple";
fruits <- "banana";
fruits <- "orange";
category_items["fruits"] = fruits;

let vegetables = new vec([*]byte) {};
vegetables <- "carrot";
vegetables <- "broccoli";
category_items["vegetables"] = vegetables;

let first_fruit = category_items["fruits"][0];  // "apple"
```

## Memory Management

### Automatic Memory Management

Built-in data structures handle their own memory management:

```orbit
let vec = new vec(int) {};
// Memory for vector capacity is automatically managed
vec <- 1;  // May trigger reallocation internally
vec <- 2;
vec <- 3;
// Memory is freed when vec goes out of scope
```

### Growth Strategies

Vectors automatically grow their capacity when needed:

```orbit
let vec = new vec(int) {};  // Initial capacity (implementation-defined)
// Adding elements may cause internal reallocations
let i = 0;
while (i < 1000) do
    vec <- i;  // Efficient amortized O(1) insertion
    i = i + 1;
end
```

## Performance Characteristics

### Vector Performance

- **Access**: O(1) - direct indexing
- **Append**: O(1) amortized - may require reallocation
- **Insert/Remove**: O(n) - requires shifting elements
- **Memory**: Contiguous allocation, cache-friendly

### Map Performance

- **Access**: O(1) average - hash table lookup
- **Insert**: O(1) average - hash table insertion
- **Remove**: O(1) average - hash table removal
- **Memory**: Hash table with separate chaining or open addressing

## Usage Patterns

### Data Processing Pipeline

```orbit
fun process_numbers(input: vec(int)): vec(int) do
    let results = new vec(int) {};
    let i = 0;
    while (i < input.length()) do
        let value = input[i];
        if (value > 0) do  // Filter positive numbers
            results <- value * 2;  // Transform: multiply by 2
        end
        i = i + 1;
    end
    return results;
end
```

### Caching with Maps

```orbit
let cache = new map(int, int) {};

fun expensive_computation(x: int): int do
    // Check cache first
    if (cache_contains(cache, x)) do  // Hypothetical function
        return cache[x];
    end
    
    // Compute result
    let result = x * x * x;  // Expensive operation
    
    // Store in cache
    cache[x] = result;
    
    return result;
end
```

### Building Complex Data Structures

```orbit
type Graph = struct {
    adjacency: map(int, vec(int)),
    
    fun add_edge(self: Graph, from: int, to: int) do
        if (!map_contains(self.adjacency, from)) do  // Hypothetical function
            self.adjacency[from] = new vec(int) {};
        end
        self.adjacency[from] <- to;
        return;
    end
    
    fun get_neighbors(self: Graph, node: int): vec(int) do
        return self.adjacency[node];
    end
};
```

## Best Practices

1. **Choose Appropriate Data Structure**: Use vectors for ordered data, maps for key-value associations
2. **Preallocate When Possible**: If size is known, consider preallocation for vectors
3. **Batch Operations**: Group multiple insertions together when possible
4. **Avoid Frequent Reallocations**: Plan vector growth patterns
5. **Use Meaningful Keys**: Choose appropriate key types for maps
6. **Consider Memory Usage**: Be aware of memory overhead for small collections

## Future Enhancements

Planned improvements to built-in data structures:

- **Slices**: Views into arrays and vectors
- **Fixed Arrays**: Stack-allocated arrays with compile-time size
- **Sets**: Unordered collections of unique elements
- **Deques**: Double-ended queues
- **Priority Queues**: Heap-based priority queues
- **Iterators**: Unified iteration interface
- **Collection Methods**: Built-in map, filter, reduce operations