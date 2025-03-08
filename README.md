# Orbit Programming Language

Orbit is a statically typed programming language with a JIT compiler.

## Features

- [x] Static typing
- [x] JIT compilation for AArch64
- [x] Basic data structures (vectors, maps, slices)
- [x] Generic types
- [x] Method definitions
- [x] Global variables
- [ ] Garbage collection
- [ ] Variable shadowing
- [ ] Operator overloading
- [ ] Macros
- [ ] Pattern matching
- [ ] Destructuring
- [ ] Closures
- [ ] Error handling and Result type
- [ ] Modules

## Core Features

### Hello World

```orbit
fun main() do
  return "Hello, World!";
end
```

### Variables

```orbit
let x = 10;
let s = "Hello, World!";
```

### Functions

```orbit
fun add(a: int, b: int) do
  return a + b;
end
```

### Conditionals

```orbit
if (x > 10) do
  return "x is greater than 10";
else do
  return "x is less than or equal to 10";
end
```

### Loops

```orbit
while (x < 10) do
  x = x + 1;
end
```

### Vectors

```orbit
let vec = new vec(int) {};
vec <- 1;  // push
vec <- 2;
vec[0] = 10;  // update by index
```

### Maps

```orbit
let map = new map([*]byte, int) {};
map["hello"] = 1;
map["world"] = 2;
return map["hello"] + map["world"];  // 3
```

### Structs and Methods

```orbit
type Point = struct {
  x: int,
  y: int,

  fun sum(self: Point) do
    return self.x + self.y;
  end

  fun move(self: Point, dx: int, dy: int) do
    self.x = self.x + dx;
    self.y = self.y + dy;
  end
};

let p = new Point { .x = 10, .y = 20 };
p.move(5, 6);
return p.sum();  // 41
```

### Generic Types

```orbit
type Pair(A: type, B: type) = struct {
  first: A,
  second: B,

  fun set_first(self: Pair(A, B), a: A) do
    self.first = a;
    return 0;
  end
};

let p = new Pair(int, [*]byte) { .first = 1, .second = "hello" };
p.set_first(3);
```

### Global Variables

```orbit
let global = 100;

fun increment() do
  global = global + 50;
  return 0;
end

fun main() do
  let local = 200;
  increment();
  return global + local;  // 350
end
```

## Build and Run

```bash
zig build
```
