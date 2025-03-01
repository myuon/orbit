# Orbit Programming Language

## Features

- [x] Static typing
- [x] JIT compilation to AArch64
- [ ] Garbage collection
- [ ] Variable shadowing
- [ ] Operator overloading
- [ ] Macros
- [ ] Pattern matching
- [ ] Destructuring
- [ ] Closures

## Hello World

```
fn main() do
  return "Hello, World!";
end
```

## Variables

```
let x = 10;
```

## Functions

```
fn add(a, b) do
  return a + b;
end
```

## Conditionals

```
if (x > 10) do
  return "x is greater than 10";
else do
  return "x is less than or equal to 10";
end
```

## Loops

```
while (x < 10) do
  x = x + 1;
end
```

## Vectors

```
let vec = new vec(int) {};

vec[0] = 1; // update
vec[1] = 2;
vec[2] = 3;

vec <- 4; // push
```

## Structs

```
type Point = struct {
  x: int,
  y: int,
}

let p = new Point { .x = 10, .y = 20 };

p.x = 30; // update
```

## Methods

```
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

return p.sum(); // 30
```
