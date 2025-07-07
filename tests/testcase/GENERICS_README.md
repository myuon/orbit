# Generics Test Cases

This directory contains test cases for the Orbit programming language generics implementation.

## Current Test Cases (Implemented)

These test cases verify that the generic syntax is correctly parsed by the compiler:

### ✅ `generic_struct_parsing.ob`
Tests parsing of generic struct declarations with type parameters:
- Single type parameter: `Container(T: type)`
- Multiple type parameters: `Pair(A: type, B: type)`

### ✅ `generic_function_parsing.ob`
Tests parsing of generic function declarations:
- Type parameters mixed with regular parameters
- Return type annotations: `: T`
- Function signature: `fun identity(T: type, value: T): T`

### ✅ `generic_type_instantiation_parsing.ob`
Tests parsing of generic type instantiations in type annotations:
- Simple instantiation: `Container(int)`
- Nested generics: `Container(Container([*]byte))`
- Complex combinations

## Future Test Cases (Not Yet Implemented)

These test cases use `.ob.future` extension and will work once full generics support is implemented:

### 🔮 `generic_struct_basic.ob.future`
Basic generic struct instantiation and usage:
- Creating instances: `new Container(int) { .value = 42 }`
- Accessing fields: `c.value`

### 🔮 `generic_pair.ob.future`
Complex generic struct with methods (from Zig reference tests):
- Multi-parameter generics: `Pair(int, [*]byte)`
- Generic methods: `set_first(self: Pair(A, B), a: A)`
- String indexing with type casting

### 🔮 `generic_function_calls.ob.future`
Generic function calls with type arguments:
- Type expressions: `type int`, `type [*]byte`
- Function calls: `get_first(type int, type [*]byte, 1, "Hello")`

### 🔮 `generic_methods.ob.future`
Generic struct methods:
- Methods on generic types: `Container(T).get()`
- Method calls with generic parameters

### 🔮 `generic_sizeof.ob.future`
Sizeof operator with generics:
- `sizeof T` within generic contexts
- Type parameter usage in sizeof expressions

### 🔮 `generic_associated_functions.ob.future`
Associated functions (static methods) on generic types:
- Constructor functions: `Container(T).new(value)`
- Type-specific methods: `Container(int).zero()`

### 🔮 `generic_vectors.ob.future`
Generic built-in types:
- Generic vectors: `vec(T)`
- Vector instantiation: `new vec(int) { 10, 20, 30 }`

### 🔮 `generic_nested.ob.future`
Nested generic type combinations:
- Complex nesting: `Box(Container(int))`
- Multiple levels of generic instantiation

## Implementation Status

| Feature | Parser | Type Checker | Monomorphization | VM/JIT |
|---------|--------|--------------|------------------|--------|
| Generic struct declaration | ✅ | ❌ | ❌ | ❌ |
| Generic function declaration | ✅ | ❌ | ❌ | ❌ |
| Generic type instantiation | ✅ | ❌ | ❌ | ❌ |
| Type expressions | ✅ | ❌ | ❌ | ❌ |
| Return type annotations | ✅ | ❌ | ❌ | ❌ |
| Generic struct instantiation | ❌ | ❌ | ❌ | ❌ |
| Generic function calls | ❌ | ❌ | ❌ | ❌ |
| Generic methods | ❌ | ❌ | ❌ | ❌ |
| Generic built-in types | ❌ | ❌ | ❌ | ❌ |
| Sizeof with generics | ❌ | ❌ | ❌ | ❌ |

## Running Tests

### Current Tests (Should Pass)
```bash
cargo run tests/testcase/generic_struct_parsing.ob
cargo run tests/testcase/generic_function_parsing.ob  
cargo run tests/testcase/generic_type_instantiation_parsing.ob
```

### Future Tests (Will Fail Until Implemented)
```bash
# These will fail until full generics implementation is complete
cargo run tests/testcase/generic_struct_basic.ob.future
cargo run tests/testcase/generic_pair.ob.future
# ... etc
```

### Integration Tests
```bash
cargo test test_program_files
```

## Expected Results

All current test cases should return their expected values as specified in the corresponding `.result` files:

- `generic_struct_parsing.ob` → `0`
- `generic_function_parsing.ob` → `42`  
- `generic_type_instantiation_parsing.ob` → `1337`

The future test cases have their expected results documented but will not work until the full generics implementation is complete.

## Next Steps

1. **Phase 3**: Type checker extensions for generic type validation
2. **Phase 4**: Monomorphization module for compile-time instantiation  
3. **Phase 5**: VM/JIT support for monomorphized code
4. **Phase 6**: Convert `.ob.future` files to working `.ob` test cases