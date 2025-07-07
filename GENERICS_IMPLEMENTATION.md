# Generics Implementation Task Breakdown

## Overview

This document outlines the comprehensive implementation plan for generics support in the Orbit programming language. The implementation requires monomorphization (compile-time type instantiation) and spans across the entire compiler pipeline.

## Implementation Phases

### Phase 1: AST Foundation (Est. 2-3 days)

#### 1.1 Extend AST Types
- [ ] Add `type_params: Vec<String>` to `StructDecl`
- [ ] Add `type_params: Vec<String>` to `Function`
- [ ] Add `Generic { name: String, args: Vec<Type> }` to `Type` enum
- [ ] Add `TypeParameter(String)` to `Type` enum
- [ ] Update `TypeExpr` to support generic type expressions

#### 1.2 Update AST Helper Functions
- [ ] Update `Type` display/debug implementations
- [ ] Update AST traversal functions for generic nodes
- [ ] Add generic type equality comparison

### Phase 2: Lexer and Parser (Est. 2-3 days)

#### 2.1 Lexer Extensions
- [ ] Ensure `type` keyword is properly tokenized
- [ ] Add support for parsing `(T: type)` syntax
- [ ] Handle angle bracket syntax if needed

#### 2.2 Parser Extensions
- [ ] Parse type parameters in struct declarations: `type Container(T: type) = struct { ... }`
- [ ] Parse type parameters in function declarations: `fun identity(T: type, value: T): T`
- [ ] Parse generic type instantiations: `Container(int)`
- [ ] Parse `type T` expressions in function calls
- [ ] Add error handling for malformed generic syntax

### Phase 3: Type System (Est. 3-4 days)

#### 3.1 Type Checking Extensions
- [ ] Implement type parameter binding during type checking
- [ ] Add generic type substitution algorithm
- [ ] Validate generic constraints and bounds
- [ ] Handle type parameter scoping rules

#### 3.2 Type Inference
- [ ] Basic type inference for generic function calls
- [ ] Type parameter unification
- [ ] Error reporting for unresolved type parameters

### Phase 4: Monomorphization Module (Est. 4-5 days)

#### 4.1 Core Monomorphization Engine
- [ ] Create `src/monomorphization.rs` module
- [ ] Implement `MonomorphizationTarget` structure
- [ ] Implement target collection algorithm
- [ ] Implement type substitution engine

#### 4.2 Symbol Generation
- [ ] Generate unique names for monomorphized functions
- [ ] Generate unique names for monomorphized types
- [ ] Handle method name generation for generic types
- [ ] Prevent name collisions

#### 4.3 Dependency Resolution
- [ ] Collect transitive generic dependencies
- [ ] Handle recursive generic instantiations
- [ ] Detect infinite recursion in generics

### Phase 5: Compiler Integration (Est. 2-3 days)

#### 5.1 Pipeline Integration
- [ ] Integrate monomorphization phase into compiler pipeline
- [ ] Ensure proper ordering: typecheck → desugar → monomorphize → compile
- [ ] Update `compiler.rs` to handle monomorphization

#### 5.2 Error Handling
- [ ] Propagate monomorphization errors
- [ ] Provide meaningful error messages for generic failures
- [ ] Handle compilation failures gracefully

### Phase 6: Built-in Generic Types (Est. 2-3 days)

#### 6.1 Vector Generics
- [ ] Convert `vec` to `vec(T)` generic type
- [ ] Update vector operations to be type-aware
- [ ] Test vector monomorphization

#### 6.2 Map Generics
- [ ] Convert `map` to `map(K, V)` generic type
- [ ] Update map operations to be type-aware
- [ ] Test map monomorphization

### Phase 7: Testing and Validation (Est. 2-3 days)

#### 7.1 Unit Tests
- [ ] Test AST generic node creation
- [ ] Test parser generic syntax handling
- [ ] Test type checker generic validation
- [ ] Test monomorphization target collection
- [ ] Test type substitution correctness

#### 7.2 Integration Tests
- [ ] Port `generics.ob` test case
- [ ] Port `generics_function.ob` test case
- [ ] Port `generics_method.ob` test case
- [ ] Port `sizeof_generic.ob` test case
- [ ] Port `associated_function_generics.ob` test case

#### 7.3 Edge Cases
- [ ] Test nested generics: `vec(Container(int))`
- [ ] Test multiple type parameters
- [ ] Test recursive generic structures
- [ ] Test generic method calls
- [ ] Test error conditions

## Reference Implementation

The Zig reference implementation provides complete guidance:
- **AST**: `zig_refs/src/ast.zig` - Generic AST structures
- **Parser**: `zig_refs/src/parser.zig` - Generic syntax parsing
- **Type Checker**: `zig_refs/src/typecheck.zig` - Generic type checking
- **Monomorphization**: `zig_refs/src/monomorphization.zig` - Complete monomorphization

## Key Technical Decisions

### 1. Monomorphization Strategy
- **Compile-time only**: No runtime generics
- **Eager instantiation**: Generate all needed instantiations upfront
- **Name mangling**: Use descriptive names for debugging

### 2. Type Parameter Syntax
- **Declaration**: `(T: type)` syntax
- **Instantiation**: `Container(int)` syntax
- **Constraints**: Future extension for bounds

### 3. Error Handling
- **Early validation**: Catch generic errors during type checking
- **Descriptive messages**: Clear error messages for generic failures
- **Graceful degradation**: Continue compilation when possible

## Testing Strategy

### Unit Testing
```rust
#[test]
fn test_generic_struct_monomorphization() {
    let test_cases = vec![
        ("Container(int)", "Container_int"),
        ("Container([*]byte)", "Container_ptr_byte"),
        ("Pair(int, float)", "Pair_int_float"),
    ];
    
    for (input, expected) in test_cases {
        let result = monomorphize_type_name(input);
        assert_eq!(result, expected);
    }
}
```

### Integration Testing
- Use existing `.ob` test files from `zig_refs/test/`
- Verify output matches expected `.stdout` files
- Test compilation and execution of generic programs

## Performance Considerations

### Compilation Time
- Minimize redundant monomorphization
- Cache monomorphized results
- Optimize type substitution algorithms

### Runtime Performance
- Monomorphized code should perform identically to hand-written concrete code
- No runtime overhead for generic abstractions
- Efficient memory layout for generic types

## Rollout Plan

### Phase 1-2: Foundation (Week 1)
- AST and Parser extensions
- Basic generic syntax support

### Phase 3-4: Core Logic (Week 2)
- Type checking and monomorphization
- Core generic functionality

### Phase 5-6: Integration (Week 3)
- Compiler integration
- Built-in generic types

### Phase 7: Testing (Week 4)
- Comprehensive testing
- Bug fixes and optimization

## Success Criteria

1. **All reference tests pass**: Every `.ob` test file from `zig_refs/test/` that uses generics
2. **Performance parity**: Monomorphized code performs as well as hand-written concrete code
3. **Error handling**: Clear, helpful error messages for generic-related compilation failures
4. **Code quality**: Clean, maintainable implementation following Rust best practices

## Next Steps

1. Begin Phase 1 with AST extensions
2. Implement each phase in order
3. Test thoroughly at each phase
4. Refer to Zig reference implementation for guidance
5. Maintain compatibility with existing non-generic code

---

**Estimated Total Implementation Time**: 3-4 weeks
**Current Status**: Planning phase complete, ready to begin implementation