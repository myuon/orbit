# 14. Virtual Machine Specification

## Overview

The Orbit compiler uses a stack-based virtual machine (VM) for efficient code execution. This document describes the VM architecture, instruction set, and calling conventions.

## VM Architecture

### Stack-Based Design

The Orbit VM uses a stack-based architecture with the following components:

- **Evaluation Stack**: Used for expression evaluation and temporary values
- **Call Stack**: Manages function calls and local variables
- **Program Counter (PC)**: Points to the current instruction
- **Base Pointer (BP)**: Points to the current function's stack frame

### Memory Layout

```
Stack Memory Layout:
┌─────────────────┐ ← Stack Top
│   Temp Values   │ (evaluation stack)
├─────────────────┤
│   Local Vars    │ (current function)
├─────────────────┤ ← Base Pointer (BP)
│   Return Info   │ (return address, old BP)
├─────────────────┤
│   Arguments     │ (function parameters)
├─────────────────┤
│   Prev Frame    │ (previous function)
└─────────────────┘ ← Stack Bottom
```

## Instruction Set

### Basic Stack Operations

- `Push(value)` - Push immediate value onto stack
- `Pop` - Remove top value from stack

### Arithmetic Operations

- `Add` - Pop two values, push sum
- `Sub` - Pop two values, push difference (second - first)
- `Mul` - Pop two values, push product
- `Div` - Pop two values, push quotient (second / first)
- `Mod` - Pop two values, push remainder (second % first)

### Comparison Operations

- `Eq` - Pop two values, push 1 if equal, 0 otherwise
- `Lt` - Pop two values, push 1 if second < first, 0 otherwise
- `Lte` - Pop two values, push 1 if second <= first, 0 otherwise
- `Gt` - Pop two values, push 1 if second > first, 0 otherwise
- `Gte` - Pop two values, push 1 if second >= first, 0 otherwise

### Control Flow

- `Jump(addr)` - Unconditional jump to address
- `JumpIfZero(addr)` - Jump to address if top of stack is 0
- `Call(addr)` - Call function at address
- `Ret` - Return from function

### Local Variables

- `GetLocal(offset)` - Push local variable at offset from BP
- `SetLocal(offset)` - Pop value and store in local variable at offset from BP

### Frame Management

- `GetBP` - Push current base pointer onto stack
- `SetBP` - Pop value and set as new base pointer
- `GetSP` - Push current stack pointer onto stack
- `SetSP` - Pop value and set as new stack pointer

## Function Calling Convention

### Stack Frame Layout and Return Value Management

#### During Function Execution

When a function is called, the following stack frame is created:

```
Higher Addresses
┌─────────────────┐
│   Local Var N   │ ← BP + N
├─────────────────┤
│      ...        │
├─────────────────┤
│   Local Var 1   │ ← BP + 1
├─────────────────┤
│   Local Var 0   │ ← BP + 0 (Base Pointer)
├─────────────────┤
│   Return Addr   │ ← BP - 1
├─────────────────┤
│   Old BP        │ ← BP - 2
├─────────────────┤
│   Arg N-1       │ ← BP - 3
├─────────────────┤
│      ...        │
├─────────────────┤
│   Arg 1         │ ← BP - (N+2)
├─────────────────┤
│   Arg 0         │ ← BP - (N+1)
├─────────────────┤
│   Return Value  │ ← BP - (N+2) (reserved by caller)
└─────────────────┘
Lower Addresses
```

#### After Function Return

Upon function return, the stack is reorganized with the return value in place of the placeholder:

```
Higher Addresses
┌─────────────────┐
│   Arg N-1       │ ← Arguments remain on stack for caller cleanup
├─────────────────┤
│      ...        │
├─────────────────┤
│   Arg 1         │
├─────────────────┤
│   Arg 0         │
├─────────────────┤
│  Return Value   │ ← Placeholder overwritten with actual return value
└─────────────────┘
Lower Addresses
```

**Key Points:**

- The caller reserves space for the return value before pushing arguments
- The callee overwrites the placeholder with the actual return value during return
- This arrangement allows proper stack cleanup and supports multiple function calls in expressions

### Calling Sequence

#### Caller Responsibilities:

1. Push placeholder for return value (e.g., 0)
2. Push function arguments onto stack (left to right)
3. Push current PC (return address)
4. Push current BP (old base pointer)
5. Set new BP to current SP
6. Jump to function address

#### Callee Responsibilities:

1. Reserve space for local variables by adjusting SP
2. Execute function body
3. Push return value onto stack
4. Restore old BP and SP
5. Return to caller

#### Return Sequence:

1. Callee prepares return value on top of evaluation stack
2. Callee restores BP and SP to remove local variables and frame info
3. Callee overwrites the return value placeholder with actual return value
4. Callee jumps to return address
5. Caller pops arguments in reverse order (last to first)
6. Return value remains on top of stack for caller use

**Critical Note**: The caller must reserve space for the return value before pushing arguments. This ensures the return value is placed in the correct position below the arguments, allowing multiple function calls in expressions like `f(x) + g(y)` to work correctly.

### Instruction Sequence Example

For a function call `add(2, 3)`:

```assembly
; Caller side:
Push 0          ; Placeholder for return value
Push 2          ; Arg 0
Push 3          ; Arg 1
Push RetAddr    ; Return address
Push BP         ; Old base pointer
SetBP           ; BP = SP
Call add_func   ; Jump to function

; Function prologue (add_func):
; (BP now points to first local variable slot)

; Function body:
GetLocal -4     ; Load arg 0 (2)
GetLocal -5     ; Load arg 1 (3)
Add             ; Compute 2 + 3

; Function epilogue (return value = 5 on stack):
; Stack before return: [0, 2, 3, ret_addr, old_bp, 5]
; The Ret instruction overwrites placeholder: [5, 2, 3]
Ret             ; Return to caller, overwrites return value placeholder

; Caller cleanup:
Pop             ; Remove arg 1 (3)
Pop             ; Remove arg 0 (2)
; Return value (5) now on top of stack

; Stack progression example for expression add(2,3) + add(4,5):
; Initial:           [...]
; After placeholder: [0, ...]       ; Placeholder for add(2,3)
; After first call:  [5, ...]       ; 5 from add(2,3)
; After placeholder: [0, 5, ...]    ; Placeholder for add(4,5)
; After second call: [7, 5, ...]    ; 7 from add(4,5), 5 still accessible
; After Add:         [12, ...]      ; 5 + 7 = 12
```

## Variable Access

### Local Variables

- Accessed via `GetLocal(offset)` and `SetLocal(offset)`
- Offset is relative to current BP
- Positive offsets: local variables
- Negative offsets: function parameters and frame info

### Global Variables

- Stored in separate global memory region
- Accessed via special global variable instructions (future extension)

## Error Handling

### Stack Overflow

- VM checks stack bounds on push operations
- Throws runtime error if stack limit exceeded

### Stack Underflow

- VM checks stack size on pop operations
- Throws runtime error if attempting to pop from empty stack

### Invalid Jumps

- VM validates jump addresses are within program bounds
- Throws runtime error for invalid jump targets

## Performance Considerations

### Optimization Opportunities

1. **Instruction Fusion**: Combine common instruction patterns
2. **Register Allocation**: Use virtual registers for frequently accessed locals
3. **Tail Call Optimization**: Eliminate stack frame for tail calls
4. **Constant Folding**: Evaluate constant expressions at compile time

### Memory Management

- Stack size is configurable
- Automatic stack frame cleanup on return
- No garbage collection needed for basic types

## Implementation Notes

### Instruction Encoding

- Instructions are represented as Rust enums
- Immediate values stored directly in instruction
- Addresses are resolved during compilation phase

### Type Safety

- VM operates on untyped 32-bit integers
- Type safety enforced at compilation phase
- Boolean values represented as 0 (false) or 1 (true)

### Debug Support

- Instructions can include debug information
- Stack trace reconstruction possible via BP chain
- Breakpoint support via special debug instructions
