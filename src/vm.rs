use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Stack operations
    Push(i64),
    PushString(String), // Only used for global initialization
    PushHeapRef(usize), // Push heap reference by index
    Pop,

    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Address arithmetic operations
    AddressAdd, // Address + Number -> Address
    AddressSub, // Address - Number -> Address

    // Comparison operations
    Eq,
    Lt,
    Lte,
    Gt,
    Gte,

    // Logical operations
    Not,

    // Control flow
    Jump(usize),
    JumpIfZero(usize),

    // Local variables
    GetLocal(i32),
    SetLocal(i32),

    // Global variables
    GetGlobal(usize),
    SetGlobal(usize),

    // Function calls
    Call(String),
    Ret,

    // Frame management
    GetBP,
    SetBP,
    GetSP,
    SetSP,
    GetPC,
    SetPC,

    // Labels
    Label(String),

    // No operation
    Nop,

    // Heap operations
    HeapAlloc, // Allocate new heap object
    HeapGet,   // Get value from heap object
    HeapSet,   // Set value in heap object

    // String operations
    StringNew,

    // Vector operations
    VectorNew,
    VectorPush,
    VectorIndex,
    VectorSet,

    // Map operations
    MapNew,
    MapIndex,
    MapSet,

    // Pointer operations
    PointerAlloc,
    PointerIndex,
    PointerSet,

    // String operations
    StringIndex,

    // Struct operations
    StructNew,
    StructFieldGet,
    StructFieldSet,

    // System calls
    Syscall,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Push(value) => write!(f, "push {}", value),
            Instruction::PushString(s) => write!(f, "push_string \"{}\"", s),
            Instruction::PushHeapRef(index) => write!(f, "push_heap_ref {}", index),
            Instruction::Pop => write!(f, "pop"),
            Instruction::Add => write!(f, "add"),
            Instruction::Sub => write!(f, "sub"),
            Instruction::AddressAdd => write!(f, "address_add"),
            Instruction::AddressSub => write!(f, "address_sub"),
            Instruction::Mul => write!(f, "mul"),
            Instruction::Div => write!(f, "div"),
            Instruction::Mod => write!(f, "mod"),
            Instruction::Eq => write!(f, "eq"),
            Instruction::Lt => write!(f, "lt"),
            Instruction::Lte => write!(f, "lte"),
            Instruction::Gt => write!(f, "gt"),
            Instruction::Gte => write!(f, "gte"),
            Instruction::Not => write!(f, "not"),
            Instruction::Jump(addr) => write!(f, "jump {}", addr),
            Instruction::JumpIfZero(addr) => write!(f, "jump_if_zero {}", addr),
            Instruction::GetLocal(offset) => write!(f, "get_local {}", offset),
            Instruction::SetLocal(offset) => write!(f, "set_local {}", offset),
            Instruction::GetGlobal(index) => write!(f, "get_global {}", index),
            Instruction::SetGlobal(index) => write!(f, "set_global {}", index),
            Instruction::Call(func_name) => write!(f, "call {}", func_name),
            Instruction::Ret => write!(f, "ret"),
            Instruction::GetBP => write!(f, "get_bp"),
            Instruction::SetBP => write!(f, "set_bp"),
            Instruction::GetSP => write!(f, "get_sp"),
            Instruction::SetSP => write!(f, "set_sp"),
            Instruction::GetPC => write!(f, "get_pc"),
            Instruction::SetPC => write!(f, "set_pc"),
            Instruction::Label(name) => write!(f, "{}:", name),
            Instruction::Nop => write!(f, "nop"),
            Instruction::HeapAlloc => write!(f, "heap_alloc"),
            Instruction::HeapGet => write!(f, "heap_get"),
            Instruction::HeapSet => write!(f, "heap_set"),
            Instruction::StringNew => write!(f, "string_new"),
            Instruction::VectorNew => write!(f, "vector_new"),
            Instruction::VectorPush => write!(f, "vector_push"),
            Instruction::VectorIndex => write!(f, "vector_index"),
            Instruction::VectorSet => write!(f, "vector_set"),
            Instruction::MapNew => write!(f, "map_new"),
            Instruction::MapIndex => write!(f, "map_index"),
            Instruction::MapSet => write!(f, "map_set"),
            Instruction::PointerAlloc => write!(f, "pointer_new"),
            Instruction::PointerIndex => write!(f, "pointer_index"),
            Instruction::PointerSet => write!(f, "pointer_set"),
            Instruction::StringIndex => write!(f, "string_index"),
            Instruction::StructNew => write!(f, "struct_new"),
            Instruction::StructFieldGet => write!(f, "struct_field_get"),
            Instruction::StructFieldSet => write!(f, "struct_field_set"),
            Instruction::Syscall => write!(f, "syscall"),
        }
    }
}
