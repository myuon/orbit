use crate::arm64::{ARM64CodeGen, Condition, Register};
use crate::runtime::Value;
use crate::vm::Instruction;
use anyhow::Result;
use memmap2::MmapMut;
use std::collections::HashMap;

/// Format binary data as hexdump
fn format_hexdump(data: &[u8], start_addr: usize) -> String {
    let mut result = String::new();

    for (i, chunk) in data.chunks(16).enumerate() {
        let addr = start_addr + i * 16;
        // result.push_str(&format!("{:08x}: ", addr));

        // Hex bytes
        for (j, byte) in chunk.iter().enumerate() {
            if j == 8 {
                result.push(' ');
            }
            result.push_str(&format!("{:02x} ", byte));
        }

        // Pad with spaces if less than 16 bytes
        for j in chunk.len()..16 {
            if j == 8 {
                result.push(' ');
            }
            result.push_str("   ");
        }

        result.push_str("\n");
    }

    result
}

/// JIT compiled function signature
/// Arguments:
/// - stack: mutable pointer to encoded stack values
/// - pc: mutable reference to program counter
/// - bp: mutable reference to base pointer
/// - sp: mutable reference to stack pointer (also serves as stack length)
/// - hp: mutable reference to heap pointer
/// - heap: mutable reference to heap storage
/// - globals: mutable reference to global variables
pub type JITFunction = extern "C" fn(
    stack: *mut u64,          // .x0
    pc: *mut usize,           // .x1
    bp: *mut usize,           // .x2
    sp: *mut usize,           // .x3
    hp: *mut usize,           // .x4
    heap: *mut Vec<Value>,    // .x5
    globals: *mut Vec<Value>, // .x6
);

/// Executable memory region for JIT compiled code
pub struct ExecutableMemory {
    /// Memory mapped region
    mmap: MmapMut,
    /// Current write position in the memory
    offset: usize,
}

impl std::fmt::Debug for ExecutableMemory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExecutableMemory")
            .field("size", &self.mmap.len())
            .field("offset", &self.offset)
            .finish()
    }
}

impl ExecutableMemory {
    /// Create a new executable memory region with specified size
    pub fn new(size: usize) -> Result<Self> {
        let mmap = MmapMut::map_anon(size)?;
        Ok(ExecutableMemory { mmap, offset: 0 })
    }

    /// Write machine code bytes to the memory region
    pub fn write_bytes(&mut self, bytes: &[u8]) -> Result<usize> {
        if self.offset + bytes.len() > self.mmap.len() {
            return Err(anyhow::anyhow!("Not enough memory for JIT code"));
        }

        let start_offset = self.offset;
        self.mmap[self.offset..self.offset + bytes.len()].copy_from_slice(bytes);
        self.offset += bytes.len();
        Ok(start_offset)
    }

    /// Make the memory region executable
    pub fn make_executable(&mut self) -> Result<()> {
        // Make the memory executable
        unsafe {
            use std::ffi::c_void;
            let ptr = self.mmap.as_ptr() as *mut c_void;
            let len = self.mmap.len();

            #[cfg(target_os = "macos")]
            {
                if libc::mprotect(ptr, len, libc::PROT_READ | libc::PROT_EXEC) != 0 {
                    return Err(anyhow::anyhow!("Failed to make memory executable"));
                }
            }

            #[cfg(target_os = "linux")]
            {
                if libc::mprotect(ptr, len, libc::PROT_READ | libc::PROT_EXEC) != 0 {
                    return Err(anyhow::anyhow!("Failed to make memory executable"));
                }
            }
        }
        Ok(())
    }

    /// Get a function pointer to the compiled code at given offset
    pub fn get_function_ptr(&self, offset: usize) -> JITFunction {
        unsafe {
            let ptr = self.mmap.as_ptr().add(offset);
            std::mem::transmute(ptr)
        }
    }

    /// Get the base pointer of the memory region
    pub fn as_ptr(&self) -> *const u8 {
        self.mmap.as_ptr()
    }
}

/// JIT function information
#[derive(Clone)]
pub struct JITCompiledFunction {
    /// Starting address in VM bytecode
    pub start_addr: usize,
    /// Function pointer to compiled code
    pub function_ptr: JITFunction,
    /// Number of times this function has been called
    pub call_count: u64,
    /// Size of compiled code in bytes
    pub code_size: usize,
}

impl std::fmt::Debug for JITCompiledFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JITCompiledFunction")
            .field("start_addr", &self.start_addr)
            .field("function_ptr", &"<function pointer>")
            .field("call_count", &self.call_count)
            .field("code_size", &self.code_size)
            .finish()
    }
}

/// ARM64 JIT Compiler
#[derive(Debug)]
pub struct ARM64JITCompiler {
    /// Executable memory region
    executable_memory: ExecutableMemory,
    /// Map from VM address to JIT function
    compiled_functions: HashMap<usize, JITCompiledFunction>,
    /// Call count threshold for JIT compilation
    jit_threshold: u64,
    /// Output file for JIT compiled machine code
    jit_compile_output: Option<String>,
}

impl ARM64JITCompiler {
    /// Create a new ARM64 JIT compiler
    pub fn new() -> Result<Self> {
        let executable_memory = ExecutableMemory::new(1024 * 1024)?; // 1MB
        Ok(ARM64JITCompiler {
            executable_memory,
            compiled_functions: HashMap::new(),
            jit_threshold: 10, // Compile after 10 calls
            jit_compile_output: None,
        })
    }

    /// Check if a function should be JIT compiled based on call count
    pub fn should_jit_compile(&self, addr: usize, call_count: u64) -> bool {
        call_count >= self.jit_threshold && !self.compiled_functions.contains_key(&addr)
    }

    /// Compile a function to ARM64 machine code
    pub fn compile_function(
        &mut self,
        start_addr: usize,
        instructions: &[Instruction],
        print_asm: bool,
    ) -> Result<()> {
        // Generate actual ARM64 code for the VM instructions
        let machine_code = if instructions.is_empty() {
            self.generate_arm64_stub()?
        } else {
            self.generate_arm64_code(start_addr, instructions)?
        };

        // Write machine code to specified output file if configured
        if let Some(ref output_file) = self.jit_compile_output {
            std::fs::write(output_file, &machine_code)?;
        }

        let offset = self.executable_memory.write_bytes(&machine_code)?;
        self.executable_memory.make_executable()?;

        // Print JIT assembly hexdump if requested
        if print_asm {
            eprintln!(
                "JIT: Compiled function at address {} ({} bytes):",
                start_addr,
                machine_code.len()
            );
            eprint!("{}", format_hexdump(&machine_code, offset));
        }

        let function_ptr = self.executable_memory.get_function_ptr(offset);

        let jit_function = JITCompiledFunction {
            start_addr,
            function_ptr,
            call_count: 0,
            code_size: machine_code.len(),
        };

        self.compiled_functions.insert(start_addr, jit_function);
        Ok(())
    }

    /// Get compiled function if available
    pub fn get_compiled_function(&self, addr: usize) -> Option<&JITCompiledFunction> {
        self.compiled_functions.get(&addr)
    }

    /// Generate ARM64 machine code for VM instructions
    fn generate_arm64_code(
        &self,
        start_addr: usize,
        instructions: &[Instruction],
    ) -> Result<Vec<u8>> {
        let mut gen = ARM64CodeGen::new();
        let mut jump_sources: HashMap<usize, usize> = HashMap::new();
        let mut jump_targets: HashMap<usize, usize> = HashMap::new();
        let mut call_sources: HashMap<usize, usize> = HashMap::new();
        let mut call_targets: HashMap<usize, usize> = HashMap::new();

        // First pass: identify jump/call sources and targets
        for (source_idx, instruction) in instructions.iter().enumerate() {
            match instruction {
                Instruction::Jump(_label) => {
                    panic!("Jump with label should have been resolved to JumpRel before JIT compilation");
                }
                Instruction::JumpIfZero(_label) => {
                    panic!("JumpIfZero with label should have been resolved to JumpIfZeroRel before JIT compilation");
                }
                Instruction::CallRel(offset) => {
                    let vm_addr = start_addr + source_idx;
                    let target_addr = (vm_addr as i32 + offset) as usize;
                    eprintln!(
                        "DEBUG: CallRel at VM addr {} (array idx {}) -> target {}",
                        vm_addr, source_idx, target_addr
                    );
                    call_sources.insert(vm_addr, usize::MAX);
                    call_targets.insert(target_addr, usize::MAX);
                }
                _ => {}
            }
        }

        // JIT context register assignments (matching Zig implementation):
        const REG_C_STACK: Register = Register::X0;
        const REG_C_PC: Register = Register::X1;
        const REG_C_BP: Register = Register::X2;
        const REG_C_SP: Register = Register::X3;
        // const REG_C_HP: Register = Register::X4;
        // const REG_C_HEAP: Register = Register::X5;
        // const REG_C_GLOBALS: Register = Register::X6;

        // Working registers
        const REG_TEMP1: Register = Register::X9;
        const REG_TEMP2: Register = Register::X10;
        const REG_TEMP3: Register = Register::X11;

        // Helper closures for common operations
        let push_to_stack = |gen: &mut ARM64CodeGen, src_reg: Register| -> Result<()> {
            // Load current SP: *REG_C_SP
            gen.ldr(REG_C_SP, 0, REG_TEMP2);

            // Calculate stack address: REG_C_STACK + (SP * 8)
            gen.mov_imm(REG_TEMP3, 8);
            gen.mul(REG_TEMP2, REG_TEMP3, REG_TEMP3);
            gen.add_reg(REG_C_STACK, REG_TEMP3, REG_TEMP3);

            // Store value to stack: [REG_C_STACK + (SP * 8)] = src_reg
            gen.str(src_reg, REG_TEMP3, 0);

            // Increment SP: *REG_C_SP += 1
            gen.add_imm(REG_TEMP2, 1, REG_TEMP2);
            gen.str(REG_TEMP2, REG_C_SP, 0);

            Ok(())
        };

        let pop_from_stack = |gen: &mut ARM64CodeGen, dst_reg: Register| -> Result<()> {
            // Load current SP: *REG_C_SP
            gen.ldr(REG_C_SP, 0, REG_TEMP2);

            // Decrement SP: *REG_C_SP -= 1
            gen.sub_imm(REG_TEMP2, 1, REG_TEMP2);
            gen.str(REG_TEMP2, REG_C_SP, 0);

            // Calculate stack address: REG_C_STACK + (SP * 8)
            gen.mov_imm(REG_TEMP3, 8);
            gen.mul(REG_TEMP2, REG_TEMP3, REG_TEMP3);
            gen.add_reg(REG_C_STACK, REG_TEMP3, REG_TEMP3);

            // Load value from stack: dst_reg = [REG_C_STACK + (SP * 8)]
            gen.ldr(REG_TEMP3, 0, dst_reg);

            Ok(())
        };

        // Second pass: generate code and record jump/call target positions
        for (instruction_idx, instruction) in instructions.iter().enumerate() {
            let vm_addr = start_addr + instruction_idx;
            // Record jump target positions
            if jump_targets.contains_key(&vm_addr) {
                eprintln!(
                    "DEBUG: Recording jump target at VM addr {} (array idx {}) -> position {}",
                    vm_addr,
                    instruction_idx,
                    gen.position()
                );
                jump_targets.insert(vm_addr, gen.position());
            }
            // Record call target positions
            if call_targets.contains_key(&vm_addr) {
                eprintln!(
                    "DEBUG: Recording call target at VM addr {} (array idx {}) -> position {}",
                    vm_addr,
                    instruction_idx,
                    gen.position()
                );
                call_targets.insert(vm_addr, gen.position());
            }
            if instruction_idx == 0 {
                gen.function_prologue();
            }

            match instruction {
                Instruction::Push(value) => {
                    if *value >= 0 {
                        gen.mov_imm(REG_TEMP1, *value as u16);
                    } else {
                        // For negative values, use MOVN with (abs(value) - 1)
                        gen.movn_imm(REG_TEMP1, (value.abs() - 1) as u16);
                    }
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::PushAddress(addr) => {
                    gen.mov_imm(REG_TEMP1, *addr as u16);
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Pop => {
                    pop_from_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Add => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.add_reg(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a + b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Sub => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.sub_reg(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a - b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Mul => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.mul(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a * b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Div => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                                                          // gen.emit(0xD4200000);
                    gen.sdiv(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a / b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Mod => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.sdiv(REG_TEMP2, REG_TEMP1, REG_TEMP3); // a / b
                    gen.msub(REG_TEMP3, REG_TEMP1, REG_TEMP2, REG_TEMP1); // a - (a/b) * b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::AddressAdd => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b (offset)
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a (address/heapref)
                    gen.add_reg(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a + b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::AddressSub => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b (offset)
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a (address/heapref)
                    gen.sub_reg(REG_TEMP2, REG_TEMP1, REG_TEMP1); // a - b
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Eq => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.subs(REG_TEMP2, REG_TEMP1, REG_TEMP3); // a - b, set flags
                    gen.cset(REG_TEMP1, Condition::EQ); // REG_TEMP1 = (a == b) ? 1 : 0
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Lt => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.cmp(REG_TEMP2, REG_TEMP1); // compare a, b
                    gen.cset(REG_TEMP1, Condition::LT); // REG_TEMP1 = (a < b) ? 1 : 0
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Lte => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.cmp(REG_TEMP2, REG_TEMP1); // compare a, b
                    gen.cset(REG_TEMP1, Condition::LE); // REG_TEMP1 = (a <= b) ? 1 : 0
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Gt => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.cmp(REG_TEMP2, REG_TEMP1); // compare a, b
                    gen.cset(REG_TEMP1, Condition::GT); // REG_TEMP1 = (a > b) ? 1 : 0
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::Gte => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // b
                    pop_from_stack(&mut gen, REG_TEMP2)?; // a
                    gen.cmp(REG_TEMP2, REG_TEMP1); // compare a, b
                    gen.cset(REG_TEMP1, Condition::GE); // REG_TEMP1 = (a >= b) ? 1 : 0
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::GetSP => {
                    gen.ldr(REG_C_SP, 0, REG_TEMP1); // Load *REG_C_SP
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::SetSP => {
                    pop_from_stack(&mut gen, REG_TEMP1)?;
                    gen.str(REG_TEMP1, REG_C_SP, 0); // Store to *REG_C_SP
                }

                Instruction::GetBP => {
                    gen.ldr(REG_C_BP, 0, REG_TEMP1); // Load *REG_C_BP
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::SetBP => {
                    pop_from_stack(&mut gen, REG_TEMP1)?;
                    gen.str(REG_TEMP1, REG_C_BP, 0); // Store to *REG_C_BP
                }

                Instruction::GetPC => {
                    gen.ldr(REG_C_PC, 0, REG_TEMP1); // Load *REG_C_PC
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::SetPC => {
                    pop_from_stack(&mut gen, REG_TEMP1)?;
                    gen.str(REG_TEMP1, REG_C_PC, 0); // Store to *REG_C_PC
                }

                Instruction::GetLocal(offset) => {
                    // Load BP: *REG_C_BP
                    gen.ldr(REG_C_BP, 0, REG_TEMP2);

                    if *offset >= 0 {
                        // Positive offset: BP + offset
                        gen.mov_imm(REG_TEMP3, *offset as u16);
                        gen.add_reg(REG_TEMP2, REG_TEMP3, REG_TEMP2);
                    } else {
                        // Negative offset: BP - abs(offset)
                        gen.mov_imm(REG_TEMP3, offset.abs() as u16);
                        gen.sub_reg(REG_TEMP2, REG_TEMP3, REG_TEMP2);
                    }

                    // Calculate stack address: REG_C_STACK + (index * 8)
                    gen.mov_imm(REG_TEMP3, 8);
                    gen.mul(REG_TEMP2, REG_TEMP3, REG_TEMP3);
                    gen.add_reg(REG_C_STACK, REG_TEMP3, REG_TEMP3);

                    // Load value from calculated address
                    gen.ldr(REG_TEMP3, 0, REG_TEMP1);
                    push_to_stack(&mut gen, REG_TEMP1)?;
                }

                Instruction::SetLocal(offset) => {
                    pop_from_stack(&mut gen, REG_TEMP1)?; // value to store

                    // Load BP: *REG_C_BP
                    gen.ldr(REG_C_BP, 0, REG_TEMP2);

                    if *offset >= 0 {
                        // Positive offset: BP + offset
                        gen.mov_imm(REG_TEMP3, *offset as u16);
                        gen.add_reg(REG_TEMP2, REG_TEMP3, REG_TEMP2);
                    } else {
                        // Negative offset: BP - abs(offset)
                        gen.mov_imm(REG_TEMP3, offset.abs() as u16);
                        gen.sub_reg(REG_TEMP2, REG_TEMP3, REG_TEMP2);
                    }

                    // Calculate stack address: REG_C_STACK + (index * 8)
                    gen.mov_imm(REG_TEMP3, 8);
                    gen.mul(REG_TEMP2, REG_TEMP3, REG_TEMP3);
                    gen.add_reg(REG_C_STACK, REG_TEMP3, REG_TEMP3);

                    // Store value to calculated address
                    gen.str(REG_TEMP1, REG_TEMP3, 0);
                }

                Instruction::Ret => {
                    // Pop return address from stack and set as PC
                    pop_from_stack(&mut gen, REG_TEMP1)?; // Pop return address (ValueEncoded)

                    // Remove the ValueEncoding bit (1u64 << 63) to get the actual address
                    // Use LSL/LSR trick to clear the MSB: shift left 1 bit, then right 1 bit
                    gen.lsl_imm(REG_TEMP1, REG_TEMP1, 1); // Left shift by 1 (removes MSB)
                    gen.lsr_imm(REG_TEMP1, REG_TEMP1, 1); // Right shift by 1 (restores position, MSB=0)

                    gen.str(REG_TEMP1, REG_C_PC, 0); // Store actual address to *REG_C_PC

                    // Function epilogue and return
                    gen.function_epilogue();
                    gen.ret();
                }

                Instruction::Jump(_label) => {
                    return Err(anyhow::anyhow!("Jump with label should have been resolved to JumpRel before JIT compilation"));
                }

                Instruction::JumpIfZero(_label) => {
                    return Err(anyhow::anyhow!("JumpIfZero with label should have been resolved to JumpIfZeroRel before JIT compilation"));
                }

                Instruction::JumpRel(offset) => {
                    // Unconditional relative jump
                    // Calculate target VM address
                    let target_vm_addr = (vm_addr as i32 + offset) as usize;

                    // Emit placeholder for branch instruction
                    gen.emit(0x0);
                    jump_sources.insert(vm_addr, gen.position() - 1);
                    jump_targets.insert(target_vm_addr, usize::MAX);
                }

                Instruction::JumpIfZeroRel(offset) => {
                    // Conditional relative jump: if stack top == 0, branch
                    pop_from_stack(&mut gen, REG_TEMP1)?; // Pop condition value

                    // Calculate target VM address
                    let target_vm_addr = (vm_addr as i32 + offset) as usize;

                    // Emit CBZ instruction with placeholder offset
                    let cbz_instruction = 0xB4000000 | REG_TEMP1.as_u32();
                    gen.emit(cbz_instruction);
                    jump_sources.insert(vm_addr, gen.position() - 1);
                    jump_targets.insert(target_vm_addr, usize::MAX);
                }

                Instruction::CallRel(_offset) => {
                    // Function call: push current PC as return address and branch
                    // Push current PC + 1 as return address
                    // gen.mov_imm(REG_TEMP1, (vm_addr + 1) as u16);
                    // push_to_stack(&mut gen, REG_TEMP1)?;

                    // Emit placeholder for branch instruction
                    gen.emit(0x0);
                    call_sources.insert(vm_addr, gen.position() - 1);
                }

                Instruction::Nop => {
                    // No operation - just continue
                    continue;
                }

                // Unsupported instructions (these will cause fallback to interpreter)
                _ => {
                    return Err(anyhow::anyhow!(
                        "Unsupported instruction for JIT: {:?}",
                        instruction
                    ));
                }
            }
        }

        // Third pass: patch jump instructions with correct offsets
        for (instruction_idx, instruction) in instructions.iter().enumerate() {
            let vm_addr = start_addr + instruction_idx;
            match instruction {
                Instruction::Jump(_label) => {
                    // Should not reach here after label resolution
                }
                Instruction::JumpIfZero(_label) => {
                    // Should not reach here after label resolution
                }

                Instruction::JumpRel(offset) => {
                    let source_addr = jump_sources
                        .get(&vm_addr)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("JumpRel source not found"))?;
                    let target_vm_addr = (vm_addr as i32 + offset) as usize;
                    let target_addr = jump_targets
                        .get(&target_vm_addr)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("JumpRel target not found"))?;
                    assert!(target_addr != usize::MAX, "JumpRel target not set");

                    let branch_offset = target_addr as i32 - source_addr as i32;
                    let b_instruction = ARM64CodeGen::get_b_instr(branch_offset);
                    gen.patch(source_addr, b_instruction);
                }

                Instruction::JumpIfZeroRel(offset) => {
                    let source_addr = jump_sources
                        .get(&vm_addr)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("JumpIfZeroRel source not found"))?;
                    let target_vm_addr = (vm_addr as i32 + offset) as usize;
                    let target_addr = jump_targets
                        .get(&target_vm_addr)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("JumpIfZeroRel target not found"))?;
                    assert!(target_addr != usize::MAX, "JumpIfZeroRel target not set");

                    let branch_offset = target_addr as i32 - source_addr as i32;
                    let cbz_offset = ARM64CodeGen::get_cbz_offset(branch_offset);

                    // Get existing CBZ instruction and add offset
                    let existing_instruction = gen.code[source_addr];
                    assert_eq!(
                        existing_instruction & 0xFF000000,
                        0xB4000000,
                        "Expected CBZ instruction"
                    );
                    let patched_instruction = existing_instruction | cbz_offset;
                    gen.patch(source_addr, patched_instruction);
                }

                Instruction::CallRel(offset) => {
                    let source_addr = call_sources
                        .get(&vm_addr)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("CallRel source not found"))?;
                    let target_addr_calc = (vm_addr as i32 + offset) as usize;
                    let target_addr = call_targets
                        .get(&target_addr_calc)
                        .copied()
                        .ok_or_else(|| anyhow::anyhow!("CallRel target not found"))?;
                    assert!(target_addr != usize::MAX, "CallRel target not set");

                    let branch_offset = target_addr as i32 - source_addr as i32;
                    let bl_instruction = ARM64CodeGen::get_bl_instr(branch_offset);
                    gen.patch(source_addr, bl_instruction);
                }
                _ => {}
            }
        }

        Ok(gen.finalize())
    }

    /// Generate ARM64 machine code stub (placeholder implementation)
    fn generate_arm64_stub(&self) -> Result<Vec<u8>> {
        // Simple stub that returns 42
        let mut gen = ARM64CodeGen::new();
        gen.mov_imm(Register::X0, 42);
        gen.ret();
        Ok(gen.finalize())
    }

    /// Set JIT compilation threshold
    pub fn set_jit_threshold(&mut self, threshold: u64) {
        self.jit_threshold = threshold;
    }

    /// Set JIT compile output file
    pub fn set_jit_compile_output(&mut self, output_file: Option<String>) {
        self.jit_compile_output = output_file;
    }

    /// Get compilation statistics
    pub fn get_stats(&self) -> JITStats {
        JITStats {
            compiled_functions: self.compiled_functions.len(),
            total_code_size: self.compiled_functions.values().map(|f| f.code_size).sum(),
            jit_threshold: self.jit_threshold,
        }
    }
}

/// JIT compilation statistics
#[derive(Debug)]
pub struct JITStats {
    pub compiled_functions: usize,
    pub total_code_size: usize,
    pub jit_threshold: u64,
}

impl std::fmt::Display for JITStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "JIT Stats: {} compiled functions, {} bytes total, threshold: {} calls",
            self.compiled_functions, self.total_code_size, self.jit_threshold
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_executable_memory_creation() {
        let result = ExecutableMemory::new(4096);
        assert!(result.is_ok());
    }

    #[test]
    fn test_jit_compiler_creation() {
        let result = ARM64JITCompiler::new();
        assert!(result.is_ok());
    }

    #[test]
    fn test_jit_threshold() {
        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Should not compile initially
        assert!(!compiler.should_jit_compile(100, 5));

        // Should compile after threshold
        assert!(compiler.should_jit_compile(100, 10));

        // Change threshold
        compiler.set_jit_threshold(20);
        assert!(!compiler.should_jit_compile(200, 15));
        assert!(compiler.should_jit_compile(200, 20));
    }

    #[test]
    fn test_arm64_code_generation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test simple arithmetic sequence
        let instructions = vec![
            Instruction::Push(10),
            Instruction::Push(20),
            Instruction::Add,
            Instruction::Ret,
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x100, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile basic instructions: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x100).is_some());
    }

    #[test]
    fn test_unsupported_instruction_handling() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test unsupported instruction (should fail gracefully)
        let instructions = vec![
            Instruction::Push(10),
            Instruction::Syscall, // This should be unsupported
        ];

        let result = compiler.compile_function(0x200, &instructions, false);
        assert!(result.is_err(), "Should fail for unsupported instructions");

        // Should not have a compiled function
        assert!(compiler.get_compiled_function(0x200).is_none());
    }

    #[test]
    fn test_ret_instruction_with_stack_pop() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test sequence: Push return address, then Ret (should pop and set PC)
        let instructions = vec![
            Instruction::Push(0x1000), // Push return address
            Instruction::Ret,          // Pop return address and set PC
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x300, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile Ret instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x300).is_some());
    }

    #[test]
    fn test_jump_rel_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test unconditional jump (using JumpRel which is what should be generated after label resolution)
        let instructions = vec![
            Instruction::Push(1),
            Instruction::JumpRel(2), // Jump 2 instructions forward (skip Push(2))
            Instruction::Push(2),    // Should be skipped
            Instruction::Push(3),    // Target of jump
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x400, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile JumpRel instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x400).is_some());
    }

    #[test]
    fn test_jump_if_zero_rel_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test conditional jump (using JumpIfZeroRel which is what should be generated after label resolution)
        let instructions = vec![
            Instruction::Push(0),          // Push zero
            Instruction::JumpIfZeroRel(2), // Jump 2 instructions forward if zero (skip Push(2))
            Instruction::Push(2),          // Should be skipped since condition is true
            Instruction::Push(3),          // Target of jump
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x500, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile JumpIfZeroRel instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x500).is_some());
    }

    #[test]
    fn test_get_pc_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test GetPC instruction
        let instructions = vec![
            Instruction::GetPC, // Get current PC
            Instruction::Ret,   // Return
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x600, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile GetPC instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x600).is_some());
    }

    #[test]
    fn test_set_pc_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test SetPC instruction
        let instructions = vec![
            Instruction::Push(0x1000), // Push address
            Instruction::SetPC,        // Set PC to address
            Instruction::Ret,          // Return
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x700, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile SetPC instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x700).is_some());
    }

    #[test]
    fn test_unsupported_instruction_failure() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test that unsupported instructions fail compilation
        let instructions = vec![
            Instruction::Push(10),
            Instruction::Syscall, // This is unsupported and should fail
        ];

        // Should fail for unsupported instructions
        let result = compiler.compile_function(0x800, &instructions, false);
        assert!(result.is_err(), "Should fail for unsupported instructions");

        // Should not have a compiled function
        assert!(compiler.get_compiled_function(0x800).is_none());
    }

    #[test]
    fn test_address_add_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test AddressAdd instruction
        let instructions = vec![
            Instruction::PushAddress(1000), // Push base address
            Instruction::Push(5),           // Push offset
            Instruction::AddressAdd,        // Add offset to address
            Instruction::Ret,               // Return
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x900, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile AddressAdd instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x900).is_some());
    }

    #[test]
    fn test_address_sub_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test AddressSub instruction
        let instructions = vec![
            Instruction::PushAddress(1000), // Push base address
            Instruction::Push(5),           // Push offset
            Instruction::AddressSub,        // Subtract offset from address
            Instruction::Ret,               // Return
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x1000, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile AddressSub instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x1000).is_some());
    }

    #[test]
    fn test_call_rel_instruction_compilation() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test CallRel instruction with simple relative call
        let instructions = vec![
            Instruction::Push(42),   // Push some value
            Instruction::CallRel(2), // Call function at relative offset +2 (instruction at index 3)
            Instruction::Ret,        // Return from main function
            Instruction::Push(100),  // Target function starts here
            Instruction::Ret,        // Return from called function
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x1100, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile CallRel instruction: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x1100).is_some());
    }

    #[test]
    fn test_call_rel_recursive_function() {
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test recursive function call (calls itself) - simplified version
        let instructions = vec![
            Instruction::Push(5),    // 0: Push initial value
            Instruction::CallRel(1), // 1: Call function at offset +1 (instruction 2)
            Instruction::Ret,        // 2: Return value (target of call)
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x1200, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile recursive CallRel: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x1200).is_some());
    }

    #[test]
    fn test_comparison_instructions_compilation() {
        use crate::runtime::VM;
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test comparison instructions: 3 < 7 should be true (1)
        let instructions = vec![
            Instruction::Push(3), // Push first operand (a)
            Instruction::Push(7), // Push second operand (b)
            Instruction::Lt,      // Compare a < b (should be true)
            Instruction::Ret,     // Return result
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x1300, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile comparison instructions: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x1300).is_some());

        // Verify with VM that this should return 1 (true) - but without Ret for simple test
        let vm_instructions = vec![
            Instruction::Push(3), // Push first operand (a)
            Instruction::Push(7), // Push second operand (b)
            Instruction::Lt,      // Compare a < b (should be true)
        ];
        let mut vm = VM::new();
        vm.load_program(vm_instructions);
        let vm_result = vm.execute().unwrap();
        println!("VM result for 3 < 7: {}", vm_result);
        assert_eq!(vm_result, 1); // 3 < 7 should be true
    }

    #[test]
    fn test_gt_comparison_compilation() {
        use crate::runtime::VM;
        use crate::vm::Instruction;

        let mut compiler = ARM64JITCompiler::new().unwrap();

        // Test GT comparison: 7 > 3 should be true (1)
        let instructions = vec![
            Instruction::Push(7), // Push first operand (a)
            Instruction::Push(3), // Push second operand (b)
            Instruction::Gt,      // Compare a > b (should be true)
            Instruction::Ret,     // Return result
        ];

        // Should successfully compile without errors
        let result = compiler.compile_function(0x1400, &instructions, false);
        assert!(
            result.is_ok(),
            "Failed to compile GT comparison: {:?}",
            result.err()
        );

        // Should have a compiled function
        assert!(compiler.get_compiled_function(0x1400).is_some());

        // Verify with VM that this should return 1 (true) - but without Ret for simple test
        let vm_instructions = vec![
            Instruction::Push(7), // Push first operand (a)
            Instruction::Push(3), // Push second operand (b)
            Instruction::Gt,      // Compare a > b (should be true)
        ];
        let mut vm = VM::new();
        vm.load_program(vm_instructions);
        let vm_result = vm.execute().unwrap();
        println!("VM result for 7 > 3: {}", vm_result);
        assert_eq!(vm_result, 1); // 7 > 3 should be true
    }

    #[test]
    fn test_comparison_false_cases() {
        use crate::runtime::VM;
        use crate::vm::Instruction;

        // Test 7 < 3 should be false (0)
        let vm_instructions = vec![
            Instruction::Push(7), // Push first operand (a)
            Instruction::Push(3), // Push second operand (b)
            Instruction::Lt,      // Compare a < b (should be false)
        ];
        let mut vm = VM::new();
        vm.load_program(vm_instructions);
        let vm_result = vm.execute().unwrap();
        println!("VM result for 7 < 3: {}", vm_result);
        assert_eq!(vm_result, 0); // 7 < 3 should be false

        // Test 3 > 7 should be false (0)
        let vm_instructions = vec![
            Instruction::Push(3), // Push first operand (a)
            Instruction::Push(7), // Push second operand (b)
            Instruction::Gt,      // Compare a > b (should be false)
        ];
        let mut vm = VM::new();
        vm.load_program(vm_instructions);
        let vm_result = vm.execute().unwrap();
        println!("VM result for 3 > 7: {}", vm_result);
        assert_eq!(vm_result, 0); // 3 > 7 should be false
    }
}
