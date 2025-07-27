/// ARM64 machine code generation for JIT compilation
use anyhow::Result;

/// ARM64 registers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Register {
    X0 = 0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
    X31,
}

impl Register {
    pub fn as_u32(self) -> u32 {
        self as u32
    }
}

/// Special register aliases
pub const XZR: Register = Register::X31; // Zero register
pub const SP: Register = Register::X31; // Stack pointer (context dependent)
pub const LR: Register = Register::X30; // Link register
pub const FP: Register = Register::X29; // Frame pointer

/// ARM64 condition codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Condition {
    EQ = 0b0000, // Equal
    NE = 0b0001, // Not equal
    HS = 0b0010, // Higher or same (unsigned >=)
    LO = 0b0011, // Lower (unsigned <)
    MI = 0b0100, // Minus/negative
    PL = 0b0101, // Plus/positive or zero
    VS = 0b0110, // Overflow
    VC = 0b0111, // No overflow
    HI = 0b1000, // Higher (unsigned >)
    LS = 0b1001, // Lower or same (unsigned <=)
    GE = 0b1010, // Greater or equal (signed >=)
    LT = 0b1011, // Less than (signed <)
    GT = 0b1100, // Greater than (signed >)
    LE = 0b1101, // Less or equal (signed <=)
    AL = 0b1110, // Always
    NV = 0b1111, // Never
}

impl Condition {
    pub fn as_u32(self) -> u32 {
        self as u32
    }

    /// Get the inverted condition
    pub fn invert(self) -> Condition {
        match self {
            Condition::EQ => Condition::NE,
            Condition::NE => Condition::EQ,
            Condition::HS => Condition::LO,
            Condition::LO => Condition::HS,
            Condition::MI => Condition::PL,
            Condition::PL => Condition::MI,
            Condition::VS => Condition::VC,
            Condition::VC => Condition::VS,
            Condition::HI => Condition::LS,
            Condition::LS => Condition::HI,
            Condition::GE => Condition::LT,
            Condition::LT => Condition::GE,
            Condition::GT => Condition::LE,
            Condition::LE => Condition::GT,
            Condition::AL => Condition::NV,
            Condition::NV => Condition::AL,
        }
    }
}

/// ARM64 machine code generator
pub struct ARM64CodeGen {
    /// Generated machine code (32-bit instructions)
    pub code: Vec<u32>,
}

impl ARM64CodeGen {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    /// Emit a 32-bit instruction
    pub fn emit(&mut self, instruction: u32) {
        self.code.push(instruction);
    }

    /// Get the current code position (in instructions, not bytes)
    pub fn position(&self) -> usize {
        self.code.len()
    }

    /// Generate machine code bytes for execution
    pub fn finalize(self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(self.code.len() * 4);
        for instruction in self.code {
            bytes.extend_from_slice(&instruction.to_le_bytes());
        }
        bytes
    }

    // === Basic Instructions ===

    /// RET - Return from subroutine
    pub fn ret(&mut self) {
        self.emit(0xd65f03c0); // ret x30
    }

    /// MOV immediate - Move 16-bit immediate to register (with LSL 0)
    pub fn mov_imm(&mut self, dst: Register, imm: u16) {
        let instruction = 0xD2800000 | ((imm as u32) << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// MOVN immediate - Move inverted 16-bit immediate to register
    pub fn movn_imm(&mut self, dst: Register, imm: u16) {
        let instruction = 0x92800000 | ((imm as u32) << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// MOV register - Move register to register (alias for ORR with XZR)
    pub fn mov_reg(&mut self, src: Register, dst: Register) {
        let instruction = 0xAA000000 | (src.as_u32() << 16) | (src.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    // === Arithmetic Instructions ===

    /// ADD - Add registers: dst = src1 + src2
    pub fn add_reg(&mut self, src1: Register, src2: Register, dst: Register) {
        let instruction = 0x8B000000 | (src2.as_u32() << 16) | (src1.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// ADD immediate - Add 12-bit immediate to register
    pub fn add_imm(&mut self, src: Register, imm: u16, dst: Register) {
        assert!(imm <= 0xFFF, "Immediate value must be 12-bit");
        let instruction = 0x91000000 | ((imm as u32) << 10) | (src.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// SUB - Subtract registers: dst = src1 - src2
    pub fn sub_reg(&mut self, src1: Register, src2: Register, dst: Register) {
        let instruction = 0xCB000000 | (src2.as_u32() << 16) | (src1.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// SUB immediate - Subtract 12-bit immediate from register
    pub fn sub_imm(&mut self, src: Register, imm: u16, dst: Register) {
        assert!(imm <= 0xFFF, "Immediate value must be 12-bit");
        let instruction = 0xD1000000 | ((imm as u32) << 10) | (src.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// MUL - Multiply (alias for MADD with XZR as addend)
    pub fn mul(&mut self, src1: Register, src2: Register, dst: Register) {
        self.madd(src1, src2, XZR, dst);
    }

    /// MADD - Multiply-add: dst = src1 * src2 + addend
    pub fn madd(&mut self, src1: Register, src2: Register, addend: Register, dst: Register) {
        let instruction = 0x9B000000
            | (src2.as_u32() << 16)
            | (addend.as_u32() << 10)
            | (src1.as_u32() << 5)
            | dst.as_u32();
        self.emit(instruction);
    }

    /// MSUB - Multiply-subtract: dst = addend - src1 * src2
    pub fn msub(&mut self, src1: Register, src2: Register, addend: Register, dst: Register) {
        let instruction = 0x9B008000
            | (src2.as_u32() << 16)
            | (addend.as_u32() << 10)
            | (src1.as_u32() << 5)
            | dst.as_u32();
        self.emit(instruction);
    }

    /// SDIV - Signed divide: dst = src1 / src2
    pub fn sdiv(&mut self, src1: Register, src2: Register, dst: Register) {
        let instruction = 0x9AC00C00 | (src2.as_u32() << 16) | (src1.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    // === Comparison Instructions ===

    /// CMP - Compare registers (alias for SUBS with XZR as destination): compare src1 - src2
    pub fn cmp(&mut self, src1: Register, src2: Register) {
        let instruction = 0xEB00001F | (src2.as_u32() << 16) | (src1.as_u32() << 5);
        self.emit(instruction);
    }

    /// CMP immediate - Compare register with 12-bit immediate
    pub fn cmp_imm(&mut self, src: Register, imm: u16) {
        assert!(imm <= 0xFFF, "Immediate value must be 12-bit");
        let instruction = 0xF100001F | ((imm as u32) << 10) | (src.as_u32() << 5);
        self.emit(instruction);
    }

    /// SUBS - Subtract and set flags: dst = src1 - src2
    pub fn subs(&mut self, src1: Register, src2: Register, dst: Register) {
        let instruction = 0xEB000000 | (src2.as_u32() << 16) | (src1.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// CSET - Conditional set: dst = (condition) ? 1 : 0
    pub fn cset(&mut self, dst: Register, condition: Condition) {
        self.csinc(XZR, XZR, condition.invert(), dst);
    }

    /// CSINC - Conditional select increment
    pub fn csinc(&mut self, src1: Register, src2: Register, condition: Condition, dst: Register) {
        let instruction = 0x9A800400
            | (condition.as_u32() << 12)
            | (src1.as_u32() << 16)
            | (src2.as_u32() << 5)
            | dst.as_u32();
        self.emit(instruction);
    }

    // === Memory Instructions ===

    /// LDR - Load register from memory [base + offset]
    pub fn ldr(&mut self, base: Register, offset: u16, dst: Register) {
        assert!(offset <= 0x1FF, "Offset must be 9-bit");
        let instruction =
            0xF9400000 | ((offset as u32) << 10) | (base.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// STR - Store register to memory [base + offset]
    pub fn str(&mut self, src: Register, base: Register, offset: u16) {
        assert!(offset <= 0x1FF, "Offset must be 9-bit");
        let instruction =
            0xF9000000 | ((offset as u32) << 10) | (base.as_u32() << 5) | src.as_u32();
        self.emit(instruction);
    }

    /// STP - Store pair pre-indexed: [base + offset]! = {src1, src2}
    pub fn stp_pre_index(&mut self, src1: Register, src2: Register, base: Register, offset: i8) {
        assert!(offset >= -64 && offset <= 63, "Offset must be 7-bit signed");
        let offset_bits = (offset as u32) & 0x7F;
        let instruction = 0xA9800000
            | (offset_bits << 15)
            | (src2.as_u32() << 10)
            | (base.as_u32() << 5)
            | src1.as_u32();
        self.emit(instruction);
    }

    /// LDP - Load pair post-indexed: {src1, src2} = [base], base += offset
    pub fn ldp_post_index(&mut self, dst1: Register, dst2: Register, base: Register, offset: i8) {
        assert!(offset >= -64 && offset <= 63, "Offset must be 7-bit signed");
        let offset_bits = (offset as u32) & 0x7F;
        let instruction = 0xA8C00000
            | (offset_bits << 15)
            | (dst2.as_u32() << 10)
            | (base.as_u32() << 5)
            | dst1.as_u32();
        self.emit(instruction);
    }

    // === Branch Instructions ===

    /// B - Unconditional branch
    pub fn b(&mut self, offset: i32) -> Result<()> {
        if offset < -0x2000000 || offset > 0x1FFFFFF {
            return Err(anyhow::anyhow!("Branch offset out of range: {}", offset));
        }
        let offset_bits = (offset as u32) & 0x3FFFFFF;
        let instruction = 0x14000000 | offset_bits;
        self.emit(instruction);
        Ok(())
    }

    /// BL - Branch with link (call)
    pub fn bl(&mut self, offset: i32) -> Result<()> {
        if offset < -0x2000000 || offset > 0x1FFFFFF {
            return Err(anyhow::anyhow!("Branch offset out of range: {}", offset));
        }
        let offset_bits = (offset as u32) & 0x3FFFFFF;
        let instruction = 0x94000000 | offset_bits;
        self.emit(instruction);
        Ok(())
    }

    /// BLR - Branch with link to register
    pub fn blr(&mut self, src: Register) {
        let instruction = 0xD63F0000 | (src.as_u32() << 5);
        self.emit(instruction);
    }

    /// CBZ - Compare and branch if zero
    pub fn cbz(&mut self, src: Register, offset: i32) -> Result<()> {
        if offset < -0x40000 || offset > 0x3FFFF {
            return Err(anyhow::anyhow!("CBZ offset out of range: {}", offset));
        }
        let offset_bits = (offset as u32) & 0x7FFFF;
        let instruction = 0xB4000000 | (offset_bits << 5) | src.as_u32();
        self.emit(instruction);
        Ok(())
    }

    /// CBNZ - Compare and branch if not zero
    pub fn cbnz(&mut self, src: Register, offset: i32) -> Result<()> {
        if offset < -0x40000 || offset > 0x3FFFF {
            return Err(anyhow::anyhow!("CBNZ offset out of range: {}", offset));
        }
        let offset_bits = (offset as u32) & 0x7FFFF;
        let instruction = 0xB5000000 | (offset_bits << 5) | src.as_u32();
        self.emit(instruction);
        Ok(())
    }

    /// Get B instruction encoding for relative branch
    pub fn get_b_instr(offset: i32) -> u32 {
        let offset_bits = (offset as u32) & 0x3FFFFFF;
        0x14000000 | offset_bits
    }

    /// Get BL instruction encoding for relative branch with link
    pub fn get_bl_instr(offset: i32) -> u32 {
        let offset_bits = (offset as u32) & 0x3FFFFFF;
        0x94000000 | offset_bits
    }

    /// Get CBZ offset encoding for conditional branch if zero
    pub fn get_cbz_offset(offset: i32) -> u32 {
        let offset_bits = (offset as u32) & 0x7FFFF;
        offset_bits << 5
    }

    // === Special/Convenience Methods ===

    /// Generate prologue for function entry
    pub fn function_prologue(&mut self) {
        // stp x29, x30, [sp, #-16]!
        self.stp_pre_index(FP, LR, SP, -16);
    }

    /// Generate epilogue for function exit
    pub fn function_epilogue(&mut self) {
        // ldp x29, x30, [sp], #16
        self.ldp_post_index(FP, LR, SP, 16);
    }

    /// Placeholder instruction (will be patched later)
    pub fn placeholder(&mut self) -> usize {
        let pos = self.position();
        self.emit(0x00000000); // NOP equivalent
        pos
    }

    /// LSL immediate - Logical shift left by immediate
    pub fn lsl_imm(&mut self, src: Register, dst: Register, shift: u8) {
        // LSL is an alias for UBFM with specific parameters
        // UBFM Xd, Xn, #(-shift MOD 64), #(63-shift)
        let immr = (64 - shift as u32) % 64;
        let imms = 63 - shift as u32;
        let instruction =
            0xD3400000 | (immr << 16) | (imms << 10) | (src.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// LSR immediate - Logical shift right by immediate
    pub fn lsr_imm(&mut self, src: Register, dst: Register, shift: u8) {
        // LSR is an alias for UBFM with specific parameters
        // UBFM Xd, Xn, #shift, #63
        let immr = shift as u32;
        let imms = 63;
        let instruction =
            0xD3400000 | (immr << 16) | (imms << 10) | (src.as_u32() << 5) | dst.as_u32();
        self.emit(instruction);
    }

    /// Patch a placeholder instruction with actual instruction
    pub fn patch(&mut self, position: usize, instruction: u32) {
        assert!(position < self.code.len(), "Invalid patch position");
        self.code[position] = instruction;
    }

    // === JIT Stack Operations ===

    /// Push value from register to VM stack
    /// Arguments: src_reg (register containing value to push), stack_reg (C stack pointer), sp_reg (C SP pointer), temp_reg1, temp_reg2 (temporary registers)
    pub fn push_to_stack(&mut self, src_reg: Register, stack_reg: Register, sp_reg: Register, temp_reg1: Register, temp_reg2: Register) {
        // Load current SP: *sp_reg
        self.ldr(sp_reg, 0, temp_reg1);

        // Calculate stack address: stack_reg + (SP * 8)
        self.mov_imm(temp_reg2, 8);
        self.mul(temp_reg1, temp_reg2, temp_reg2);
        self.add_reg(stack_reg, temp_reg2, temp_reg2);

        // Store value to stack: [stack_reg + (SP * 8)] = src_reg
        self.str(src_reg, temp_reg2, 0);

        // Increment SP: *sp_reg += 1
        self.add_imm(temp_reg1, 1, temp_reg1);
        self.str(temp_reg1, sp_reg, 0);
    }

    /// Pop value from VM stack to register
    /// Arguments: dst_reg (register to store popped value), stack_reg (C stack pointer), sp_reg (C SP pointer), temp_reg1, temp_reg2 (temporary registers)
    pub fn pop_from_stack(&mut self, dst_reg: Register, stack_reg: Register, sp_reg: Register, temp_reg1: Register, temp_reg2: Register) {
        // Load current SP: *sp_reg
        self.ldr(sp_reg, 0, temp_reg1);

        // Decrement SP: *sp_reg -= 1
        self.sub_imm(temp_reg1, 1, temp_reg1);
        self.str(temp_reg1, sp_reg, 0);

        // Calculate stack address: stack_reg + (SP * 8)
        self.mov_imm(temp_reg2, 8);
        self.mul(temp_reg1, temp_reg2, temp_reg2);
        self.add_reg(stack_reg, temp_reg2, temp_reg2);

        // Load value from stack: dst_reg = [stack_reg + (SP * 8)]
        self.ldr(temp_reg2, 0, dst_reg);
    }

    /// Set POINTER_BIT (1u64 << 63) for pointer encoding
    /// Arguments: reg (register to modify), temp_reg (temporary register)
    pub fn set_pointer_bit(&mut self, reg: Register, temp_reg: Register) {
        // Set POINTER_BIT (1u64 << 63) to encode as pointer
        self.mov_imm(temp_reg, 0x8000);
        self.lsl_imm(temp_reg, temp_reg, 48); // Shift to bit 63 (0x8000 << 48 = 1u64 << 63)
        self.add_reg(reg, temp_reg, reg); // reg += POINTER_BIT (same as |= since MSB is 0)
    }

    /// Clear POINTER_BIT (remove MSB) for address extraction
    /// Arguments: reg (register to modify)
    pub fn clear_pointer_bit(&mut self, reg: Register) {
        // Clear POINTER_BIT (remove MSB) to get actual address
        // Use LSL/LSR trick: shift left 1 bit, then right 1 bit
        self.lsl_imm(reg, reg, 1); // Left shift by 1 (removes MSB)
        self.lsr_imm(reg, reg, 1); // Right shift by 1 (restores position, MSB=0)
    }
}

impl Default for ARM64CodeGen {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_encoding() {
        assert_eq!(Register::X0.as_u32(), 0);
        assert_eq!(Register::X15.as_u32(), 15);
        assert_eq!(Register::X31.as_u32(), 31);
    }

    #[test]
    fn test_condition_invert() {
        assert_eq!(Condition::EQ.invert(), Condition::NE);
        assert_eq!(Condition::LT.invert(), Condition::GE);
        assert_eq!(Condition::GT.invert(), Condition::LE);
    }

    #[test]
    fn test_basic_instructions() {
        let mut gen = ARM64CodeGen::new();

        // Test MOV immediate
        gen.mov_imm(Register::X0, 42);
        assert_eq!(gen.code.last(), Some(&0xD2800540)); // mov x0, #42

        // Test ADD registers
        gen.add_reg(Register::X0, Register::X1, Register::X2);
        assert_eq!(gen.code.last(), Some(&0x8B010002)); // add x2, x0, x1

        // Test RET
        gen.ret();
        assert_eq!(gen.code.last(), Some(&0xd65f03c0)); // ret
    }

    #[test]
    fn test_function_prologue_epilogue() {
        let mut gen = ARM64CodeGen::new();

        gen.function_prologue();
        assert_eq!(gen.code.len(), 1);

        gen.function_epilogue();
        assert_eq!(gen.code.len(), 2); // prologue + ldp
    }

    #[test]
    fn test_finalize() {
        let mut gen = ARM64CodeGen::new();
        gen.mov_imm(Register::X0, 42);
        gen.ret();

        let bytes = gen.finalize();
        assert_eq!(bytes.len(), 8); // 2 instructions * 4 bytes each
    }

    #[test]
    fn test_ldr_instruction_encoding() {
        let mut gen = ARM64CodeGen::new();

        // Test LDR x9, [x11] (offset = 0)
        gen.ldr(Register::X11, 0, Register::X9);

        let machine_code = gen.finalize();

        // Expected encoding for LDR x9, [x11]:
        // 0xF9400000 (base) + (0 << 10) + (11 << 5) + 9 = 0xF9400169
        // In little-endian: [0x69, 0x01, 0x40, 0xF9]
        let expected = vec![0x69, 0x01, 0x40, 0xF9];

        assert_eq!(
            machine_code, expected,
            "LDR x9, [x11] should generate correct offset addressing mode instruction"
        );
    }

    #[test]
    fn test_arithmetic_operand_order() {
        let mut gen = ARM64CodeGen::new();

        // Test SUB x9, x10, x11 → x9 = x10 - x11
        gen.sub_reg(Register::X10, Register::X11, Register::X9);
        let sub_instruction = *gen.code.last().unwrap();
        // Expected: 0xCB000000 | (x11 << 16) | (x10 << 5) | x9
        // = 0xCB000000 | (11 << 16) | (10 << 5) | 9
        // = 0xCB000000 | 0xB0000 | 0x140 | 9 = 0xCB0B0149
        assert_eq!(
            sub_instruction, 0xCB0B0149,
            "SUB should encode operands correctly"
        );

        // Test SDIV x9, x10, x11 → x9 = x10 / x11
        gen.sdiv(Register::X10, Register::X11, Register::X9);
        let div_instruction = *gen.code.last().unwrap();
        // Expected: 0x9AC00C00 | (x11 << 16) | (x10 << 5) | x9
        // = 0x9AC00C00 | (11 << 16) | (10 << 5) | 9
        // = 0x9AC00C00 | 0xB0000 | 0x140 | 9 = 0x9ACB0D49
        assert_eq!(
            div_instruction, 0x9ACB0D49,
            "SDIV should encode operands correctly"
        );
    }
}
