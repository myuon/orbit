use crate::vm::Instruction;
use std::collections::HashMap;

pub struct LabelResolver {
    /// Map from label name to instruction index
    label_positions: HashMap<String, usize>,
}

impl LabelResolver {
    pub fn new() -> Self {
        Self {
            label_positions: HashMap::new(),
        }
    }

    /// Resolve labels in the instruction list, converting Label instructions to Nop
    /// and replacing Call/Jump instructions with their relative offset equivalents
    pub fn resolve_labels(
        &mut self,
        instructions: Vec<Instruction>,
    ) -> Result<Vec<Instruction>, String> {
        // First pass: collect all label positions
        self.collect_labels(&instructions);

        // Second pass: replace instructions
        self.replace_instructions(instructions)
    }

    /// First pass: scan through instructions and record label positions
    fn collect_labels(&mut self, instructions: &[Instruction]) {
        self.label_positions.clear();

        for (index, instruction) in instructions.iter().enumerate() {
            if let Instruction::Label(label_name) = instruction {
                self.label_positions.insert(label_name.clone(), index);
            }
        }
    }

    /// Second pass: replace Call/Jump instructions and convert Labels to Nop
    fn replace_instructions(
        &self,
        instructions: Vec<Instruction>,
    ) -> Result<Vec<Instruction>, String> {
        let mut resolved_instructions = Vec::new();

        for (current_index, instruction) in instructions.into_iter().enumerate() {
            match instruction {
                // Convert Label instructions to Nop
                Instruction::Label(_) => {
                    resolved_instructions.push(Instruction::Nop);
                }

                // Convert Call(label) to CallRel(offset)
                Instruction::Call(label_name) => {
                    if let Some(&target_index) = self.label_positions.get(&label_name) {
                        let relative_offset = target_index as i32 - current_index as i32;
                        resolved_instructions.push(Instruction::CallRel(relative_offset));
                    } else {
                        return Err(format!("Unresolved label in Call: {}", label_name));
                    }
                }

                // Convert Jump(absolute) to JumpRel(offset) if it references a label
                // For now, keep Jump(absolute) as-is since it might be direct address jumps
                Instruction::Jump(target) => {
                    resolved_instructions.push(Instruction::Jump(target));
                }

                // Keep all other instructions unchanged
                _ => {
                    resolved_instructions.push(instruction);
                }
            }
        }

        Ok(resolved_instructions)
    }
}
