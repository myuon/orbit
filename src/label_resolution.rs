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

                // Convert Jump(label) to JumpRel(offset)
                Instruction::Jump(label_name) => {
                    if let Some(&target_index) = self.label_positions.get(&label_name) {
                        let relative_offset = target_index as i32 - current_index as i32;
                        resolved_instructions.push(Instruction::JumpRel(relative_offset));
                    } else {
                        return Err(format!("Unresolved label in Jump: {}", label_name));
                    }
                }

                // Convert JumpIfZero(label) to JumpIfZeroRel(offset)
                Instruction::JumpIfZero(label_name) => {
                    if let Some(&target_index) = self.label_positions.get(&label_name) {
                        let relative_offset = target_index as i32 - current_index as i32;
                        resolved_instructions.push(Instruction::JumpIfZeroRel(relative_offset));
                    } else {
                        return Err(format!("Unresolved label in JumpIfZero: {}", label_name));
                    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_label_resolution_for_jump() {
        let mut resolver = LabelResolver::new();

        let instructions = vec![
            Instruction::Push(1),
            Instruction::Jump("target".to_string()),
            Instruction::Push(2),
            Instruction::Label("target".to_string()),
            Instruction::Push(3),
        ];

        let resolved = resolver.resolve_labels(instructions).unwrap();

        // Check that Jump was converted to JumpRel with correct offset
        // Jump from index 1 to index 3, so offset = 3 - 1 = 2
        assert_eq!(resolved[1], Instruction::JumpRel(2));

        // Check that Label was converted to Nop
        assert_eq!(resolved[3], Instruction::Nop);
    }

    #[test]
    fn test_label_resolution_for_jump_if_zero() {
        let mut resolver = LabelResolver::new();

        let instructions = vec![
            Instruction::Push(0),
            Instruction::JumpIfZero("end".to_string()),
            Instruction::Push(2),
            Instruction::Label("end".to_string()),
            Instruction::Push(3),
        ];

        let resolved = resolver.resolve_labels(instructions).unwrap();

        // Check that JumpIfZero was converted to JumpIfZeroRel with correct offset
        // JumpIfZero from index 1 to index 3, so offset = 3 - 1 = 2
        assert_eq!(resolved[1], Instruction::JumpIfZeroRel(2));

        // Check that Label was converted to Nop
        assert_eq!(resolved[3], Instruction::Nop);
    }

    #[test]
    fn test_label_resolution_for_call() {
        let mut resolver = LabelResolver::new();

        let instructions = vec![
            Instruction::Push(1),
            Instruction::Call("function".to_string()),
            Instruction::Push(2),
            Instruction::Label("function".to_string()),
            Instruction::Push(3),
        ];

        let resolved = resolver.resolve_labels(instructions).unwrap();

        // Check that Call was converted to CallRel with correct offset
        // Call from index 1 to index 3, so offset = 3 - 1 = 2
        assert_eq!(resolved[1], Instruction::CallRel(2));

        // Check that Label was converted to Nop
        assert_eq!(resolved[3], Instruction::Nop);
    }

    #[test]
    fn test_unresolved_label_error() {
        let mut resolver = LabelResolver::new();

        let instructions = vec![
            Instruction::Push(1),
            Instruction::Jump("nonexistent".to_string()),
            Instruction::Push(2),
        ];

        let result = resolver.resolve_labels(instructions);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("Unresolved label in Jump: nonexistent"));
    }
}
