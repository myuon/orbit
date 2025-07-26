use orbit::runtime::{VM, Value};
use orbit::vm::Instruction;

fn main() {
    let mut vm = VM::new();
    
    // Test GetHP -> Push(3) -> AddressAdd sequence
    vm.load_program(vec![
        Instruction::GetHP,      // Should push Address(0)
        Instruction::Push(3),    // Should push Int(3)
        // Let's not do AddressAdd yet, just see what's on the stack
    ]);
    
    // Execute GetHP
    vm.step().unwrap();
    println!("After GetHP: {:?}", vm.get_stack());
    
    // Execute Push(3)
    vm.step().unwrap();  
    println!("After Push(3): {:?}", vm.get_stack());
}