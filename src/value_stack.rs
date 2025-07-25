use crate::runtime::Value;
use crate::value_encoding::ValueEncoder;

/// A wrapper around Vec<u64> that provides Value-based API
/// for stack operations while internally storing encoded values
#[derive(Debug, Clone)]
pub struct ValueStack {
    /// Internal encoded stack storage
    encoded_stack: Vec<u64>,
}

impl ValueStack {
    /// Create a new empty value stack
    pub fn new() -> Self {
        ValueStack {
            encoded_stack: Vec::new(),
        }
    }

    /// Create a new value stack with specified capacity
    pub fn with_capacity(capacity: usize) -> Self {
        ValueStack {
            encoded_stack: Vec::with_capacity(capacity),
        }
    }

    /// Push a Value onto the stack (encodes automatically)
    pub fn push(&mut self, value: Value) {
        let encoded = ValueEncoder::encode(&value);
        self.encoded_stack.push(encoded);
    }

    /// Pop a Value from the stack (decodes automatically)
    pub fn pop(&mut self) -> Option<Value> {
        self.encoded_stack.pop().map(|encoded| ValueEncoder::decode(encoded))
    }

    /// Peek at the top value without removing it
    pub fn peek(&self) -> Option<Value> {
        self.encoded_stack.last().map(|&encoded| ValueEncoder::decode(encoded))
    }

    /// Get the length of the stack
    pub fn len(&self) -> usize {
        self.encoded_stack.len()
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.encoded_stack.is_empty()
    }

    /// Clear all values from the stack
    pub fn clear(&mut self) {
        self.encoded_stack.clear();
    }

    /// Get a value at a specific index from the bottom (does not remove)
    pub fn get(&self, index: usize) -> Option<Value> {
        self.encoded_stack.get(index).map(|&encoded| ValueEncoder::decode(encoded))
    }

    /// Set a value at a specific index from the bottom
    pub fn set(&mut self, index: usize, value: Value) -> Result<(), String> {
        if index >= self.encoded_stack.len() {
            return Err(format!("Index {} out of bounds for stack of length {}", index, self.encoded_stack.len()));
        }
        let encoded = ValueEncoder::encode(&value);
        self.encoded_stack[index] = encoded;
        Ok(())
    }

    /// Get the last n values from the stack (does not remove them)
    pub fn last_n(&self, n: usize) -> Vec<Value> {
        let start = if n > self.encoded_stack.len() {
            0
        } else {
            self.encoded_stack.len() - n
        };
        
        self.encoded_stack[start..]
            .iter()
            .map(|&encoded| ValueEncoder::decode(encoded))
            .collect()
    }

    /// Remove the last n values from the stack
    pub fn truncate(&mut self, new_len: usize) {
        self.encoded_stack.truncate(new_len);
    }

    /// Resize the stack to the specified length, filling with the given value
    pub fn resize(&mut self, new_len: usize, fill_value: Value) {
        let encoded_fill = ValueEncoder::encode(&fill_value);
        self.encoded_stack.resize(new_len, encoded_fill);
    }

    /// Get direct access to the underlying encoded stack for JIT compilation
    pub fn as_encoded_vec(&self) -> &Vec<u64> {
        &self.encoded_stack
    }

    /// Get mutable access to the underlying encoded stack for JIT compilation
    pub fn as_encoded_vec_mut(&mut self) -> &mut Vec<u64> {
        &mut self.encoded_stack
    }

    /// Push an already encoded value directly (for JIT use)
    pub fn push_encoded(&mut self, encoded: u64) {
        self.encoded_stack.push(encoded);
    }

    /// Pop an encoded value directly (for JIT use)
    pub fn pop_encoded(&mut self) -> Option<u64> {
        self.encoded_stack.pop()
    }

    /// Get an encoded value at a specific index (for JIT use)
    pub fn get_encoded(&self, index: usize) -> Option<u64> {
        self.encoded_stack.get(index).copied()
    }

    /// Set an encoded value at a specific index (for JIT use)
    pub fn set_encoded(&mut self, index: usize, encoded: u64) -> Result<(), String> {
        if index >= self.encoded_stack.len() {
            return Err(format!("Index {} out of bounds for stack of length {}", index, self.encoded_stack.len()));
        }
        self.encoded_stack[index] = encoded;
        Ok(())
    }

    /// Create iterator over Values (decoding on the fly)
    pub fn iter(&self) -> impl Iterator<Item = Value> + '_ {
        self.encoded_stack.iter().map(|&encoded| ValueEncoder::decode(encoded))
    }

    /// Create iterator over encoded values
    pub fn iter_encoded(&self) -> impl Iterator<Item = u64> + '_ {
        self.encoded_stack.iter().copied()
    }
}

impl Default for ValueStack {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<Value>> for ValueStack {
    fn from(values: Vec<Value>) -> Self {
        let encoded_stack = values.iter().map(|v| ValueEncoder::encode(v)).collect();
        ValueStack { encoded_stack }
    }
}

impl From<ValueStack> for Vec<Value> {
    fn from(stack: ValueStack) -> Self {
        stack.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{HeapIndex};

    #[test]
    fn test_push_pop() {
        let mut stack = ValueStack::new();
        
        stack.push(Value::Int(42));
        stack.push(Value::Boolean(true));
        
        assert_eq!(stack.len(), 2);
        assert_eq!(stack.pop(), Some(Value::Int(1))); // Boolean(true) decodes as Int(1)
        assert_eq!(stack.pop(), Some(Value::Int(42)));
        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_peek() {
        let mut stack = ValueStack::new();
        
        assert_eq!(stack.peek(), None);
        
        stack.push(Value::Int(100));
        assert_eq!(stack.peek(), Some(Value::Int(100)));
        assert_eq!(stack.len(), 1); // peek doesn't remove
        
        assert_eq!(stack.pop(), Some(Value::Int(100)));
        assert_eq!(stack.peek(), None);
    }

    #[test]
    fn test_get_set() {
        let mut stack = ValueStack::new();
        
        stack.push(Value::Int(1));
        stack.push(Value::Int(2));
        stack.push(Value::Int(3));
        
        assert_eq!(stack.get(0), Some(Value::Int(1)));
        assert_eq!(stack.get(1), Some(Value::Int(2)));
        assert_eq!(stack.get(2), Some(Value::Int(3)));
        assert_eq!(stack.get(3), None);
        
        stack.set(1, Value::Int(20)).unwrap();
        assert_eq!(stack.get(1), Some(Value::Int(20)));
    }

    #[test]
    fn test_encoded_operations() {
        let mut stack = ValueStack::new();
        
        let encoded_42 = ValueEncoder::encode(&Value::Int(42));
        stack.push_encoded(encoded_42);
        
        assert_eq!(stack.get_encoded(0), Some(encoded_42));
        assert_eq!(stack.pop_encoded(), Some(encoded_42));
        assert_eq!(stack.len(), 0);
    }

    #[test]
    fn test_heap_ref() {
        let mut stack = ValueStack::new();
        
        stack.push(Value::HeapRef(HeapIndex(10)));
        assert_eq!(stack.pop(), Some(Value::HeapRef(HeapIndex(10))));
    }

    #[test]
    fn test_address() {
        let mut stack = ValueStack::new();
        
        stack.push(Value::Address(0x1000));
        assert_eq!(stack.pop(), Some(Value::Address(0x1000)));
    }

    #[test]
    fn test_from_vec() {
        let values = vec![Value::Int(1), Value::Int(2), Value::Int(3)];
        let stack = ValueStack::from(values.clone());
        
        assert_eq!(stack.len(), 3);
        assert_eq!(stack.get(0), Some(Value::Int(1)));
        assert_eq!(stack.get(1), Some(Value::Int(2)));
        assert_eq!(stack.get(2), Some(Value::Int(3)));
    }

    #[test]
    fn test_to_vec() {
        let mut stack = ValueStack::new();
        stack.push(Value::Int(1));
        stack.push(Value::Int(2));
        stack.push(Value::Int(3));
        
        let values: Vec<Value> = stack.into();
        assert_eq!(values, vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    }
}