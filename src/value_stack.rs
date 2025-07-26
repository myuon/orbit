use crate::runtime::Value;
use crate::value_encoding::ValueEncoder;

/// A fixed-size stack that provides Value-based API
/// for stack operations while internally storing encoded values
#[derive(Debug, Clone)]
pub struct ValueStack {
    /// Internal encoded stack storage (fixed-size array)
    encoded_stack: Box<[u64]>,
    /// Current length of the stack
    len: usize,
}

impl ValueStack {
    /// Create a new empty value stack (with default capacity)
    pub fn new() -> Self {
        Self::with_fixed_capacity(1024)
    }

    /// Create a new value stack with specified capacity (deprecated, use with_fixed_capacity)
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_fixed_capacity(capacity)
    }

    /// Create a new value stack with pre-allocated fixed capacity
    /// This is the preferred constructor for VM stack usage
    pub fn with_fixed_capacity(capacity: usize) -> Self {
        let encoded_stack = vec![0u64; capacity].into_boxed_slice();
        ValueStack {
            encoded_stack,
            len: 0,
        }
    }

    /// Push a Value onto the stack (encodes automatically)
    /// DEPRECATED: Use sp-based operations instead for VM stack
    pub fn push(&mut self, value: Value) {
        if self.len >= self.encoded_stack.len() {
            panic!("Stack overflow: capacity exceeded");
        }
        let encoded = ValueEncoder::encode(&value);
        self.encoded_stack[self.len] = encoded;
        self.len += 1;
    }

    /// Pop a Value from the stack (decodes automatically)
    /// DEPRECATED: Use sp-based operations instead for VM stack
    pub fn pop(&mut self) -> Option<Value> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        let encoded = self.encoded_stack[self.len];
        Some(ValueEncoder::decode(encoded))
    }

    /// Set a value at the sp position and increment sp (stack push via sp)
    pub fn push_at_sp(&mut self, value: Value, sp: &mut usize) -> Result<(), String> {
        if *sp >= self.encoded_stack.len() {
            return Err(format!(
                "Stack overflow: SP {} exceeds capacity {}",
                *sp,
                self.encoded_stack.len()
            ));
        }

        let encoded = ValueEncoder::encode(&value);
        self.encoded_stack[*sp] = encoded;
        *sp += 1;

        // Update length to track the highest used position
        if *sp > self.len {
            self.len = *sp;
        }

        Ok(())
    }

    /// Get value at sp-1 position and decrement sp (stack pop via sp)
    pub fn pop_at_sp(&mut self, sp: &mut usize) -> Result<Value, String> {
        if *sp == 0 {
            return Err("Stack underflow: SP is 0".to_string());
        }

        if *sp >= self.encoded_stack.len() {
            return Err(format!(
                "Stack access out of bounds: SP {} >= capacity {}",
                *sp,
                self.encoded_stack.len()
            ));
        }

        let encoded = self.encoded_stack[*sp];

        *sp -= 1;

        Ok(ValueEncoder::decode(encoded))
    }

    /// Peek at the top value without removing it
    pub fn peek(&self) -> Option<Value> {
        if self.len == 0 {
            return None;
        }
        let encoded = self.encoded_stack[self.len - 1];
        Some(ValueEncoder::decode(encoded))
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Clear all values from the stack
    pub fn clear(&mut self) {
        self.len = 0;
    }

    /// Get a value at a specific index from the bottom (does not remove)
    pub fn get(&self, index: usize) -> Option<Value> {
        if index >= self.len {
            return None;
        }
        let encoded = self.encoded_stack[index];
        Some(ValueEncoder::decode(encoded))
    }

    /// Set a value at a specific index from the bottom
    pub fn set(&mut self, index: usize, value: Value) -> Result<(), String> {
        if index >= self.encoded_stack.len() {
            return Err(format!(
                "Index {} out of bounds for stack capacity {}",
                index,
                self.encoded_stack.len()
            ));
        }
        let encoded = ValueEncoder::encode(&value);
        self.encoded_stack[index] = encoded;

        // Update length if we're setting beyond current length
        if index >= self.len {
            self.len = index + 1;
        }

        Ok(())
    }

    /// Get the last n values from the stack (does not remove them)
    pub fn last_n(&self, n: usize) -> Vec<Value> {
        let start = if n > self.len { 0 } else { self.len - n };

        self.encoded_stack[start..self.len]
            .iter()
            .map(|&encoded| ValueEncoder::decode(encoded))
            .collect()
    }

    /// Remove the last n values from the stack
    pub fn truncate(&mut self, new_len: usize) {
        if new_len < self.len {
            self.len = new_len;
        }
    }

    /// Resize the stack to the specified length, filling with the given value
    pub fn resize(&mut self, new_len: usize, fill_value: Value) {
        if new_len > self.encoded_stack.len() {
            panic!(
                "Cannot resize beyond capacity: {} > {}",
                new_len,
                self.encoded_stack.len()
            );
        }

        let encoded_fill = ValueEncoder::encode(&fill_value);

        // Fill any new slots with the fill value
        for i in self.len..new_len {
            self.encoded_stack[i] = encoded_fill;
        }

        self.len = new_len;
    }

    /// Get direct access to the underlying encoded stack for JIT compilation
    pub fn as_encoded_slice(&self) -> &[u64] {
        &self.encoded_stack[..self.len]
    }

    /// Get mutable access to the underlying encoded stack for JIT compilation
    pub fn as_encoded_slice_mut(&mut self) -> &mut [u64] {
        &mut self.encoded_stack[..self.len]
    }

    /// Push an already encoded value directly (for JIT use)
    pub fn push_encoded(&mut self, encoded: u64) {
        if self.len >= self.encoded_stack.len() {
            panic!("Stack overflow: capacity exceeded");
        }
        self.encoded_stack[self.len] = encoded;
        self.len += 1;
    }

    /// Pop an encoded value directly (for JIT use)
    pub fn pop_encoded(&mut self) -> Option<u64> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        Some(self.encoded_stack[self.len])
    }

    /// Get an encoded value at a specific index (for JIT use)
    pub fn get_encoded(&self, index: usize) -> Option<u64> {
        if index >= self.len {
            return None;
        }
        Some(self.encoded_stack[index])
    }

    /// Set an encoded value at a specific index (for JIT use)
    pub fn set_encoded(&mut self, index: usize, encoded: u64) -> Result<(), String> {
        if index >= self.encoded_stack.len() {
            return Err(format!(
                "Index {} out of bounds for stack capacity {}",
                index,
                self.encoded_stack.len()
            ));
        }
        self.encoded_stack[index] = encoded;

        // Update length if we're setting beyond current length
        if index >= self.len {
            self.len = index + 1;
        }

        Ok(())
    }

    /// Create iterator over Values (decoding on the fly)
    pub fn iter(&self) -> impl Iterator<Item = Value> + '_ {
        self.encoded_stack[..self.len]
            .iter()
            .map(|&encoded| ValueEncoder::decode(encoded))
    }

    /// Create iterator over encoded values
    pub fn iter_encoded(&self) -> impl Iterator<Item = u64> + '_ {
        self.encoded_stack[..self.len].iter().copied()
    }
}

impl Default for ValueStack {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<Value>> for ValueStack {
    fn from(values: Vec<Value>) -> Self {
        let capacity = values.len().max(1024); // Use at least 1024 capacity
        let mut stack = Self::with_fixed_capacity(capacity);
        for value in values {
            stack.push(value);
        }
        stack
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
    use crate::runtime::HeapIndex;

    #[test]
    fn test_push_pop() {
        let mut stack = ValueStack::new();

        stack.push(Value::Int(42));
        stack.push(Value::Boolean(true));

        assert_eq!(stack.len, 2);
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
        assert_eq!(stack.len, 1); // peek doesn't remove

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
        assert_eq!(stack.len, 0);
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

        assert_eq!(stack.len, 3);
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
