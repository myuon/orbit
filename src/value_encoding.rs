use crate::runtime::{HeapIndex, Value};

/// Value encoding for JIT compilation
/// 
/// Encoding scheme using 64-bit integers:
/// - LSB (bit 0): 0 = numeric value, 1 = pointer
/// - For numeric values (LSB = 0): remaining 63 bits store the value
///   - Int, Boolean, Byte are all stored as i63 values
/// - For pointer values (LSB = 1):
///   - Bit 1: 0 = Address, 1 = HeapRef
///   - Remaining 62 bits store the address/index
/// 
/// Examples:
/// - Int(42) -> 0x0000000000000054 (42 << 1 | 0)
/// - Boolean(true) -> 0x0000000000000002 (1 << 1 | 0)
/// - Byte(255) -> 0x00000000000001FE (255 << 1 | 0)
/// - Address(0x1000) -> 0x0000000000002001 (0x1000 << 2 | 0 << 1 | 1)
/// - HeapRef(5) -> 0x0000000000000017 (5 << 2 | 1 << 1 | 1)

const POINTER_BIT: u64 = 1;
const HEAP_REF_BIT: u64 = 2;

pub struct ValueEncoder;

impl ValueEncoder {
    /// Encode a Value into a u64 for JIT compilation
    pub fn encode(value: &Value) -> u64 {
        match value {
            Value::Int(n) => {
                // Store as i63: shift left by 1, LSB = 0
                // Use wrapping operations to handle negative numbers correctly
                ((*n as u64).wrapping_shl(1)) & !POINTER_BIT
            }
            Value::Boolean(b) => {
                // Store as i63: shift left by 1, LSB = 0
                ((*b as u64) << 1) & !POINTER_BIT
            }
            Value::Byte(b) => {
                // Store as i63: shift left by 1, LSB = 0
                ((*b as u64) << 1) & !POINTER_BIT
            }
            Value::Address(addr) => {
                // LSB = 1 (pointer), bit 1 = 0 (address), remaining bits = address
                (*addr as u64) << 2 | POINTER_BIT
            }
            Value::HeapRef(HeapIndex(index)) => {
                // LSB = 1 (pointer), bit 1 = 1 (heap ref), remaining bits = index
                (*index as u64) << 2 | HEAP_REF_BIT | POINTER_BIT
            }
        }
    }

    /// Decode a u64 back into a Value
    pub fn decode(encoded: u64) -> Value {
        if (encoded & POINTER_BIT) == 0 {
            // Numeric value: shift right by 1 to get original value
            // Use arithmetic right shift to preserve sign
            let numeric_value = ((encoded as i64) >> 1);
            // We can't distinguish between Int, Boolean, and Byte from encoding
            // Default to Int for now (this is the accepted limitation)
            Value::Int(numeric_value)
        } else {
            // Pointer value: check bit 1 to distinguish Address vs HeapRef
            if (encoded & HEAP_REF_BIT) == 0 {
                // Address: extract from bits 2 and up
                let addr = (encoded >> 2) as usize;
                Value::Address(addr)
            } else {
                // HeapRef: extract from bits 2 and up
                let index = (encoded >> 2) as usize;
                Value::HeapRef(HeapIndex(index))
            }
        }
    }

    /// Check if an encoded value represents a pointer
    pub fn is_pointer(encoded: u64) -> bool {
        (encoded & POINTER_BIT) != 0
    }

    /// Check if an encoded value represents a heap reference
    pub fn is_heap_ref(encoded: u64) -> bool {
        (encoded & POINTER_BIT) != 0 && (encoded & HEAP_REF_BIT) != 0
    }

    /// Check if an encoded value represents an address
    pub fn is_address(encoded: u64) -> bool {
        (encoded & POINTER_BIT) != 0 && (encoded & HEAP_REF_BIT) == 0
    }

    /// Get the numeric value from an encoded u64 (assumes it's numeric)
    pub fn get_numeric_value(encoded: u64) -> i64 {
        ((encoded as i64) >> 1)
    }

    /// Get the address from an encoded u64 (assumes it's an address)
    pub fn get_address(encoded: u64) -> usize {
        (encoded >> 2) as usize
    }

    /// Get the heap index from an encoded u64 (assumes it's a heap ref)
    pub fn get_heap_index(encoded: u64) -> usize {
        (encoded >> 2) as usize
    }

    /// Create an encoded integer value
    pub fn encode_int(value: i64) -> u64 {
        (value as u64).wrapping_shl(1)
    }

    /// Create an encoded boolean value
    pub fn encode_bool(value: bool) -> u64 {
        (value as u64) << 1
    }

    /// Create an encoded byte value
    pub fn encode_byte(value: u8) -> u64 {
        (value as u64) << 1
    }

    /// Create an encoded address value
    pub fn encode_address(addr: usize) -> u64 {
        (addr as u64) << 2 | POINTER_BIT
    }

    /// Create an encoded heap reference value
    pub fn encode_heap_ref(index: usize) -> u64 {
        (index as u64) << 2 | HEAP_REF_BIT | POINTER_BIT
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode_int() {
        let original = Value::Int(42);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        assert_eq!(decoded, Value::Int(42));
        assert!(!ValueEncoder::is_pointer(encoded));
    }

    #[test]
    fn test_encode_decode_boolean() {
        let original = Value::Boolean(true);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        // Boolean will be decoded as Int due to limitation
        assert_eq!(decoded, Value::Int(1));
        assert!(!ValueEncoder::is_pointer(encoded));
    }

    #[test]
    fn test_encode_decode_byte() {
        let original = Value::Byte(255);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        // Byte will be decoded as Int due to limitation
        assert_eq!(decoded, Value::Int(255));
        assert!(!ValueEncoder::is_pointer(encoded));
    }

    #[test]
    fn test_encode_decode_address() {
        let original = Value::Address(0x1000);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        assert_eq!(decoded, Value::Address(0x1000));
        assert!(ValueEncoder::is_pointer(encoded));
        assert!(ValueEncoder::is_address(encoded));
        assert!(!ValueEncoder::is_heap_ref(encoded));
    }

    #[test]
    fn test_encode_decode_heap_ref() {
        let original = Value::HeapRef(HeapIndex(42));
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        assert_eq!(decoded, Value::HeapRef(HeapIndex(42)));
        assert!(ValueEncoder::is_pointer(encoded));
        assert!(!ValueEncoder::is_address(encoded));
        assert!(ValueEncoder::is_heap_ref(encoded));
    }

    #[test]
    fn test_negative_integers() {
        let original = Value::Int(-42);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        assert_eq!(decoded, Value::Int(-42));
        assert!(!ValueEncoder::is_pointer(encoded));
    }

    #[test]
    fn test_large_values() {
        // Test maximum i63 value
        let max_i63 = (1i64 << 62) - 1;
        let original = Value::Int(max_i63);
        let encoded = ValueEncoder::encode(&original);
        let decoded = ValueEncoder::decode(encoded);
        
        assert_eq!(decoded, Value::Int(max_i63));
    }

    #[test]
    fn test_utility_functions() {
        let int_encoded = ValueEncoder::encode_int(100);
        assert_eq!(ValueEncoder::get_numeric_value(int_encoded), 100);
        
        let addr_encoded = ValueEncoder::encode_address(0x2000);
        assert_eq!(ValueEncoder::get_address(addr_encoded), 0x2000);
        
        let heap_encoded = ValueEncoder::encode_heap_ref(15);
        assert_eq!(ValueEncoder::get_heap_index(heap_encoded), 15);
    }

    #[test]
    fn test_bit_patterns() {
        // Test specific bit patterns to ensure correct encoding
        let int_encoded = ValueEncoder::encode_int(0);
        assert_eq!(int_encoded, 0x0000000000000000);
        
        let bool_true_encoded = ValueEncoder::encode_bool(true);
        assert_eq!(bool_true_encoded, 0x0000000000000002);
        
        let addr_encoded = ValueEncoder::encode_address(1);
        assert_eq!(addr_encoded, 0x0000000000000005); // 1 << 2 | 1
        
        let heap_encoded = ValueEncoder::encode_heap_ref(1);
        assert_eq!(heap_encoded, 0x0000000000000007); // 1 << 2 | 2 | 1
    }
}