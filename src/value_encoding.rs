use crate::runtime::{HeapIndex, Value};

/// Value encoding for JIT compilation
///
/// Encoding scheme using 64-bit integers:
/// - MSB (bit 63): 0 = numeric value, 1 = pointer
/// - For numeric values (MSB = 0): remaining 63 bits store the value
///   - Int, Boolean, Byte are all stored as i63 values (sign-extended)
/// - For pointer values (MSB = 1):
///   - Bit 62: 0 = Address, 1 = HeapRef
///   - Remaining 62 bits store the address/index
///
/// Examples:
/// - Int(42) -> 0x000000000000002A (42)
/// - Boolean(true) -> 0x0000000000000001 (1)
/// - Byte(255) -> 0x00000000000000FF (255)
/// - Address(0x1000) -> 0x8000000000001000 (MSB=1, bit 62=0, value=0x1000)
/// - HeapRef(5) -> 0xC000000000000005 (MSB=1, bit 62=1, value=5)

const POINTER_BIT: u64 = 1u64 << 63; // MSB (bit 63)
const HEAP_REF_BIT: u64 = 1u64 << 62; // Bit 62

pub struct ValueEncoder;

impl ValueEncoder {
    /// Encode a Value into a u64 for JIT compilation
    pub fn encode(value: &Value) -> u64 {
        match value {
            Value::Int(n) => {
                // Store as i63: MSB = 0 (numeric), remaining 63 bits store the value
                // Mask out the MSB to ensure it's 0 for numeric values
                (*n as u64) & !(POINTER_BIT)
            }
            Value::Boolean(b) => {
                // Store as i63: MSB = 0 (numeric), value in lower bits
                (*b as u64) & !(POINTER_BIT)
            }
            Value::Byte(b) => {
                // Store as i63: MSB = 0 (numeric), value in lower bits
                (*b as u64) & !(POINTER_BIT)
            }
            Value::Address(addr) => {
                // MSB = 1 (pointer), bit 62 = 0 (address), remaining bits = address
                POINTER_BIT | ((*addr as u64) & !(POINTER_BIT | HEAP_REF_BIT))
            }
            Value::HeapRef(HeapIndex(index)) => {
                // MSB = 1 (pointer), bit 62 = 1 (heap ref), remaining bits = index
                POINTER_BIT | HEAP_REF_BIT | ((*index as u64) & !(POINTER_BIT | HEAP_REF_BIT))
            }
        }
    }

    /// Decode a u64 back into a Value
    pub fn decode(encoded: u64) -> Value {
        if (encoded & POINTER_BIT) == 0 {
            // Numeric value: MSB = 0, extract value from lower 63 bits
            // Sign-extend from bit 62 to handle negative numbers correctly
            let numeric_value = if (encoded & (1u64 << 62)) != 0 {
                // Negative number: sign-extend
                (encoded as i64) | (POINTER_BIT as i64)
            } else {
                // Positive number
                encoded as i64
            };
            // We can't distinguish between Int, Boolean, and Byte from encoding
            // Default to Int for now (this is the accepted limitation)
            Value::Int(numeric_value)
        } else {
            // Pointer value: check bit 62 to distinguish Address vs HeapRef
            if (encoded & HEAP_REF_BIT) == 0 {
                // Address: extract from lower 62 bits
                let addr = (encoded & !(POINTER_BIT | HEAP_REF_BIT)) as usize;
                Value::Address(addr)
            } else {
                // HeapRef: extract from lower 62 bits
                let index = (encoded & !(POINTER_BIT | HEAP_REF_BIT)) as usize;
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
        // Sign-extend from bit 62 to handle negative numbers correctly
        if (encoded & (1u64 << 62)) != 0 {
            // Negative number: sign-extend
            (encoded as i64) | (POINTER_BIT as i64)
        } else {
            // Positive number
            encoded as i64
        }
    }

    /// Get the address from an encoded u64 (assumes it's an address)
    pub fn get_address(encoded: u64) -> usize {
        (encoded & !(POINTER_BIT | HEAP_REF_BIT)) as usize
    }

    /// Get the heap index from an encoded u64 (assumes it's a heap ref)
    pub fn get_heap_index(encoded: u64) -> usize {
        (encoded & !(POINTER_BIT | HEAP_REF_BIT)) as usize
    }

    /// Create an encoded integer value
    pub fn encode_int(value: i64) -> u64 {
        (value as u64) & !(POINTER_BIT)
    }

    /// Create an encoded boolean value
    pub fn encode_bool(value: bool) -> u64 {
        (value as u64) & !(POINTER_BIT)
    }

    /// Create an encoded byte value
    pub fn encode_byte(value: u8) -> u64 {
        (value as u64) & !(POINTER_BIT)
    }

    /// Create an encoded address value
    pub fn encode_address(addr: usize) -> u64 {
        POINTER_BIT | ((addr as u64) & !(POINTER_BIT | HEAP_REF_BIT))
    }

    /// Create an encoded heap reference value
    pub fn encode_heap_ref(index: usize) -> u64 {
        POINTER_BIT | HEAP_REF_BIT | ((index as u64) & !(POINTER_BIT | HEAP_REF_BIT))
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
        assert_eq!(bool_true_encoded, 0x0000000000000001);

        let addr_encoded = ValueEncoder::encode_address(1);
        assert_eq!(addr_encoded, 0x8000000000000001); // MSB=1, bit 62=0, value=1

        let heap_encoded = ValueEncoder::encode_heap_ref(1);
        assert_eq!(heap_encoded, 0xC000000000000001); // MSB=1, bit 62=1, value=1
    }
}
