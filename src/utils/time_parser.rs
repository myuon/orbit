/// Parse timeout string like "10s", "5m", "30" (defaults to seconds)
pub fn parse_timeout(timeout_str: &str) -> Result<u64, String> {
    if timeout_str.is_empty() {
        return Err("Empty timeout value".to_string());
    }

    if timeout_str.ends_with('s') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str
            .parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else if timeout_str.ends_with('m') {
        let num_str = &timeout_str[..timeout_str.len() - 1];
        num_str
            .parse::<u64>()
            .map(|m| m * 60)
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    } else {
        // Default to seconds if no unit specified
        timeout_str
            .parse::<u64>()
            .map_err(|_| format!("Invalid timeout value: {}", timeout_str))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_timeout() {
        let test_cases = vec![
            ("10s", Ok(10)),
            ("5m", Ok(300)),
            ("30", Ok(30)),
            ("", Err("Empty timeout value".to_string())),
            ("invalid", Err("Invalid timeout value: invalid".to_string())),
            ("10x", Err("Invalid timeout value: 10x".to_string())),
        ];

        for (input, expected) in test_cases {
            let result = parse_timeout(input);
            assert_eq!(result, expected, "Failed for input: {}", input);
        }
    }
}
