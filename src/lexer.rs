use crate::ast::{Token, TokenType};
use anyhow::Result;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.get(0).copied();
        
        Lexer {
            input: chars,
            position: 0,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        if self.position < self.input.len() {
            self.current_char = Some(self.input[self.position]);
        } else {
            self.current_char = None;
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> f64 {
        let mut num_str = String::new();
        
        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() || ch == '.' {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        
        num_str.parse().unwrap_or(0.0)
    }

    fn read_string(&mut self) -> String {
        let mut string_value = String::new();
        self.advance(); // Skip opening quote
        
        while let Some(ch) = self.current_char {
            if ch == '"' {
                self.advance(); // Skip closing quote
                break;
            } else if ch == '\\' {
                self.advance();
                if let Some(escaped) = self.current_char {
                    match escaped {
                        'n' => string_value.push('\n'),
                        't' => string_value.push('\t'),
                        'r' => string_value.push('\r'),
                        '\\' => string_value.push('\\'),
                        '"' => string_value.push('"'),
                        _ => {
                            string_value.push('\\');
                            string_value.push(escaped);
                        }
                    }
                    self.advance();
                }
            } else {
                string_value.push(ch);
                self.advance();
            }
        }
        
        string_value
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        
        while let Some(ch) = self.current_char {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        
        identifier
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            match self.current_char {
                Some(ch) if ch.is_whitespace() => {
                    self.skip_whitespace();
                }
                Some(ch) if ch.is_ascii_digit() => {
                    let pos = self.position;
                    let num = self.read_number();
                    return Token::new(TokenType::Number(num), pos);
                }
                Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => {
                    let pos = self.position;
                    let identifier = self.read_identifier();
                    let token_type = match identifier.as_str() {
                        "true" => TokenType::Boolean(true),
                        "false" => TokenType::Boolean(false),
                        "let" => TokenType::Let,
                        "fun" => TokenType::Fun,
                        "do" => TokenType::Do,
                        "end" => TokenType::End,
                        "return" => TokenType::Return,
                        "if" => TokenType::If,
                        "then" => TokenType::Then,
                        "else" => TokenType::Else,
                        _ => TokenType::Identifier(identifier),
                    };
                    return Token::new(token_type, pos);
                }
                Some('"') => {
                    let pos = self.position;
                    let string_value = self.read_string();
                    return Token::new(TokenType::String(string_value), pos);
                }
                Some('+') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Plus, pos);
                }
                Some('-') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Minus, pos);
                }
                Some('*') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Star, pos);
                }
                Some('/') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Slash, pos);
                }
                Some('=') => {
                    let pos = self.position;
                    self.advance();
                    if let Some('=') = self.current_char {
                        self.advance();
                        return Token::new(TokenType::Equal, pos);
                    }
                    return Token::new(TokenType::Assign, pos);
                }
                Some(';') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Semicolon, pos);
                }
                Some('(') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::LeftParen, pos);
                }
                Some(')') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::RightParen, pos);
                }
                Some(',') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Comma, pos);
                }
                Some(':') => {
                    let pos = self.position;
                    self.advance();
                    return Token::new(TokenType::Colon, pos);
                }
                Some('!') => {
                    let pos = self.position;
                    self.advance();
                    if let Some('=') = self.current_char {
                        self.advance();
                        return Token::new(TokenType::NotEqual, pos);
                    }
                    // Handle unexpected '!' character
                    continue;
                }
                Some('<') => {
                    let pos = self.position;
                    self.advance();
                    if let Some('=') = self.current_char {
                        self.advance();
                        return Token::new(TokenType::LessEqual, pos);
                    }
                    return Token::new(TokenType::Less, pos);
                }
                Some('>') => {
                    let pos = self.position;
                    self.advance();
                    if let Some('=') = self.current_char {
                        self.advance();
                        return Token::new(TokenType::GreaterEqual, pos);
                    }
                    return Token::new(TokenType::Greater, pos);
                }
                None => {
                    return Token::new(TokenType::Eof, self.position);
                }
                Some(_) => {
                    self.advance();
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        
        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            
            if is_eof {
                break;
            }
        }
        
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_single_token(input: &str, expected: TokenType) {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 2, "Expected exactly one token + EOF for input: {}", input);
        assert_eq!(tokens[0].token_type, expected, "Token mismatch for input: {}", input);
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_numbers() {
        assert_single_token("42", TokenType::Number(42.0));
        assert_single_token("3.14", TokenType::Number(3.14));
    }

    #[test]
    fn test_tokenize_arithmetic() {
        let mut lexer = Lexer::new("2 + 3 * 4");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].token_type, TokenType::Number(2.0));
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Number(3.0));
        assert_eq!(tokens[3].token_type, TokenType::Star);
        assert_eq!(tokens[4].token_type, TokenType::Number(4.0));
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_parentheses() {
        let mut lexer = Lexer::new("(2 + 3) * 4");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].token_type, TokenType::LeftParen);
        assert_eq!(tokens[1].token_type, TokenType::Number(2.0));
        assert_eq!(tokens[2].token_type, TokenType::Plus);
        assert_eq!(tokens[3].token_type, TokenType::Number(3.0));
        assert_eq!(tokens[4].token_type, TokenType::RightParen);
        assert_eq!(tokens[5].token_type, TokenType::Star);
        assert_eq!(tokens[6].token_type, TokenType::Number(4.0));
        assert_eq!(tokens[7].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_comparison_operators() {
        let test_cases = vec![
            ("==", TokenType::Equal),
            ("!=", TokenType::NotEqual),
            ("<", TokenType::Less),
            (">", TokenType::Greater),
            ("<=", TokenType::LessEqual),
            (">=", TokenType::GreaterEqual),
        ];

        for (input, expected) in test_cases {
            assert_single_token(input, expected);
        }
    }

    #[test]
    fn test_tokenize_conditional_keywords() {
        let test_cases = vec![
            ("if", TokenType::If),
            ("then", TokenType::Then),
            ("else", TokenType::Else),
        ];

        for (input, expected) in test_cases {
            assert_single_token(input, expected);
        }
    }

    #[test]
    fn test_tokenize_conditional_expression() {
        let mut lexer = Lexer::new("if x > 5 then 10 else 20");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[0].token_type, TokenType::If);
        assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Greater);
        assert_eq!(tokens[3].token_type, TokenType::Number(5.0));
        assert_eq!(tokens[4].token_type, TokenType::Then);
        assert_eq!(tokens[5].token_type, TokenType::Number(10.0));
        assert_eq!(tokens[6].token_type, TokenType::Else);
        assert_eq!(tokens[7].token_type, TokenType::Number(20.0));
        assert_eq!(tokens[8].token_type, TokenType::Eof);
    }
}