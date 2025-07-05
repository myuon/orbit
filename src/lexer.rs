use crate::ast::{Token, TokenType};

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
                None => {
                    return Token::new(TokenType::Eof, self.position);
                }
                Some(_) => {
                    self.advance();
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        
        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            
            if is_eof {
                break;
            }
        }
        
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_number() {
        let mut lexer = Lexer::new("42");
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::Number(42.0));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_float() {
        let mut lexer = Lexer::new("3.14");
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::Number(3.14));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_arithmetic() {
        let mut lexer = Lexer::new("2 + 3 * 4");
        let tokens = lexer.tokenize();
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
        let tokens = lexer.tokenize();
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
}