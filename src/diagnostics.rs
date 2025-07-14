use crate::ast::Span;

/// Represents the severity level of a diagnostic message
#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

/// Represents a source location with line and column information
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub byte_offset: usize,
}

/// Represents a diagnostic message with position information
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub location: Option<SourceLocation>,
    pub span: Option<Span>,
    pub help: Option<String>,
}

impl Diagnostic {
    /// Create a new error diagnostic
    pub fn error(message: String) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            message,
            location: None,
            span: None,
            help: None,
        }
    }

    /// Create a new error diagnostic with position
    pub fn error_at(message: String, location: SourceLocation) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            message,
            location: Some(location),
            span: None,
            help: None,
        }
    }

    /// Create a new error diagnostic with span
    pub fn error_with_span(message: String, span: Span, location: Option<SourceLocation>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            message,
            location,
            span: Some(span),
            help: None,
        }
    }

    /// Add help text to this diagnostic
    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }

    /// Format this diagnostic as a user-friendly error message
    pub fn format(&self, source_content: Option<&str>) -> String {
        let mut output = String::new();

        // Format the main message
        let level_str = match self.level {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning", 
            DiagnosticLevel::Info => "info",
        };

        if let Some(location) = &self.location {
            output.push_str(&format!("{}:{}:{}: {}: {}\n", 
                location.file, location.line, location.column, level_str, self.message));
            
            // Show source context if available
            if let Some(source) = source_content {
                if let Some(context) = self.get_source_context(source, location) {
                    output.push_str(&context);
                }
            }
        } else {
            output.push_str(&format!("{}: {}\n", level_str, self.message));
        }

        // Add help text if available
        if let Some(help) = &self.help {
            output.push_str(&format!("help: {}\n", help));
        }

        output
    }

    /// Format this diagnostic with a position calculator that can provide the correct source context
    pub fn format_with_calculator(&self, position_calculator: &PositionCalculator) -> String {
        let mut output = String::new();

        // Format the main message
        let level_str = match self.level {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning", 
            DiagnosticLevel::Info => "info",
        };

        if let Some(location) = &self.location {
            output.push_str(&format!("{}:{}:{}: {}: {}\n", 
                location.file, location.line, location.column, level_str, self.message));
            
            // Get the appropriate source content based on the location
            let source_content = position_calculator.get_source_for_location(location);
            if let Some(context) = self.get_source_context(&source_content, location) {
                output.push_str(&context);
            }
        } else {
            output.push_str(&format!("{}: {}\n", level_str, self.message));
        }

        // Add help text if available
        if let Some(help) = &self.help {
            output.push_str(&format!("help: {}\n", help));
        }

        output
    }

    /// Get source context around the error location
    fn get_source_context(&self, source: &str, location: &SourceLocation) -> Option<String> {
        let lines: Vec<&str> = source.lines().collect();
        
        if location.line == 0 || location.line > lines.len() {
            return None;
        }

        let line_idx = location.line - 1;
        let line_content = lines[line_idx];
        
        let mut context = String::new();
        
        // Show the line with the error
        context.push_str(&format!("{:4} | {}\n", location.line, line_content));
        
        // Show pointer to the exact column
        let pointer_line = format!("{:4} | {}{}\n", 
            "", 
            " ".repeat(location.column.saturating_sub(1)), 
            "^");
        context.push_str(&pointer_line);
        
        Some(context)
    }
}

/// Utility for converting byte positions to line/column information
pub struct PositionCalculator {
    pub source: String,
    std_lib_content: Option<String>,
    std_lib_offset: usize,
}

impl PositionCalculator {
    /// Create a new position calculator
    pub fn new(source: String, std_lib_content: Option<String>) -> Self {
        let std_lib_offset = std_lib_content.as_ref()
            .map(|content| content.len() + 1) // +1 for the newline added between std and user code
            .unwrap_or(0);
            
        Self {
            source,
            std_lib_content,
            std_lib_offset,
        }
    }

    /// Convert a byte position to line/column information
    pub fn position_to_location(&self, byte_offset: usize, filename: &str) -> SourceLocation {
        // If position is within std.ob, handle it separately
        if let Some(std_content) = &self.std_lib_content {
            if byte_offset < self.std_lib_offset {
                return self.calculate_location_in_content(std_content, byte_offset, "std.ob");
            }
        }

        // Position is in user code, adjust for std.ob offset
        let user_offset = if byte_offset >= self.std_lib_offset {
            byte_offset - self.std_lib_offset
        } else {
            0
        };

        // Get user code part
        let user_code = if let Some(_) = &self.std_lib_content {
            // Skip std.ob content and newline
            self.source.get(self.std_lib_offset..).unwrap_or("")
        } else {
            &self.source
        };

        self.calculate_location_in_content(user_code, user_offset, filename)
    }

    /// Calculate line/column in a specific content string
    fn calculate_location_in_content(&self, content: &str, offset: usize, filename: &str) -> SourceLocation {
        let mut line = 1;
        let mut column = 1;
        let mut current_offset = 0;

        for ch in content.chars() {
            if current_offset >= offset {
                break;
            }

            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }

            current_offset += ch.len_utf8();
        }

        SourceLocation {
            file: filename.to_string(),
            line,
            column,
            byte_offset: offset,
        }
    }

    /// Check if a position is within the std.ob content
    pub fn is_in_std_lib(&self, byte_offset: usize) -> bool {
        self.std_lib_content.is_some() && byte_offset < self.std_lib_offset
    }

    /// Get the appropriate source content for a given location
    pub fn get_source_for_location(&self, location: &SourceLocation) -> String {
        if location.file == "std.ob" {
            // Return std.ob content if available
            self.std_lib_content.clone().unwrap_or_default()
        } else {
            // Return user code content
            if let Some(_) = &self.std_lib_content {
                // Skip std.ob content and newline
                self.source.get(self.std_lib_offset..).unwrap_or("").to_string()
            } else {
                self.source.clone()
            }
        }
    }
}

/// Result type that includes diagnostics
pub type DiagnosticResult<T> = Result<T, Diagnostic>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_calculator_without_std() {
        let source = "let x = 5;\nlet y = 10;".to_string();
        let y_pos = source.find('y').unwrap();
        let calc = PositionCalculator::new(source, None);
        
        let loc = calc.position_to_location(0, "test.ob");
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 1);
        
        let loc = calc.position_to_location(y_pos, "test.ob"); // Position of "y"
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_position_calculator_with_std() {
        let std_content = "type int = struct {};".to_string();
        let user_code = "let x = 5;".to_string();
        let full_source = format!("{}\n{}", std_content, user_code);
        
        let calc = PositionCalculator::new(full_source, Some(std_content.clone()));
        
        // Position in std.ob
        let loc = calc.position_to_location(5, "test.ob"); // Position of "int"
        assert_eq!(loc.file, "std.ob");
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 6);
        
        // Position in user code
        let user_start = std_content.len() + 1; // +1 for newline
        let loc = calc.position_to_location(user_start + 4, "test.ob"); // Position of "x"
        assert_eq!(loc.file, "test.ob");
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 5);
    }

    #[test]
    fn test_diagnostic_formatting() {
        let location = SourceLocation {
            file: "test.ob".to_string(),
            line: 2,
            column: 5,
            byte_offset: 15,
        };
        
        let diagnostic = Diagnostic::error_at(
            "Undefined variable 'x'".to_string(), 
            location
        ).with_help("Did you mean to declare 'x' first?".to_string());
        
        let source = "let y = 10;\nlet z = x + 5;";
        let formatted = diagnostic.format(Some(source));
        
        assert!(formatted.contains("test.ob:2:5: error: Undefined variable 'x'"));
        assert!(formatted.contains("let z = x + 5;"));
        assert!(formatted.contains("^"));
        assert!(formatted.contains("help: Did you mean to declare 'x' first?"));
    }
}