//! Position tracking and error handling utilities
//! 
//! This module provides utilities for tracking source code positions and creating
//! positioned errors that can provide precise error locations in source files.

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Option<usize>,
    pub end: Option<usize>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
        }
    }

    pub fn unknown() -> Self {
        Self {
            start: None,
            end: None,
        }
    }

    pub fn single(pos: usize) -> Self {
        Self {
            start: Some(pos),
            end: Some(pos),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Positioned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Positioned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn with_unknown_span(value: T) -> Self {
        Self {
            value,
            span: Span::unknown(),
        }
    }
}

/// Positioned error with cloneable string message
#[derive(Debug, Clone)]
pub struct PositionedError {
    pub message: String,
    pub span: Span,
}

impl PositionedError {
    /// Create a positioned error with a span from a string message
    pub fn new_with_span(message: String, span: Span) -> Self {
        Self { message, span }
    }

    /// Create a positioned error from an anyhow::Error and span  
    pub fn from_error_with_span(error: anyhow::Error, span: Span) -> Self {
        Self {
            message: error.to_string(),
            span,
        }
    }

    /// Create a positioned error without span information
    pub fn new_without_span(message: String) -> Self {
        Self {
            message,
            span: Span::unknown(),
        }
    }

    /// Convert to anyhow::Error (for Result compatibility)
    pub fn into_anyhow(self) -> anyhow::Error {
        anyhow::Error::from(self)
    }
}

impl std::fmt::Display for PositionedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for PositionedError {}

/// Macro for creating positioned errors similar to anyhow!
#[macro_export]
macro_rules! anyhow_with_position {
    ($span:expr, $msg:literal $(,)?) => {
        $crate::utils::positioned::PositionedError::new_with_span($msg.to_string(), $span)
    };
    ($span:expr, $err:expr $(,)?) => {
        $crate::utils::positioned::PositionedError::from_error_with_span($err, $span)
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::utils::positioned::PositionedError::new_with_span(format!($fmt, $($arg)*), $span)
    };
}

/// Macro for early return with positioned errors similar to bail!
#[macro_export]
macro_rules! bail_with_position {
    ($span:expr, $msg:literal $(,)?) => {
        return Err($crate::anyhow_with_position!($span, $msg).into_anyhow())
    };
    ($span:expr, $err:expr $(,)?) => {
        return Err($crate::anyhow_with_position!($span, $err).into_anyhow())
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        return Err($crate::anyhow_with_position!($span, $fmt, $($arg)*).into_anyhow())
    };
}