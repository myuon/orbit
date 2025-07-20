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

/// Positioned error - wrapper around anyhow::Error with position information
#[derive(Debug, Clone)]
pub struct PositionedError {
    pub value: String, // Store the error message as string to allow cloning
    pub span: Span,
}

impl PositionedError {
    pub fn new(value: anyhow::Error, span: Span) -> Self {
        Self {
            value: value.to_string(),
            span,
        }
    }

    pub fn new_with_message(message: String, span: Span) -> Self {
        Self {
            value: message,
            span,
        }
    }
}

impl std::fmt::Display for PositionedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl std::error::Error for PositionedError {}

/// Macro for creating positioned errors similar to anyhow!
#[macro_export]
macro_rules! anyhow_with_position {
    ($span:expr, $msg:literal $(,)?) => {
        anyhow::Error::new($crate::utils::positioned::PositionedError::new_with_message($msg.to_string(), $span))
    };
    ($span:expr, $err:expr $(,)?) => {
        anyhow::Error::new($crate::utils::positioned::PositionedError::new($err, $span))
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        anyhow::Error::new($crate::utils::positioned::PositionedError::new_with_message(format!($fmt, $($arg)*), $span))
    };
}

/// Macro for early return with positioned errors similar to bail!
#[macro_export]
macro_rules! bail_with_position {
    ($span:expr, $msg:literal $(,)?) => {
        return Err($crate::anyhow_with_position!($span, $msg))
    };
    ($span:expr, $err:expr $(,)?) => {
        return Err($crate::anyhow_with_position!($span, $err))
    };
    ($span:expr, $fmt:expr, $($arg:tt)*) => {
        return Err($crate::anyhow_with_position!($span, $fmt, $($arg)*))
    };
}
