pub mod codes;
pub mod error;
pub mod span;

pub use codes::ErrorCode;
pub use error::{Diagnostic, Severity, callee_not_function_diagnostic};
pub use span::{Position, Span};
