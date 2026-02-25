mod calls;
mod ops;
mod props;
mod types;
pub mod interpreter;

pub use interpreter::{
    interpret, interpret_program, interpret_program_with_heap, interpret_program_with_limit,
    interpret_program_with_limit_and_cancel, interpret_program_with_trace,
};
pub use types::{Completion, Program, VmError};
