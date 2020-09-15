pub use compile_pass::*;
pub use exec_pass::*;
pub use infer_pass::*;
pub use scope_pass::*;

mod compile_pass;
mod exec_pass;
mod infer_pass;
mod scope_pass;

pub const INFER_PASS: &str = "infer_pass";
pub const SCOPE_PASS: &str = "scope_pass";
pub const COMPILE_PASS: &str = "compile_pass";
pub const EXEC_PASS: &str = "exec_pass";
