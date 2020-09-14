pub use compile_pass::*;
pub use const_pass::*;
pub use exec_pass::*;
pub use infer_pass::*;

mod compile_pass;
mod const_pass;
mod exec_pass;
mod infer_pass;

pub const CONST_PASS: &str = "const_pass";
pub const INFER_PASS: &str = "infer_pass";
pub const COMPILE_PASS: &str = "compile_pass";
pub const EXEC_PASS: &str = "exec_pass";
