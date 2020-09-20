pub use codegen_pass::*;
pub use collapse_pass::*;
pub use compile_pass::*;
pub use exec_pass::*;
pub use infer_pass::*;
pub use mir_pass::*;
pub use scope_pass::*;
pub use universe_pass::*;
pub use validate_pass::*;

mod codegen_pass;
mod collapse_pass;
mod compile_pass;
mod exec_pass;
mod infer_pass;
mod mir_pass;
mod scope_pass;
mod universe_pass;
mod validate_pass;

pub const VALIDATE_PASS: &str = "validate_pass";
pub const UNIVERSE_PASS: &str = "universe_pass";
pub const MIR_PASS: &str = "mir_pass";
pub const SCOPE_PASS: &str = "scope_pass";
pub const INFER_PASS: &str = "infer_pass";
pub const COLLAPSE_PASS: &str = "collapse_pass";
pub const COMPILE_PASS: &str = "compile_pass";
pub const EXEC_PASS: &str = "exec_pass";
pub const CODEGEN_PASS: &str = "codegen_pass";
