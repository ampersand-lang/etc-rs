#[derive(Debug, Clone)]
pub struct Target {
    pub pointer_width: usize,
    pub pointer_align: usize,
}

// SYS-V 64-bit
impl Default for Target {
    fn default() -> Self {
        Self {
            pointer_width: 8,
            pointer_align: 8,
        }
    }
}
