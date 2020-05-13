pub use mint;
pub use spirv_interop_macros::*;

#[derive(Debug)]
pub struct UniformVariable {
    name: &'static str,
    set: u32,
    binding: u32,
}

#[derive(Debug)]
pub struct InputVariable {
    name: &'static str,
    location: u32,
}

#[derive(Debug)]
pub struct OutputVariable {
    name: &'static str,
    location: u32,
}
