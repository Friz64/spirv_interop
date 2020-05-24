pub use memoffset;
pub use mint;
pub use spirv_interop_macros::*;

#[derive(Debug)]
pub struct Sampler;

#[derive(Debug)]
pub struct UniformVariable {
    pub name: &'static str,
    pub set: u32,
    pub binding: u32,
}

#[derive(Debug)]
pub struct InputAttribute {
    pub name: &'static str,
    pub location: u32,
    pub offset: usize,
}

#[derive(Debug)]
pub struct OutputAttribute {
    pub name: &'static str,
    pub location: u32,
    pub offset: usize,
}
