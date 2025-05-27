// Declare modules for the interval sub-crate
pub mod bound_float;
pub mod bound_mpqf;
pub mod traits;    // New
pub mod itv;       // New: For Interval struct definition

// Re-export key types
pub use bound_float::FloatBound;
pub use bound_mpqf::RationalBound;
pub use traits::*; // Re-export new traits
pub use itv::Interval; // Re-export Interval struct
