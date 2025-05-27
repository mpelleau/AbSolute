use libc::{c_int, fesetround, fegetround};
use std::fmt::{self, Display}; // Corrected: Removed Debug as it's not directly used for Display

// Platform-dependent rounding mode constants from libc.
// These might not be defined on all targets by libc crate directly.
// Common values are provided as fallbacks, but this is a known portability issue.
// A build script could try to detect these, or a more specific fenv crate might be needed for full portability.

// Standard rounding modes (values often found on Linux x86_64)
pub const FE_TONEAREST: c_int = 0x0000;
pub const FE_DOWNWARD: c_int = 0x0400;
pub const FE_UPWARD: c_int = 0x0800;
pub const FE_TOWARDZERO: c_int = 0x0c00;
// Add other modes if available and needed by the library.

// Store the original mode for OCaml's init/restore pattern if still needed by other code.
// However, the RAII guard is preferred for new code.
static mut OCAML_ORIGINAL_ROUNDING_MODE: c_int = -1; // Initial state, -1 indicates not set or error

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RoundingMode {
    ToNearest,
    Downward,
    Upward,
    TowardZero,
    // Current, // To represent not changing but capturing the current mode
}

impl Display for RoundingMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RoundingMode::ToNearest => write!(f, "ToNearest"),
            RoundingMode::Downward => write!(f, "Downward"),
            RoundingMode::Upward => write!(f, "Upward"),
            RoundingMode::TowardZero => write!(f, "TowardZero"),
        }
    }
}


impl RoundingMode {
    fn to_libc_const(self) -> c_int { // Corrected: Added `fn` keyword and `self`
        match self {
            RoundingMode::ToNearest => FE_TONEAREST,
            RoundingMode::Downward => FE_DOWNWARD,
            RoundingMode::Upward => FE_UPWARD,
            RoundingMode::TowardZero => FE_TOWARDZERO,
        }
    }
}

/// An RAII guard to temporarily set the FPU rounding mode.
/// When dropped, it restores the mode that was active when it was created.
#[must_use = "FPU rounding mode guard will reset on drop"]
pub struct FpuRoundingGuard {
    original_mode: c_int,
    active: bool, // To prevent double-restore if set_mode fails
}

impl FpuRoundingGuard {
    /// Attempts to set the FPU rounding mode. Stores the current mode for restoration.
    /// Returns None if either getting or setting the mode fails.
    pub fn new(mode: RoundingMode) -> Option<Self> {
        unsafe {
            let original_mode = fegetround();
            if original_mode == -1 { // Error getting current mode
                // eprintln!("Warning: Failed to get current FPU rounding mode.");
                return None; 
            }

            if fesetround(mode.to_libc_const()) != 0 { // Error setting new mode
                // eprintln!("Warning: Failed to set FPU rounding mode to {:?}", mode);
                // Attempt to restore original mode just in case it was partially changed
                // though fesetround failing usually means no change happened.
                fesetround(original_mode); // Best effort restore
                return None;
            }
            Some(Self { original_mode, active: true })
        }
    }
}

impl Drop for FpuRoundingGuard {
    fn drop(&mut self) {
        if self.active {
            unsafe {
                if fesetround(self.original_mode) != 0 {
                    // eprintln!("Warning: Failed to restore original FPU rounding mode.");
                    // This is problematic. The FPU state is now potentially unexpected.
                }
            }
        }
    }
}

/// Executes a function `f` within a specific FPU rounding environment.
/// Returns the result of `f`.
/// Panics if the rounding mode cannot be set. This is often preferred for numerical code
/// where incorrect rounding can silently corrupt results.
pub fn with_rounding_mode<F, R>(mode: RoundingMode, f: F) -> R
where
    F: FnOnce() -> R,
{
    let _guard = FpuRoundingGuard::new(mode)
        .unwrap_or_else(|| panic!("Failed to set FPU rounding mode: {:?}. Aborting.", mode));
    f()
}


// --- Original OCaml-style functions (can be deprecated or kept for specific use) ---

/// OCaml's `ml_float_init`: Saves current FPU mode and sets to FE_UPWARD.
/// Prefer using `FpuRoundingGuard` or `with_rounding_mode` for new code.
pub fn ocaml_ml_float_init() -> Result<(), &'static str> {
    unsafe {
        let current_mode = fegetround();
        if current_mode == -1 {
            return Err("Failed to get current FPU rounding mode");
        }
        OCAML_ORIGINAL_ROUNDING_MODE = current_mode;
        
        if fesetround(FE_UPWARD) != 0 { // OCaml version specifically used FE_UPWARD
            // Attempt to restore if set failed, though unlikely to succeed if current_mode was valid
            fesetround(OCAML_ORIGINAL_ROUNDING_MODE); 
            return Err("Failed to set FPU rounding mode to FE_UPWARD");
        }
        Ok(())
    }
}

/// OCaml's `ml_float_restore`: Restores FPU mode saved by `ocaml_ml_float_init`.
/// Prefer using `FpuRoundingGuard` or `with_rounding_mode` for new code.
pub fn ocaml_ml_float_restore() -> Result<(), &'static str> {
    unsafe {
        if OCAML_ORIGINAL_ROUNDING_MODE != -1 {
            if fesetround(OCAML_ORIGINAL_ROUNDING_MODE) != 0 {
                return Err("Failed to restore original FPU rounding mode");
            }
            OCAML_ORIGINAL_ROUNDING_MODE = -1; // Reset state after restoring
            Ok(())
        } else {
            Err("No FPU rounding mode previously saved by ocaml_ml_float_init or restore already called")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to get current rounding mode as RoundingMode enum, if possible
    fn get_current_mode_enum() -> Option<RoundingMode> {
        unsafe {
            let mode_val = fegetround();
            if mode_val == FE_TONEAREST { Some(RoundingMode::ToNearest) }
            else if mode_val == FE_DOWNWARD { Some(RoundingMode::Downward) }
            else if mode_val == FE_UPWARD { Some(RoundingMode::Upward) }
            else if mode_val == FE_TOWARDZERO { Some(RoundingMode::TowardZero) }
            else { None } // Unknown or error
        }
    }

    #[test]
    fn test_fpu_guard() {
        // This test is sensitive to the environment and what other tests might do.
        // It also might not work on all platforms if libc doesn't fully support fenv.
        // We assume default is ToNearest for this test's check.
        let initial_mode_val = unsafe { fegetround() };
        if initial_mode_val == -1 {
            eprintln!("FPU test skipped: fegetround() failed, possibly unsupported platform.");
            return;
        }

        let _original_mode_enum_opt = get_current_mode_enum(); // Renamed to avoid warning, used in commented eprint
        // eprintln!("Original mode: {:?}", original_mode_enum_opt);

        if let Some(guard) = FpuRoundingGuard::new(RoundingMode::Upward) {
            let current_mode_after_set = unsafe { fegetround() };
            // eprintln!("Mode under guard: {:?}", get_current_mode_enum());
            assert_eq!(current_mode_after_set, FE_UPWARD, "Mode should be Upward within guard");
            drop(guard);
        } else {
            eprintln!("FPU test skipped: FpuRoundingGuard::new(Upward) failed.");
            return; // Cannot proceed if guard fails
        }
        
        let mode_after_guard = unsafe { fegetround() };
        // eprintln!("Mode after guard: {:?}", get_current_mode_enum());
        assert_eq!(mode_after_guard, initial_mode_val, "Mode should be restored after guard");

        // Test with `with_rounding_mode`
        let _result = with_rounding_mode(RoundingMode::Downward, || { // Renamed to avoid warning
            assert_eq!(unsafe { fegetround() }, FE_DOWNWARD, "Mode should be Downward within closure");
            10.0 / 3.0 // Example operation
        });
        // eprintln!("Result from with_rounding_mode: {}", result);
        // We don't check the result value itself for exactness here, just that mode is restored.
        
        let mode_after_with = unsafe { fegetround() };
        // eprintln!("Mode after with_rounding_mode: {:?}", get_current_mode_enum());
        assert_eq!(mode_after_with, initial_mode_val, "Mode should be restored after with_rounding_mode");
    }

    #[test]
    fn test_ocaml_compat_functions() {
        let initial_mode_val = unsafe { fegetround() };
        if initial_mode_val == -1 {
            eprintln!("FPU ocaml_compat test skipped: fegetround() failed.");
            return;
        }

        if ocaml_ml_float_init().is_ok() {
            assert_eq!(unsafe { fegetround() }, FE_UPWARD);
            assert!(ocaml_ml_float_restore().is_ok());
            assert_eq!(unsafe { fegetround() }, initial_mode_val);
        } else {
             eprintln!("FPU ocaml_compat test skipped: ocaml_ml_float_init() failed.");
        }
    }
}
