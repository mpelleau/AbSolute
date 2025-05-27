use once_cell::sync::Lazy;
use std::sync::Mutex; // Using Mutex for simplicity; RwLock could be used for read-heavy fields.
use std::fmt;

// --- Error Type ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigError {
    InvalidValue(String),
}

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConfigError::InvalidValue(msg) => write!(f, "Invalid configuration value: {}", msg),
        }
    }
}

impl std::error::Error for ConfigError {}

// --- Configuration Parameters ---
// Using i64 for general integer parameters, String for string params, bool for booleans, f64 for float.
// Debug level is i32 as it's likely smaller.

pub static NAME: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));
pub static PRECISION: Lazy<Mutex<f64>> = Lazy::new(|| Mutex::new(0.01));
pub static MAX_ITER: Lazy<Mutex<i64>> = Lazy::new(|| Mutex::new(100_000_000));
pub static MAX_DEPTH: Lazy<Mutex<i64>> = Lazy::new(|| Mutex::new(100_000_000));
pub static MAX_SOL: Lazy<Mutex<i64>> = Lazy::new(|| Mutex::new(10_000_000));
pub static DOMAIN: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new("box".to_string()));
pub static BOOLEAN: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new("boolean".to_string()));
pub static ITERATOR: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new("roundrobin".to_string()));
pub static PRODUCT: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static WITNESS: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static SPLIT: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new("default".to_string()));
pub static JOIN: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new("weak".to_string()));
pub static MINIMIZING: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static VISUALIZATION: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static OBJ: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false)); // 'obj' is a keyword in Rust, consider renaming if used directly
pub static TEX: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static SVG: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static TRACE: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static DEBUG_LEVEL: Lazy<Mutex<i32>> = Lazy::new(|| Mutex::new(0)); // Renamed from 'debug'
pub static PRUNING: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static SURE: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static ITER_PARAM: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false)); // Renamed from 'iter'
pub static PRUNING_ITER: Lazy<Mutex<i64>> = Lazy::new(|| Mutex::new(100_000_000));
pub static REWRITE: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(true));
pub static STEP_BY_STEP: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));
pub static POL_AS_GEN: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

// --- Setter Functions ---

pub fn set_debug_level(lv: i32) -> Result<(), ConfigError> {
    if lv >= 0 {
        *DEBUG_LEVEL.lock().unwrap() = lv;
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("debug level must be positive".to_string()))
    }
}

// OCaml: let set_debug () = debug := 1
// This implies a direct set without returning Result, perhaps panicking or just setting.
// For consistency, let's make it return Result, or provide two versions.
// Or, it could just ignore errors if we want to match the OCaml style of not erroring here.
// Let's assume it should succeed.
pub fn set_debug_default() {
    *DEBUG_LEVEL.lock().unwrap() = 1;
}

pub fn set_precision(f: f64) -> Result<(), ConfigError> {
    if f > 0.0 {
        *PRECISION.lock().unwrap() = f;
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("precision must be strictly positive".to_string()))
    }
}

pub fn set_max_iter(i: i64) -> Result<(), ConfigError> {
    if i > 0 {
        *MAX_ITER.lock().unwrap() = i;
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("number of iterations must be strictly positive".to_string()))
    }
}

pub fn set_max_depth(i: i64) -> Result<(), ConfigError> {
    if i > 0 {
        *MAX_DEPTH.lock().unwrap() = i;
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("depth must be strictly positive".to_string()))
    }
}

pub fn set_pruning_iter(i: i64) -> Result<(), ConfigError> {
    if i > 0 {
        *PRUNING_ITER.lock().unwrap() = i;
        *PRUNING.lock().unwrap() = true; // Also sets pruning to true
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("number of pruning iterations must be strictly positive".to_string()))
    }
}

pub fn set_max_sol(s: i64) -> Result<(), ConfigError> {
    if s > 0 {
        *MAX_SOL.lock().unwrap() = s;
        Ok(())
    } else {
        Err(ConfigError::InvalidValue("number of solutions must be strictly positive".to_string()))
    }
}

pub fn set_split_strategy(s: &str) -> Result<(), ConfigError> {
    match s {
        "default" | "maxSmear" | "smear" | "pizza" => {
            *SPLIT.lock().unwrap() = s.to_string();
            Ok(())
        }
        x => Err(ConfigError::InvalidValue(format!("bisection strategy {} undefined", x))),
    }
}

pub fn set_iterator_strategy(s: &str) -> Result<(), ConfigError> {
    match s {
        "roundrobin" | "graph" => {
            *ITERATOR.lock().unwrap() = s.to_string();
            Ok(())
        }
        x => Err(ConfigError::InvalidValue(format!("iterator strategy {} undefined", x))),
    }
}

// --- Getter functions (examples, add as needed) ---
// For reading values, getters can be convenient to hide the Mutex locking.

pub fn get_precision() -> f64 {
    *PRECISION.lock().unwrap()
}

pub fn get_debug_level() -> i32 {
    *DEBUG_LEVEL.lock().unwrap()
}

// Add other getters as they become necessary during translation of other modules.

#[cfg(test)]
mod tests {
    use super::*;

    fn reset_to_defaults() {
        // Helper to reset relevant statics to their default values for isolated tests
        *NAME.lock().unwrap() = String::new();
        *PRECISION.lock().unwrap() = 0.01;
        *MAX_ITER.lock().unwrap() = 100_000_000;
        *MAX_DEPTH.lock().unwrap() = 100_000_000;
        *MAX_SOL.lock().unwrap() = 10_000_000;
        *DOMAIN.lock().unwrap() = "box".to_string();
        // ... reset others if they are modified in tests
        *DEBUG_LEVEL.lock().unwrap() = 0;
        *PRUNING.lock().unwrap() = false;
        *PRUNING_ITER.lock().unwrap() = 100_000_000;
        *SPLIT.lock().unwrap() = "default".to_string();
        *ITERATOR.lock().unwrap() = "roundrobin".to_string();
    }

    #[test]
    fn test_set_debug_level_valid() {
        reset_to_defaults();
        assert!(set_debug_level(5).is_ok());
        assert_eq!(*DEBUG_LEVEL.lock().unwrap(), 5);
    }

    #[test]
    fn test_set_debug_level_invalid() {
        reset_to_defaults();
        let result = set_debug_level(-1);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ConfigError::InvalidValue("debug level must be positive".to_string()));
        assert_eq!(*DEBUG_LEVEL.lock().unwrap(), 0); // Should remain default
    }

    #[test]
    fn test_set_debug_default() {
        reset_to_defaults();
        set_debug_default();
        assert_eq!(*DEBUG_LEVEL.lock().unwrap(), 1);
    }

    #[test]
    fn test_set_precision_valid() {
        reset_to_defaults();
        assert!(set_precision(0.001).is_ok());
        assert_eq!(*PRECISION.lock().unwrap(), 0.001);
    }

    #[test]
    fn test_set_precision_invalid() {
        reset_to_defaults();
        let result = set_precision(0.0);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ConfigError::InvalidValue("precision must be strictly positive".to_string()));
        assert_eq!(*PRECISION.lock().unwrap(), 0.01); // Should remain default
    }

    #[test]
    fn test_set_pruning_iter_valid() {
        reset_to_defaults();
        assert!(!*PRUNING.lock().unwrap()); // Check initial state
        assert!(set_pruning_iter(500).is_ok());
        assert_eq!(*PRUNING_ITER.lock().unwrap(), 500);
        assert!(*PRUNING.lock().unwrap()); // Should also set pruning to true
    }
    
    #[test]
    fn test_set_split_strategy_valid() {
        reset_to_defaults();
        assert!(set_split_strategy("maxSmear").is_ok());
        assert_eq!(*SPLIT.lock().unwrap(), "maxSmear");
    }

    #[test]
    fn test_set_split_strategy_invalid() {
        reset_to_defaults();
        let result = set_split_strategy("unknownStrategy");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ConfigError::InvalidValue("bisection strategy unknownStrategy undefined".to_string()));
        assert_eq!(*SPLIT.lock().unwrap(), "default");
    }

    #[test]
    fn test_getters() {
        reset_to_defaults();
        set_debug_level(3).unwrap();
        assert_eq!(get_debug_level(), 3);
        set_precision(0.5).unwrap();
        assert_eq!(get_precision(), 0.5);
    }
}
