#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kleene {
    False,
    True,
    Unknown,
}

/// Kleene AND operation.
pub fn k_and(a: Kleene, b: Kleene) -> Kleene {
    use Kleene::*;
    match (a, b) {
        (False, _) | (_, False) => False,
        (Unknown, _) | (_, Unknown) => Unknown,
        (True, True) => True,
    }
}

/// Kleene OR operation.
pub fn k_or(a: Kleene, b: Kleene) -> Kleene {
    use Kleene::*;
    match (a, b) {
        (True, _) | (_, True) => True,
        (Unknown, _) | (_, Unknown) => Unknown,
        (False, False) => False,
    }
}

/// Kleene NOT operation.
pub fn k_not(a: Kleene) -> Kleene {
    use Kleene::*;
    match a {
        True => False,
        False => True,
        Unknown => Unknown,
    }
}

/// Converts a standard boolean to a Kleene value.
pub fn from_bool(b: bool) -> Kleene {
    if b {
        Kleene::True
    } else {
        Kleene::False
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_k_and() {
        use Kleene::*;
        assert_eq!(k_and(True, True), True);
        assert_eq!(k_and(True, False), False);
        assert_eq!(k_and(True, Unknown), Unknown);

        assert_eq!(k_and(False, True), False);
        assert_eq!(k_and(False, False), False);
        assert_eq!(k_and(False, Unknown), False);

        assert_eq!(k_and(Unknown, True), Unknown);
        assert_eq!(k_and(Unknown, False), False);
        assert_eq!(k_and(Unknown, Unknown), Unknown);
    }

    #[test]
    fn test_k_or() {
        use Kleene::*;
        assert_eq!(k_or(True, True), True);
        assert_eq!(k_or(True, False), True);
        assert_eq!(k_or(True, Unknown), True);

        assert_eq!(k_or(False, True), True);
        assert_eq!(k_or(False, False), False);
        assert_eq!(k_or(False, Unknown), Unknown);

        assert_eq!(k_or(Unknown, True), True);
        assert_eq!(k_or(Unknown, False), Unknown);
        assert_eq!(k_or(Unknown, Unknown), Unknown);
    }

    #[test]
    fn test_k_not() {
        use Kleene::*;
        assert_eq!(k_not(True), False);
        assert_eq!(k_not(False), True);
        assert_eq!(k_not(Unknown), Unknown);
    }

    #[test]
    fn test_from_bool() {
        assert_eq!(from_bool(true), Kleene::True);
        assert_eq!(from_bool(false), Kleene::False);
    }
}
