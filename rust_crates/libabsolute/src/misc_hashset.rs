use std::collections::HashSet;
use std::hash::Hash;
use std::fmt::Display;

// OCaml: type 'a t = ('a, unit) Hashtbl.t
// We'll use Rust's HashSet directly or provide thin wrappers.
// For this translation, we provide wrapper functions.

// OCaml: let print pp_elem fmt (h : 'a t) = ...
// Rust: A function to format the set content into a string.
pub fn format_hash_set<T, FmtElem>(
    set: &HashSet<T>,
    mut format_elem: FmtElem,
    separator: &str,
) -> String
where
    T: Hash + Eq,
    FmtElem: FnMut(&T) -> String,
{
    // HashSet iteration order is arbitrary. For consistent output in tests/logs,
    // it's often better to collect to a Vec, sort it (if T is Ord), then join.
    // However, to match OCaml's Hashtbl.to_seq general behavior (order not guaranteed),
    // we'll just map and join. If sorted output is needed, user should sort outside.
    set.iter()
        .map(|elem| format_elem(elem))
        .collect::<Vec<String>>()
        .join(separator)
}

// OCaml: let mem : 'a t -> 'a -> bool = Hashtbl.mem
pub fn contains<T: Hash + Eq>(set: &HashSet<T>, value: &T) -> bool {
    set.contains(value)
}

// OCaml: let singleton x : 'a t = ...
pub fn singleton<T: Hash + Eq + Clone>(value: T) -> HashSet<T> {
    let mut set = HashSet::new();
    set.insert(value);
    set
    // More idiomatic: std::iter::once(value).collect()
}

// OCaml: let add (h : 'a t) x = Hashtbl.replace h x ()
// Hashtbl.replace returns unit. HashSet::insert returns bool.
pub fn add<T: Hash + Eq + Clone>(set: &mut HashSet<T>, value: T) {
    set.insert(value); // We ignore the boolean result to match OCaml's unit return
}

// OCaml: let remove : 'a t -> 'a -> unit = Hashtbl.remove
pub fn remove<T: Hash + Eq>(set: &mut HashSet<T>, value: &T) {
    set.remove(value); // We ignore the boolean result
}

// OCaml: let iter f (h : 'a t) = Hashtbl.iter (fun a () -> f a) h
pub fn iter<'a, T, F>(set: &'a HashSet<T>, mut f: F)
where
    T: Hash + Eq,
    F: FnMut(&'a T),
{
    set.iter().for_each(|elem| f(elem));
}

// OCaml: let fold f (h : 'a t) = Hashtbl.fold (fun a _ -> f a) h
// Assuming OCaml's `f` is `key -> acc -> acc` and an initial accumulator is implied or passed elsewhere.
// Rust's `fold` is `(B, Self::Item) -> B`.
// The OCaml code `Hashtbl.fold (fun a _ -> f a) h` is unusual.
// If `f` is `('elem -> 'acc -> 'acc')`, then a standard fold signature is:
pub fn fold<'a, T, Acc, F>(set: &'a HashSet<T>, initial_accumulator: Acc, mut f: F) -> Acc
where
    T: Hash + Eq,
    F: FnMut(Acc, &'a T) -> Acc,
{
    set.iter().fold(initial_accumulator, |acc, elem| f(acc, elem))
}

// OCaml: let copy : 'a t -> 'a t = Hashtbl.copy
pub fn copy<T: Hash + Eq + Clone>(set: &HashSet<T>) -> HashSet<T> {
    set.clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet; // For direct use in tests

    #[test]
    fn test_singleton_contains_add() {
        let mut s1 = singleton(10);
        assert!(contains(&s1, &10));
        assert!(!contains(&s1, &20));
        
        add(&mut s1, 20);
        assert!(contains(&s1, &20));
        
        let s_idiomatic: HashSet<i32> = std::iter::once(10).collect();
        assert_eq!(singleton(10), s_idiomatic);
    }

    #[test]
    fn test_remove() {
        let mut s = HashSet::new();
        add(&mut s, 10);
        add(&mut s, 20);
        assert!(contains(&s, &10));
        remove(&mut s, &10);
        assert!(!contains(&s, &10));
        assert!(contains(&s, &20));
    }

    #[test]
    fn test_iter() {
        let mut s = HashSet::new();
        add(&mut s, 10);
        add(&mut s, 20);
        
        let mut sum = 0;
        iter(&s, |x| sum += x);
        assert_eq!(sum, 30); // Order doesn't matter for sum
    }

    #[test]
    fn test_fold() {
        let mut s = HashSet::new();
        add(&mut s, 10); // 1
        add(&mut s, 20); // 2
        add(&mut s, 30); // 3
        
        // Sum using fold
        let sum = fold(&s, 0, |acc, x| acc + x);
        assert_eq!(sum, 60);

        // Product using fold
        let product = fold(&s, 1, |acc, x| acc * x);
        assert_eq!(product, 10*20*30);
    }

    #[test]
    fn test_copy() {
        let mut s1 = HashSet::new();
        add(&mut s1, 10);
        add(&mut s1, 20);
        
        let s2 = copy(&s1);
        assert_eq!(s1, s2);
        // Check they are distinct copies
        add(&mut s1, 30);
        assert_ne!(s1, s2);
        assert!(contains(&s1, &30));
        assert!(!contains(&s2, &30));
    }

    #[test]
    fn test_format_hash_set() {
        let mut s = HashSet::new();
        add(&mut s, 10);
        add(&mut s, 20);
        
        // Need to sort for stable output as HashSet iteration is unordered
        let mut vec_from_set: Vec<String> = s.iter().map(|x| x.to_string()).collect();
        vec_from_set.sort();
        let formatted_sorted = vec_from_set.join(", ");

        // The format_hash_set itself doesn't sort, so we test its raw output possibility
        let formatted_raw = format_hash_set(&s, |x| x.to_string(), ", ");
        assert!(formatted_raw == "10, 20" || formatted_raw == "20, 10");
        assert_eq!(formatted_sorted, "10, 20");
    }
}
