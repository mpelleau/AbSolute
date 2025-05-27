use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display};
use rand::seq::SliceRandom; // For list_pick

#[derive(Debug, Clone, Copy)]
pub struct BotFound;

impl Display for BotFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Bot_found")
    }
}
impl std::error::Error for BotFound {}


#[derive(Debug, Clone, Copy)]
pub struct TopFound;

impl Display for TopFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Top_found")
    }
}
impl std::error::Error for TopFound {}

// Simplified inplace_print module
pub mod inplace_print {
    // Simplified version: just prints the new string on a new line.
    // True in-place printing is complex and better handled by dedicated TUI libraries.
    pub fn inplace_print_generic(s: &str) {
        println!("{}", s);
    }
}

pub fn format_list_sep<T, F>(list: &[T], separator: &str, mut format_item: F) -> String
where
    F: FnMut(&T) -> String,
{
    list.iter()
        .map(|item| format_item(item))
        .collect::<Vec<String>>()
        .join(separator)
}

pub fn swap_pair<A, B>((a, b): (A, B)) -> (B, A) {
    (b, a)
}

pub fn fold_on_combination_2<T, Acc, F>(
    arr: &[T],
    initial_acc: Acc,
    allow_duplicates: bool,
    mut f: F,
) -> Acc
where
    F: FnMut(Acc, &T, &T) -> Acc,
{
    let l = arr.len();
    let mut acc = initial_acc;
    if l == 0 {
        return acc;
    }

    for i1 in 0..l {
        let start_i2 = if allow_duplicates { i1 } else { i1 + 1 };
        for i2 in start_i2..l {
             if !allow_duplicates && i1 == i2 { 
                continue;
            }
            acc = f(acc, &arr[i1], &arr[i2]);
        }
    }
    acc
}

pub fn meet_option<T, U, R, F>(opt_t: Option<T>, opt_u: Option<U>, f: F) -> Option<R>
where
    F: FnOnce(T, U) -> R,
{
    opt_t.and_then(|t_val| opt_u.map(|u_val| f(t_val, u_val)))
}

pub fn merge_options<T, U>(opt_t: Option<T>, opt_u: Option<U>) -> Option<(T, U)> {
    opt_t.and_then(|t_val| opt_u.map(|u_val| (t_val, u_val)))
}

pub fn join_options<T, F>(opt_a: Option<T>, opt_b: Option<T>, f: F) -> Option<T>
where
    F: FnOnce(T, T) -> T,
{
    match (opt_a, opt_b) {
        (None, b) => b,
        (a, None) => a,
        (Some(a_val), Some(b_val)) => Some(f(a_val, b_val)),
    }
}

pub mod var_map {
    use super::*; 
    use std::collections::BTreeMap; // Ensure BTreeMap is imported here too

    pub fn find_fail<V: Clone>(key: &str, map: &BTreeMap<String, V>) -> V {
        map.get(key)
            .cloned()
            .unwrap_or_else(|| panic!("key not found: {}", key))
    }

    pub fn of_list<V>(assoc: Vec<(String, V)>) -> BTreeMap<String, V> {
        assoc.into_iter().collect()
    }

    pub fn keys<V>(map: &BTreeMap<String, V>) -> Vec<String> {
        map.keys().cloned().collect()
    }

    pub fn format_map<V, FmtVal>(
        map: &BTreeMap<String, V>,
        mut format_val: FmtVal,
        separator: &str,
    ) -> String
    where
        FmtVal: FnMut(&V) -> String,
    {
        map.iter()
            .map(|(k, v)| format!("{}: {}", k, format_val(v)))
            .collect::<Vec<String>>()
            .join(separator)
    }
}

pub fn list_pick<T>(list: &[T]) -> Option<&T> {
    if list.is_empty() {
        None
    } else {
        list.choose(&mut rand::thread_rng())
    }
}

pub mod var_set {
    use super::*; 
    use std::collections::BTreeSet; // Ensure BTreeSet is imported here too

    pub fn format_set<FmtItem>(
        set: &BTreeSet<String>,
        mut format_item: FmtItem,
        separator: &str,
    ) -> String
    where
        FmtItem: FnMut(&String) -> String,
    {
        set.iter()
            .map(|item| format_item(item))
            .collect::<Vec<String>>()
            .join(separator)
    }
    pub fn of_list(list: Vec<String>) -> BTreeSet<String> {
        list.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap; // For tests using BTreeMap directly

    #[test]
    fn test_swap_pair() {
        assert_eq!(swap_pair((1, "hello")), ("hello", 1));
    }

    #[test]
    fn test_fold_on_combination_2_no_duplicates() {
        let arr = [1, 2, 3, 4];
        let result = fold_on_combination_2(&arr, 0, false, |acc, &x, &y| acc + x * y);
        // 1*2 + 1*3 + 1*4 + 2*3 + 2*4 + 3*4 = 2 + 3 + 4 + 6 + 8 + 12 = 35
        assert_eq!(result, 35);
    }

    #[test]
    fn test_fold_on_combination_2_with_duplicates() {
        let arr = [1, 2, 3];
        let result = fold_on_combination_2(&arr, 0, true, |acc, &x, &y| acc + x * y);
        // 1*1 + 1*2 + 1*3 + 2*2 + 2*3 + 3*3 = 1 + 2 + 3 + 4 + 6 + 9 = 25
        assert_eq!(result, 25);
    }
    
    #[test]
    fn test_fold_on_combination_2_empty() {
        let arr: [i32; 0] = [];
        let result = fold_on_combination_2(&arr, 0, false, |acc, &x, &y| acc + x * y);
        assert_eq!(result, 0);
    }

    #[test]
    fn test_meet_option() {
        let res: Option<i32> = meet_option(Some(5), Some(3), |x, y| x + y);
        assert_eq!(res, Some(8));
        let res_none: Option<i32> = meet_option(None, Some(3), |x, y| x + y);
        assert_eq!(res_none, None);
    }

    #[test]
    fn test_merge_options() {
        assert_eq!(merge_options(Some(5), Some("hi")), Some((5, "hi")));
        assert_eq!(merge_options::<i32, &str>(None, Some("hi")), None);
    }

    #[test]
    fn test_join_options() {
        let res: Option<i32> = join_options(Some(5), Some(3), |x, y| x + y);
        assert_eq!(res, Some(8));
        assert_eq!(join_options(None, Some(3), |x: i32, y: i32| x + y), Some(3));
        assert_eq!(join_options(Some(5), None, |x: i32, y: i32| x + y), Some(5));
        assert_eq!(join_options(None, None, |x: i32, y: i32| x + y), None);
    }

    #[test]
    fn test_var_map_ops() {
        let mut map = BTreeMap::new(); // Test BTreeMap directly
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);

        assert_eq!(var_map::find_fail("a", &map), 1);
        
        let map_from_list = var_map::of_list(vec![("x".to_string(), 10), ("y".to_string(), 20)]);
        assert_eq!(map_from_list.get("x"), Some(&10));

        let keys = var_map::keys(&map_from_list);
        assert_eq!(keys, vec!["x".to_string(), "y".to_string()]);

        let formatted = var_map::format_map(&map_from_list, |v| v.to_string(), "; ");
        // BTreeMap iterates in sorted key order
        assert_eq!(formatted, "x: 10; y: 20");
    }
    
    #[test]
    #[should_panic(expected = "key not found: z")]
    fn test_var_map_find_fail_panic() {
        let map: BTreeMap<String, i32> = BTreeMap::new(); // Test BTreeMap directly
        var_map::find_fail("z", &map);
    }

    #[test]
    fn test_list_pick() {
        let list1 = [1, 2, 3, 4, 5];
        for _ in 0..10 { 
            if let Some(picked) = list_pick(&list1) {
                assert!(list1.contains(picked));
            } else {
                panic!("Should have picked an element");
            }
        }
        let list_empty: [i32;0] = [];
        assert_eq!(list_pick(&list_empty), None);
    }

    #[test]
    fn test_var_set_ops() {
        let data = vec!["a".to_string(), "b".to_string(), "a".to_string()];
        let set = var_set::of_list(data);
        let mut expected = BTreeSet::new(); // Test BTreeSet directly
        expected.insert("a".to_string());
        expected.insert("b".to_string());
        assert_eq!(set, expected);

        let formatted = var_set::format_set(&set, |s| s.to_uppercase(), ", ");
        // BTreeSet iterates in sorted order
        assert_eq!(formatted, "A, B"); 
    }
}
