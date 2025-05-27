pub mod fpu_control;
pub mod misc_tools;
pub mod config_params;
pub mod arith_q;
pub mod arith_f;
pub mod arith_i;
pub mod misc_hashset;
pub mod arith_ring;
pub mod misc_kleene;
pub mod arith_polynom;
pub mod misc_cgraph;
pub mod misc_consistency;
pub mod lang;
pub mod arith_bound;
pub mod interval; // New module

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
