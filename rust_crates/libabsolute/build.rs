fn main() {
    // Ensure LALRPOP runs before other tasks that might depend on generated code.
    // If this file is newly created, this is all it needs for now.
    // If other build steps exist, add this to them.
    
    // Only rerun build.rs if the grammar file changes.
    println!("cargo:rerun-if-changed=src/lang/libabsolute_parser.lalrpop"); 
    
    // Process the LALRPOP grammar file.
    // This generates a Rust source file in the OUT_DIR.
    lalrpop::process_root().unwrap();
}
