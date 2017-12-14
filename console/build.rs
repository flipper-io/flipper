fn main() {
    // Specify the path to libflipper.so
    let console_root = env!("CARGO_MANIFEST_DIR");
    println!(r"cargo:rustc-link-search={}/../build/libflipper", console_root);
}