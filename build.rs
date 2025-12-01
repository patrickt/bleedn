use std::env;
use std::path::PathBuf;

fn main() {
    let vendor_dir = PathBuf::from("vendor/edn.c");
    let src_dir = vendor_dir.join("src");
    let include_dir = vendor_dir.join("include");

    // Tell cargo to rerun if any of these change
    println!("cargo:rerun-if-changed={}", vendor_dir.display());
    println!("cargo:rerun-if-changed=wrapper.h");

    // Compile the C library
    let mut build = cc::Build::new();
    
    build
        .include(&include_dir)
        .include(&src_dir)
        .flag("-std=c11")
        .flag("-Wall")
        .flag("-Wextra")
        .flag("-Wpedantic")
        .opt_level(2);

    // Add platform-specific flags
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    match target_arch.as_str() {
        "x86_64" => {
            build.flag("-msse4.2");
        }
        "aarch64" | "arm64" => {
            // NEON is enabled by default on ARM64
        }
        _ => {}
    }

    // Add all source files
    let sources = vec![
        "src/edn.c",
        "src/arena.c",
        "src/simd.c",
        "src/string.c",
        "src/number.c",
        "src/character.c",
        "src/identifier.c",
        "src/symbolic.c",
        "src/equality.c",
        "src/uniqueness.c",
        "src/collection.c",
        "src/tagged.c",
        "src/discard.c",
        "src/reader.c",
        "src/metadata.c",
        "src/text_block.c",
        "src/newline_finder.c",
    ];

    for source in sources {
        build.file(vendor_dir.join(source));
    }

    build.compile("edn");

    // Generate bindings
    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_arg(format!("-I{}", include_dir.display()))
        .clang_arg(format!("-I{}", src_dir.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Only generate bindings for edn.h functions
        .allowlist_function("edn_.*")
        .allowlist_type("edn_.*")
        .allowlist_var("EDN_.*")
        // Generate proper Rust enums
        .rustified_enum("edn_type_t")
        .rustified_enum("edn_error_t")
        .rustified_enum("edn_default_reader_mode_t")
        // Derive common traits
        .derive_debug(true)
        .derive_default(true)
        .derive_partialeq(true)
        .derive_eq(true)
        .generate()
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
