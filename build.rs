use std::env;
use std::fs;
use std::path::Path;

fn main() {
    // Paths
    let error_codes_dir = Path::new("error_codes"); // Folder containing error code .md files
    let destination_file = Path::new("src/error_docs.rs"); // Target file in src/

    // Start generating the Rust source code
    let mut generated_code = String::new();
    generated_code.push_str("use std::collections::HashMap;\n\n");
    generated_code.push_str("pub fn get_error_docs() -> HashMap<&'static str, &'static str> {\n");
    generated_code.push_str("    let mut docs = HashMap::new();\n");

    // Iterate over all .md files in the error_codes folder
    for entry in fs::read_dir(error_codes_dir).expect("Failed to read error_codes directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // Only process .md files
        if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("md") {
            // Get the file stem (e.g., "E0001" from "E0001.md")
            let file_name = path.file_stem().unwrap().to_str().unwrap();

            // Construct a relative path for `include_str!`
            let relative_path = path
                .canonicalize() // Converts to absolute path
                .unwrap()
                .strip_prefix(env::current_dir().unwrap().canonicalize().unwrap()) // Strip project root
                .unwrap()
                .to_str()
                .unwrap()
                .replace("\\", "/"); // Ensure forward slashes

            // Add a line to embed the file
            generated_code.push_str(&format!(
                "    docs.insert(\"{}\", include_str!(\"../{}\"));\n",
                file_name, relative_path
            ));
        }
    }

    generated_code.push_str("    docs\n");
    generated_code.push_str("}\n");

    // Write the generated code to the destination file
    fs::create_dir_all(destination_file.parent().unwrap()).expect("Failed to create src directory");
    fs::write(destination_file, generated_code).expect("Failed to write error_docs.rs");

    // Print cargo instruction to rerun this script if anything in error_codes changes
    println!("cargo:rerun-if-changed=error_codes");
}
