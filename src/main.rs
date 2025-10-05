//! # Brainf*ck Interpreter and JIT Compiler CLI
//!
//! A command-line interface for executing Brainf*ck programs with optional JIT compilation.
//!
//! ## Usage
//! ```bash
//! bfjit [--no-jit] <input.bf>
//! ```
//!
//! ## Options
//! - `--no-jit`: Disable JIT compilation and use the interpreter instead
//! - `<input.bf>`: Path to the Brainf*ck source file to execute
//!
//! ## Examples
//! ```bash
//! # Execute with JIT compilation (default, fastest)
//! bfjit hello_world.bf
//!
//! # Execute with interpreter (slower, but useful for debugging)
//! bfjit --no-jit hello_world.bf
//! ```
//!
//! ## Performance
//! - **JIT Mode**: Compiles Brainf*ck to native x86-64 machine code for maximum performance
//! - **Interpreter Mode**: Executes Brainf*ck operations directly, slower but more portable
//!
//! The JIT compiler typically provides 10-100x performance improvement over interpretation,
//! making it suitable for computationally intensive Brainf*ck programs.

use std::{env, process};

mod jit;

/// Entry point of the Brainf*ck CLI application
///
/// Handles command-line argument parsing and executes the appropriate execution mode.
/// Exits with status code 1 on any error, printing the error message to stderr.
fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {e}");
        process::exit(1);
    }
}

/// Main application logic with error handling
///
/// Parses command-line arguments and dispatches to either the interpreter
/// or JIT compiler based on user preferences.
///
/// # Returns
/// * `Ok(())` - Successful execution of the Brainf*ck program
/// * `Err(BfError)` - Error during argument parsing, file reading, compilation, or execution
///
/// # Error Handling
/// All errors are propagated up to `main()` where they are displayed to the user
/// and the program exits with a non-zero status code.
fn run() -> Result<(), jit::BfError> {
    let args: Vec<String> = env::args().collect();
    let program = args.first().map_or("bfjit", |s| s.as_str());
    let (no_jit, file_path) = parse_args(program, &args[1..])?;

    if no_jit {
        println!("JIT: off");
        jit::interpret_file(file_path)
    } else {
        println!("JIT: on");
        execute_jit(file_path)
    }
}

/// Executes a Brainf*ck program using JIT compilation
///
/// This function:
/// 1. Compiles the Brainf*ck source file to native x86-64 machine code
/// 2. Allocates a memory buffer for the Brainf*ck program's execution
/// 3. Executes the compiled code with the allocated memory
///
/// # Arguments
/// * `file_path` - Path to the Brainf*ck source file
///
/// # Returns
/// * `Ok(())` - Successful compilation and execution
/// * `Err(BfError)` - Error during file reading, compilation, or memory allocation
///
/// # Memory Management
/// Allocates a large contiguous memory buffer (`JIT_MEMORY_CAP` bytes) for the
/// Brainf*ck program's data tape. This memory is automatically freed when the
/// vector goes out of scope.
///
/// # Safety
/// Uses unsafe code to pass a raw pointer to the compiled machine code.
/// The JIT compiler ensures the generated code respects memory boundaries,
/// but the caller must ensure the memory buffer is large enough for the program's needs.
fn execute_jit(file_path: String) -> Result<(), jit::BfError> {
    let code = jit::jit_compile_file(file_path)?;
    let mut memory = vec![0u8; jit::JIT_MEMORY_CAP];
    code.execute(&mut memory);

    Ok(())
}

/// Parses command-line arguments and validates them
///
/// Supports the following argument patterns:
/// - `program <file.bf>` - Execute with JIT enabled
/// - `program --no-jit <file.bf>` - Execute with interpreter only
///
/// # Arguments
/// * `program` - Name of the program (used in error messages)
/// * `args` - Command-line arguments (excluding program name)
///
/// # Returns
/// * `Ok((no_jit, file_path))` - Successfully parsed arguments
///   - `no_jit`: true if `--no-jit` flag was provided
///   - `file_path`: path to the Brainf*ck source file
/// * `Err(BfError::Runtime)` - Invalid arguments or usage
///
/// # Error Conditions
/// - No input file provided
/// - Multiple input files provided (not supported)
/// - Unknown command-line flags
/// - Invalid argument combinations
///
/// # Examples
/// ```rust
/// // Valid argument combinations:
/// parse_args("bfjit", &["hello.bf"]) // -> Ok((false, "hello.bf"))
/// parse_args("bfjit", &["--no-jit", "hello.bf"]) // -> Ok((true, "hello.bf"))
///
/// // Invalid argument combinations:
/// parse_args("bfjit", &[]) // -> Err("No input file provided")
/// parse_args("bfjit", &["file1.bf", "file2.bf"]) // -> Err("Providing several files is not supported")
/// ```
fn parse_args(program: &str, args: &[String]) -> Result<(bool, String), jit::BfError> {
    let mut no_jit = false;
    let mut file_path = None;

    // Process each argument in order
    for arg in args {
        match arg.as_str() {
            "--no-jit" => {
                // Flag to disable JIT compilation
                no_jit = true;
            }
            path if file_path.is_none() => {
                // First non-flag argument is treated as the input file
                file_path = Some(path.to_string());
            }
            _ => {
                // Multiple files or unknown flags are not supported
                return Err(jit::BfError::Runtime(format!(
                    "Usage: {program} [--no-jit] <input.bf>\nProviding several files is not supported"
                )));
            }
        }
    }

    // Ensure an input file was provided
    file_path
        .ok_or_else(|| {
            jit::BfError::Runtime(format!(
                "Usage: {program} [--no-jit] <input.bf>\nNo input file provided"
            ))
        })
        .map(|path| (no_jit, path))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_args_basic_file() {
        let result = parse_args("bfjit", &["hello.bf".to_string()]);
        assert!(result.is_ok());
        let (no_jit, file_path) = result.unwrap();
        assert!(!no_jit);
        assert_eq!(file_path, "hello.bf");
    }

    #[test]
    fn test_parse_args_with_no_jit() {
        let result = parse_args("bfjit", &["--no-jit".to_string(), "hello.bf".to_string()]);
        assert!(result.is_ok());
        let (no_jit, file_path) = result.unwrap();
        assert!(no_jit);
        assert_eq!(file_path, "hello.bf");
    }

    #[test]
    fn test_parse_args_no_file() {
        let result = parse_args("bfjit", &[]);
        assert!(result.is_err());
        if let Err(jit::BfError::Runtime(msg)) = result {
            assert!(msg.contains("No input file provided"));
        }
    }

    #[test]
    fn test_parse_args_multiple_files() {
        let result = parse_args("bfjit", &["file1.bf".to_string(), "file2.bf".to_string()]);
        assert!(result.is_err());
        if let Err(jit::BfError::Runtime(msg)) = result {
            assert!(msg.contains("several files is not supported"));
        }
    }

    #[test]
    fn test_parse_args_no_jit_flag_order() {
        // Test that --no-jit can come after the filename (flexible ordering)
        let result = parse_args("bfjit", &["hello.bf".to_string(), "--no-jit".to_string()]);
        assert!(result.is_ok());
        let (no_jit, file_path) = result.unwrap();
        assert!(no_jit);
        assert_eq!(file_path, "hello.bf");
    }
}
