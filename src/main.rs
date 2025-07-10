use std::{env, process};

mod jit;

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}

fn run() -> Result<(), jit::BfError> {
    let args: Vec<String> = env::args().collect();
    let program = args.get(0).map(|s| s.as_str()).unwrap_or("bfjit");
    let (no_jit, file_path) = parse_args(program, &args[1..])?;

    if no_jit {
        println!("JIT: off");
        jit::interpret_file(file_path)
    } else {
        println!("JIT: on");
        execute_jit(file_path)
    }
}

fn execute_jit(file_path: String) -> Result<(), jit::BfError> {
    let code = jit::jit_compile_file(file_path)?;
    let mut memory = vec![0u8; jit::JIT_MEMORY_CAP];
    unsafe {
        code.execute(memory.as_mut_ptr());
    }
    
    Ok(())
}

fn parse_args(program: &str, args: &[String]) -> Result<(bool, String), jit::BfError> {
    let mut no_jit = false;
    let mut file_path = None;

    for arg in args {
        match arg.as_str() {
            "--no-jit" => no_jit = true,
            path if file_path.is_none() => file_path = Some(path.to_string()),
            _ => {
                return Err(jit::BfError::Runtime(format!(
                    "Usage: {} [--no-jit] <input.bf>\nProviding several files is not supported",
                    program
                )));
            }
        }
    }

    file_path
        .ok_or_else(|| {
            jit::BfError::Runtime(format!(
                "Usage: {} [--no-jit] <input.bf>\nNo input file provided",
                program
            ))
        })
        .map(|path| (no_jit, path))
}
