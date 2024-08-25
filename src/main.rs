mod jit;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let program = &args[0];

    let mut no_jit = false;
    let mut file_path = None;

    for arg in &args[1..] {
        if arg == "--no-jit" {
            no_jit = true;
        } else {
            if file_path.is_some() {
                eprintln!("Usage: {} [--no-jit] <input.bf>", program);
                eprintln!("Providing several files is not supported");
                return Err("Invalid arguments".to_string());
            }
            file_path = Some(arg);
        }
    }

    let file_path = file_path.ok_or_else(|| {
        eprintln!("Usage: {} [--no-jit] <input.bf>", program);
        "No input is provided".to_string()
    })?;

    let ops = jit::generate_ops(file_path)?;

    if no_jit {
        println!("JIT: off");
        jit::interpret(&ops)?;
    } else {
        println!("JIT: on");
        let code = jit::jit_compile(&ops)?;
        let mut memory = vec![0u8; jit::JIT_MEMORY_CAP];
        unsafe {
            (code.run)(memory.as_mut_ptr());
        }
    }

    Ok(())
}
