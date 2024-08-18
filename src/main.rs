use clap::Parser;
use jit::{Lexer, Op, OpKind, Ops};
use std::{fs, io, path::PathBuf};

mod jit;

#[derive(Debug, Parser)]
#[clap(version)]
struct Opt {
    #[clap(name = "FILE")]
    file_path: PathBuf,
    // #[clap(long = "no-jit", help = "Disable JIT compilation")]
    // no_jit: bool,
}

fn main() -> io::Result<()> {
    let opt = Opt::parse();
    let content = fs::read_to_string(&opt.file_path)?;
    let mut lexer = Lexer::new(content.as_bytes());
    let mut ops = Ops::new();

    while let Some(ch) = lexer.next() {
        let current_op_kind = match ch {
            b'+' => OpKind::Inc,
            b'-' => OpKind::Dec,
            b'<' => OpKind::Left,
            b'>' => OpKind::Right,
            b'.' => OpKind::Output,
            b',' => OpKind::Input,
            b'[' => OpKind::JumpIfZero,
            b']' => OpKind::JumpIfNonZero,
            _ => continue,
        };

        if let Some(last_op) = ops.items.last_mut() {
            if last_op.kind == current_op_kind {
                last_op.operand += 1;
                continue;
            }
        }

        let new_op = Op {
            kind: current_op_kind,
            operand: 1,
        };

        ops.push(new_op);
    }

    for op in ops.items.iter() {
        let kind_char = Into::<u8>::into(op.kind) as char;
        println!("OpKind: {}, Operand: {}", kind_char, op.operand);
    }

    if !jit::interpret(ops) {
        eprintln!("Failed to interpret operations");
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Interpretation failed",
        ));
    }

    Ok(())
}
