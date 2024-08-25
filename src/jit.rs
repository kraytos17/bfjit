use libc::{mmap, munmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE};
use std::fs;
use std::io::{self, Read, Write};
use std::mem;
use std::ptr;

pub const JIT_MEMORY_CAP: usize = 10 * 1000 * 1000;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpKind {
    Inc = b'+',
    Dec = b'-',
    Left = b'<',
    Right = b'>',
    Output = b'.',
    Input = b',',
    JumpIfZero = b'[',
    JumpIfNonZero = b']',
}

#[derive(Debug, Clone)]
pub struct Op {
    kind: OpKind,
    operand: usize,
}

pub type Ops = Vec<Op>;

pub struct Lexer<'a> {
    content: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(content: &'a [u8]) -> Self {
        Lexer { content, pos: 0 }
    }

    fn next(&mut self) -> Option<u8> {
        while self.pos < self.content.len() && !is_bf_cmd(self.content[self.pos]) {
            self.pos += 1;
        }
        if self.pos >= self.content.len() {
            None
        } else {
            let ch = self.content[self.pos];
            self.pos += 1;
            Some(ch)
        }
    }
}

fn is_bf_cmd(ch: u8) -> bool {
    b"+-<>,.[]".contains(&ch)
}

pub fn interpret(ops: &Ops) -> Result<(), String> {
    let mut memory = vec![0u8; 1];
    let mut head = 0;
    let mut ip = 0;

    while ip < ops.len() {
        let op = &ops[ip];
        match op.kind {
            OpKind::Inc => {
                memory[head] = memory[head].wrapping_add(op.operand as u8);
                ip += 1;
            }
            OpKind::Dec => {
                memory[head] = memory[head].wrapping_sub(op.operand as u8);
                ip += 1;
            }
            OpKind::Left => {
                if head < op.operand {
                    return Err("RUNTIME ERROR: Memory underflow".to_string());
                }
                head -= op.operand;
                ip += 1;
            }
            OpKind::Right => {
                head += op.operand;
                while head >= memory.len() {
                    memory.push(0);
                }
                ip += 1;
            }
            OpKind::Input => {
                for _ in 0..op.operand {
                    let mut input = [0u8; 1];
                    io::stdin()
                        .read_exact(&mut input)
                        .map_err(|e| e.to_string())?;
                    memory[head] = input[0];
                }
                ip += 1;
            }
            OpKind::Output => {
                for _ in 0..op.operand {
                    io::stdout()
                        .write_all(&[memory[head]])
                        .map_err(|e| e.to_string())?;
                }
                ip += 1;
            }
            OpKind::JumpIfZero => {
                if memory[head] == 0 {
                    ip = op.operand;
                } else {
                    ip += 1;
                }
            }
            OpKind::JumpIfNonZero => {
                if memory[head] != 0 {
                    ip = op.operand;
                } else {
                    ip += 1;
                }
            }
        }
    }

    Ok(())
}

pub struct Code {
    pub run: unsafe fn(*mut u8),
    len: usize,
}

impl Drop for Code {
    fn drop(&mut self) {
        unsafe {
            munmap(self.run as *mut _, self.len);
        }
    }
}

struct Backpatch {
    operand_byte_addr: usize,
    src_byte_addr: usize,
    dst_op_index: usize,
}

pub fn jit_compile(ops: &Ops) -> Result<Code, String> {
    let mut sb = Vec::new();
    let mut backpatches = Vec::new();
    let mut addrs = Vec::new();

    for op in ops.iter() {
        addrs.push(sb.len());
        match op.kind {
            OpKind::Inc => {
                assert!(op.operand < 256, "TODO: support bigger operands");
                sb.extend_from_slice(&[0x80, 0x07]);
                sb.push(op.operand as u8);
            }
            OpKind::Dec => {
                assert!(op.operand < 256, "TODO: support bigger operands");
                sb.extend_from_slice(&[0x80, 0x2f]);
                sb.push(op.operand as u8);
            }
            OpKind::Left => {
                sb.extend_from_slice(&[0x48, 0x81, 0xef]);
                sb.extend_from_slice(&(op.operand as u32).to_le_bytes());
            }
            OpKind::Right => {
                sb.extend_from_slice(&[0x48, 0x81, 0xc7]);
                sb.extend_from_slice(&(op.operand as u32).to_le_bytes());
            }
            OpKind::Output => {
                for _ in 0..op.operand {
                    sb.extend_from_slice(&[
                        0x57, 0x48, 0xc7, 0xc0, 0x01, 0x00, 0x00, 0x00, 0x48, 0x89, 0xfe, 0x48,
                        0xc7, 0xc7, 0x01, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc2, 0x01, 0x00, 0x00,
                        0x00, 0x0f, 0x05, 0x5f,
                    ]);
                }
            }
            OpKind::Input => {
                for _ in 0..op.operand {
                    sb.extend_from_slice(&[
                        0x57, 0x48, 0xc7, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x48, 0x89, 0xfe, 0x48,
                        0xc7, 0xc7, 0x00, 0x00, 0x00, 0x00, 0x48, 0xc7, 0xc2, 0x01, 0x00, 0x00,
                        0x00, 0x0f, 0x05, 0x5f,
                    ]);
                }
            }
            OpKind::JumpIfZero => {
                sb.extend_from_slice(&[0x8a, 0x07, 0x84, 0xc0, 0x0f, 0x84]);
                let operand_byte_addr = sb.len();
                sb.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
                let src_byte_addr = sb.len();
                backpatches.push(Backpatch {
                    operand_byte_addr,
                    src_byte_addr,
                    dst_op_index: op.operand,
                });
            }
            OpKind::JumpIfNonZero => {
                sb.extend_from_slice(&[0x8a, 0x07, 0x84, 0xc0, 0x0f, 0x85]);
                let operand_byte_addr = sb.len();
                sb.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
                let src_byte_addr = sb.len();
                backpatches.push(Backpatch {
                    operand_byte_addr,
                    src_byte_addr,
                    dst_op_index: op.operand,
                });
            }
        }
    }
    addrs.push(sb.len());

    for bp in &backpatches {
        let src_addr = bp.src_byte_addr as i32;
        let dst_addr = addrs[bp.dst_op_index] as i32;
        let operand = dst_addr - src_addr;
        sb[bp.operand_byte_addr..bp.operand_byte_addr + 4].copy_from_slice(&operand.to_le_bytes());
    }

    sb.push(0xC3);

    let code_len = sb.len();
    let code_ptr = unsafe {
        mmap(
            ptr::null_mut(),
            code_len,
            PROT_EXEC | PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0,
        )
    };

    if code_ptr == libc::MAP_FAILED {
        return Err(format!(
            "Could not allocate executable memory: {}",
            io::Error::last_os_error()
        ));
    }

    unsafe {
        ptr::copy_nonoverlapping(sb.as_ptr(), code_ptr as *mut u8, code_len);
    }

    Ok(Code {
        run: unsafe { mem::transmute::<*mut libc::c_void, unsafe fn(*mut u8)>(code_ptr) },
        len: code_len,
    })
}

pub fn generate_ops(file_path: &str) -> Result<Ops, String> {
    let content = fs::read(file_path).map_err(|e| e.to_string())?;
    let mut lexer = Lexer::new(&content);
    let mut ops = Vec::new();
    let mut stack = Vec::new();

    while let Some(c) = lexer.next() {
        match c {
            b'.' | b',' | b'<' | b'>' | b'-' | b'+' => {
                let mut count = 1;
                while let Some(s) = lexer.next() {
                    if s == c {
                        count += 1;
                    } else {
                        lexer.pos -= 1;
                        break;
                    }
                }
                ops.push(Op {
                    kind: unsafe { mem::transmute::<u8, OpKind>(c) },
                    operand: count,
                });
            }
            b'[' => {
                let addr = ops.len();
                ops.push(Op {
                    kind: OpKind::JumpIfZero,
                    operand: 0,
                });
                stack.push(addr);
            }
            b']' => {
                if let Some(addr) = stack.pop() {
                    ops.push(Op {
                        kind: OpKind::JumpIfNonZero,
                        operand: addr + 1,
                    });
                    ops[addr].operand = ops.len();
                } else {
                    return Err(format!(
                        "{} [{}]: ERROR: Unbalanced loop",
                        file_path, lexer.pos
                    ));
                }
            }
            _ => {}
        }
    }

    if !stack.is_empty() {
        return Err(format!(
            "{} [{}]: ERROR: Unbalanced loop",
            file_path, lexer.pos
        ));
    }

    Ok(ops)
}
