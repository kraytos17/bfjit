use libc::{mmap, munmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE};
use std::{
    error::Error,
    fmt, fs,
    io::{self, Read, Write},
    mem,
    path::Path,
    ptr,
};

pub const JIT_MEMORY_CAP: usize = 10 * 1000 * 1000;
const SYSCALL_READ: u8 = 0x00;
const SYSCALL_WRITE: u8 = 0x01;
const FD_STDIN: u8 = 0x00;
const FD_STDOUT: u8 = 0x01;

#[derive(Debug)]
pub enum BfError {
    Io(io::Error),
    Parse {
        file: String,
        pos: usize,
        message: String,
    },
    Runtime(String),
    Jit(String),
    InvalidOpcode {
        opcode: char,
        pos: usize,
    },
}

impl fmt::Display for BfError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BfError::Io(e) => write!(f, "IO error: {}", e),
            BfError::Parse { file, pos, message } => {
                write!(f, "{} [{}]: Parse error: {}", file, pos, message)
            }
            BfError::Runtime(msg) => write!(f, "Runtime error: {}", msg),
            BfError::Jit(msg) => write!(f, "JIT error: {}", msg),
            BfError::InvalidOpcode { opcode, pos } => {
                write!(f, "Invalid opcode '{}' at position {}", opcode, pos)
            }
        }
    }
}

impl Error for BfError {}

impl From<io::Error> for BfError {
    fn from(err: io::Error) -> Self {
        BfError::Io(err)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl OpKind {
    #[inline]
    fn is_foldable_byte(byte: u8) -> bool {
        matches!(byte, b'+' | b'-' | b'<' | b'>' | b'.' | b',')
    }
}

impl TryFrom<u8> for OpKind {
    type Error = BfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            b'+' => Ok(OpKind::Inc),
            b'-' => Ok(OpKind::Dec),
            b'<' => Ok(OpKind::Left),
            b'>' => Ok(OpKind::Right),
            b'.' => Ok(OpKind::Output),
            b',' => Ok(OpKind::Input),
            b'[' => Ok(OpKind::JumpIfZero),
            b']' => Ok(OpKind::JumpIfNonZero),
            _ => Err(BfError::InvalidOpcode {
                opcode: value as char,
                pos: 0,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Op {
    kind: OpKind,
    operand: usize,
}

pub struct Lexer<'a> {
    content: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        Lexer { content, pos: 0 }
    }

    pub fn next(&mut self) -> Option<u8> {
        self.content.get(self.pos).copied().map(|ch| {
            self.pos += 1;
            ch
        })
    }

    pub fn peek(&self) -> Option<u8> {
        self.content.get(self.pos).copied()
    }

    pub fn position(&self) -> usize {
        self.pos
    }
}

pub struct Interpreter {
    memory: Vec<u8>,
    head: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut memory = Vec::with_capacity(30_000);
        memory.push(0);
        Self { memory, head: 0 }
    }

    pub fn interpret(&mut self, ops: &[Op]) -> Result<(), BfError> {
        let stdin = io::stdin();
        let mut stdin_lock = stdin.lock();
        let stdout = io::stdout();
        let mut stdout_lock = stdout.lock();

        let mut ip = 0;
        while ip < ops.len() {
            let op = &ops[ip];
            match op.kind {
                OpKind::Inc => {
                    self.memory[self.head] = self.memory[self.head].wrapping_add(op.operand as u8);
                    ip += 1;
                }
                OpKind::Dec => {
                    self.memory[self.head] = self.memory[self.head].wrapping_sub(op.operand as u8);
                    ip += 1;
                }
                OpKind::Left => {
                    if self.head < op.operand {
                        return Err(BfError::Runtime("Memory underflow".to_string()));
                    }
                    self.head -= op.operand;
                    ip += 1;
                }
                OpKind::Right => {
                    self.head += op.operand;
                    while self.head >= self.memory.len() {
                        self.memory.push(0);
                    }
                    ip += 1;
                }
                OpKind::Input => {
                    for _ in 0..op.operand {
                        let mut input = [0u8; 1];
                        stdin_lock.read_exact(&mut input)?;
                        self.memory[self.head] = input[0];
                    }
                    ip += 1;
                }
                OpKind::Output => {
                    for _ in 0..op.operand {
                        stdout_lock.write_all(&[self.memory[self.head]])?;
                    }
                    ip += 1;
                }
                OpKind::JumpIfZero => {
                    ip = if self.memory[self.head] == 0 {
                        op.operand
                    } else {
                        ip + 1
                    };
                }
                OpKind::JumpIfNonZero => {
                    ip = if self.memory[self.head] == 0 {
                        ip + 1
                    } else {
                        op.operand
                    };
                }
            }
        }

        stdout_lock.flush()?;
        Ok(())
    }
}

pub struct JitCode {
    run: unsafe fn(*mut u8),
    len: usize,
}

impl JitCode {
    pub unsafe fn execute(&self, memory_ptr: *mut u8) {
        (self.run)(memory_ptr);
    }
}

impl Drop for JitCode {
    fn drop(&mut self) {
        unsafe {
            munmap(self.run as *mut _, self.len);
        }
    }
}

pub struct JitCompiler;

impl JitCompiler {
    pub fn compile(ops: &[Op]) -> Result<JitCode, BfError> {
        let mut code_buffer = Vec::with_capacity(ops.len() * 32);
        let mut backpatches = Vec::with_capacity(64);
        let mut instruction_addresses = Vec::with_capacity(ops.len() + 1);

        for op in ops {
            instruction_addresses.push(code_buffer.len());
            Self::emit_instruction(op, &mut code_buffer, &mut backpatches)?;
        }
        instruction_addresses.push(code_buffer.len());

        Self::apply_backpatches(&mut code_buffer, &backpatches, &instruction_addresses);
        code_buffer.push(0xC3); // RET

        let executable_memory = Self::allocate_executable_memory(&code_buffer)?;

        Ok(JitCode {
            run: unsafe { mem::transmute(executable_memory) },
            len: code_buffer.len(),
        })
    }

    fn emit_instruction(
        op: &Op,
        buffer: &mut Vec<u8>,
        backpatches: &mut Vec<Backpatch>,
    ) -> Result<(), BfError> {
        match op.kind {
            OpKind::Inc => {
                if op.operand >= 256 {
                    return Err(BfError::Jit("Operand too large for INC".to_string()));
                }
                buffer.extend_from_slice(&[0x80, 0x07]);
                buffer.push(op.operand as u8);
            }
            OpKind::Dec => {
                if op.operand >= 256 {
                    return Err(BfError::Jit("Operand too large for DEC".to_string()));
                }
                buffer.extend_from_slice(&[0x80, 0x2F]);
                buffer.push(op.operand as u8);
            }
            OpKind::Left => {
                buffer.extend_from_slice(&[0x48, 0x81, 0xEF]);
                buffer.extend_from_slice(&(op.operand as u32).to_le_bytes());
            }
            OpKind::Right => {
                buffer.extend_from_slice(&[0x48, 0x81, 0xC7]);
                buffer.extend_from_slice(&(op.operand as u32).to_le_bytes());
            }
            OpKind::Output => {
                for _ in 0..op.operand {
                    Self::emit_syscall(buffer, SYSCALL_WRITE, FD_STDOUT);
                }
            }
            OpKind::Input => {
                for _ in 0..op.operand {
                    Self::emit_syscall(buffer, SYSCALL_READ, FD_STDIN);
                }
            }
            OpKind::JumpIfZero => {
                buffer.extend_from_slice(&[0x8A, 0x07, 0x84, 0xC0, 0x0F, 0x84]);
                let operand_addr = buffer.len();
                buffer.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
                backpatches.push(Backpatch {
                    operand_addr,
                    src_addr: buffer.len(),
                    target_idx: op.operand,
                });
            }
            OpKind::JumpIfNonZero => {
                buffer.extend_from_slice(&[0x8A, 0x07, 0x84, 0xC0, 0x0F, 0x85]);
                let operand_addr = buffer.len();
                buffer.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
                backpatches.push(Backpatch {
                    operand_addr,
                    src_addr: buffer.len(),
                    target_idx: op.operand,
                });
            }
        }
        Ok(())
    }

    fn emit_syscall(buffer: &mut Vec<u8>, syscall: u8, fd: u8) {
        buffer.extend_from_slice(&[
            0x57, // push rdi
            0x48, 0xC7, 0xC0, syscall, 0x00, 0x00, 0x00, // mov rax, syscall
            0x48, 0x89, 0xFE, // mov rsi, rdi
            0x48, 0xC7, 0xC7, fd, 0x00, 0x00, 0x00, // mov rdi, fd
            0x48, 0xC7, 0xC2, 0x01, 0x00, 0x00, 0x00, // mov rdx, 1
            0x0F, 0x05, // syscall
            0x5F, // pop rdi
        ]);
    }

    fn apply_backpatches(
        buffer: &mut Vec<u8>,
        backpatches: &[Backpatch],
        instruction_addresses: &[usize],
    ) {
        for bp in backpatches {
            let src_addr = bp.src_addr as i32;
            let target_addr = instruction_addresses[bp.target_idx] as i32;
            let offset = target_addr - src_addr;
            buffer[bp.operand_addr..bp.operand_addr + 4].copy_from_slice(&offset.to_le_bytes());
        }
    }

    fn allocate_executable_memory(code: &[u8]) -> Result<*mut libc::c_void, BfError> {
        let ptr = unsafe {
            mmap(
                ptr::null_mut(),
                code.len(),
                PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANONYMOUS,
                -1,
                0,
            )
        };

        if ptr == libc::MAP_FAILED {
            return Err(BfError::Jit(format!(
                "Failed to allocate executable memory: {}",
                io::Error::last_os_error()
            )));
        }

        unsafe {
            ptr::copy_nonoverlapping(code.as_ptr(), ptr as *mut u8, code.len());
        }

        Ok(ptr)
    }
}

struct Backpatch {
    operand_addr: usize,
    src_addr: usize,
    target_idx: usize,
}

pub struct Parser;

impl Parser {
    pub fn parse_file<P: AsRef<Path>>(path: P) -> Result<Vec<Op>, BfError> {
        let content = fs::read(path.as_ref())?;
        Self::parse_bytes(&content, path.as_ref().to_string_lossy().into_owned())
    }

    pub fn parse_bytes(content: &[u8], source_name: String) -> Result<Vec<Op>, BfError> {
        let mut lexer = Lexer::new(content);
        let mut ops = Vec::with_capacity(content.len());
        let mut loop_stack = Vec::new();

        while let Some(c) = lexer.next() {
            if OpKind::is_foldable_byte(c) {
                let kind = OpKind::try_from(c).map_err(|_| BfError::InvalidOpcode {
                    opcode: c as char,
                    pos: lexer.position(),
                })?;

                let mut count = 1;
                while lexer.peek() == Some(c) {
                    count += 1;
                    lexer.next();
                }

                ops.push(Op {
                    kind,
                    operand: count,
                });
            } else if c == b'[' {
                let addr = ops.len();
                ops.push(Op {
                    kind: OpKind::JumpIfZero,
                    operand: 0, // Will be patched later
                });
                loop_stack.push(addr);
            } else if c == b']' {
                if let Some(start_addr) = loop_stack.pop() {
                    ops.push(Op {
                        kind: OpKind::JumpIfNonZero,
                        operand: start_addr,
                    });
                    ops[start_addr].operand = ops.len();
                } else {
                    return Err(BfError::Parse {
                        file: source_name,
                        pos: lexer.position(),
                        message: "Unmatched closing bracket".to_string(),
                    });
                }
            }
        }

        if !loop_stack.is_empty() {
            return Err(BfError::Parse {
                file: source_name,
                pos: lexer.position(),
                message: "Unmatched opening bracket".to_string(),
            });
        }

        Ok(ops)
    }
}

pub fn interpret_file<P: AsRef<Path>>(path: P) -> Result<(), BfError> {
    let ops = Parser::parse_file(path)?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&ops)
}

pub fn jit_compile_file<P: AsRef<Path>>(path: P) -> Result<JitCode, BfError> {
    let ops = Parser::parse_file(path)?;
    JitCompiler::compile(&ops)
}
