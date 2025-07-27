//! # Brainf*ck Interpreter and JIT Compiler
//!
//! This module provides a complete implementation of a Brainf*ck interpreter with JIT compilation support.
//! It includes lexical analysis, parsing, interpretation, and x86-64 JIT compilation capabilities.
//!
//! ## Features
//! - Optimized parsing with instruction folding (consecutive operations are combined)
//! - Memory-safe interpreter implementation
//! - x86-64 JIT compiler for high-performance execution
//! - Comprehensive error handling
//! - Support for standard Brainf*ck operations: `+ - < > . , [ ]`

use libc::{MAP_ANONYMOUS, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE, mmap, munmap};
use std::{
    error::Error,
    fmt, fs,
    io::{self, Read, Write},
    mem,
    path::Path,
    ptr,
};

/// Maximum amount of memory (in bytes) that can be allocated for JIT compilation
pub const JIT_MEMORY_CAP: usize = 10 * 1000 * 1000;

/// System call number for read operations
const SYSCALL_READ: u8 = 0x00;
/// System call number for write operations
const SYSCALL_WRITE: u8 = 0x01;
/// File descriptor for standard input
const FD_STDIN: u8 = 0x00;
/// File descriptor for standard output
const FD_STDOUT: u8 = 0x01;

/// Comprehensive error type for all Brainf*ck interpreter operations
#[derive(Debug)]
pub enum BfError {
    /// I/O related errors (file reading, stdin/stdout operations)
    Io(io::Error),
    /// Parse errors with file context and position information
    Parse {
        file: String,
        pos: usize,
        message: String,
    },
    /// Runtime errors during interpretation (e.g., memory underflow)
    Runtime(String),
    /// JIT compilation errors
    Jit(String),
    /// Invalid Brainf*ck opcode encountered
    InvalidOpcode { opcode: char, pos: usize },
}

impl fmt::Display for BfError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(e) => write!(f, "IO error: {e}"),
            Self::Parse { file, pos, message } => {
                write!(f, "{file} [{pos}]: Parse error: {message}")
            }
            Self::Runtime(msg) => write!(f, "Runtime error: {msg}"),
            Self::Jit(msg) => write!(f, "JIT error: {msg}"),
            Self::InvalidOpcode { opcode, pos } => {
                write!(f, "Invalid opcode '{opcode}' at position {pos}")
            }
        }
    }
}

impl Error for BfError {}

impl From<io::Error> for BfError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

/// Represents the eight Brainf*ck operations
///
/// Each variant corresponds to a Brainf*ck command:
/// - `+`: Increment the byte at the data pointer
/// - `-`: Decrement the byte at the data pointer  
/// - `<`: Move the data pointer left
/// - `>`: Move the data pointer right
/// - `.`: Output the byte at the data pointer
/// - `,`: Input a byte and store it at the data pointer
/// - `[`: Jump forward to the command after the matching `]` if the byte at the data pointer is 0
/// - `]`: Jump back to the command after the matching `[` if the byte at the data pointer is nonzero
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
    /// Checks if a byte represents an operation that can be folded (combined with consecutive identical operations)
    ///
    /// Foldable operations are: `+`, `-`, `<`, `>`, `.`, `,`
    /// Jump operations `[` and `]` are not foldable as they have specific control flow semantics
    #[inline]
    const fn is_foldable_byte(byte: u8) -> bool {
        matches!(byte, b'+' | b'-' | b'<' | b'>' | b'.' | b',')
    }
}

impl TryFrom<u8> for OpKind {
    type Error = BfError;

    /// Converts a byte to an `OpKind`, returning an error for invalid opcodes
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            b'+' => Ok(Self::Inc),
            b'-' => Ok(Self::Dec),
            b'<' => Ok(Self::Left),
            b'>' => Ok(Self::Right),
            b'.' => Ok(Self::Output),
            b',' => Ok(Self::Input),
            b'[' => Ok(Self::JumpIfZero),
            b']' => Ok(Self::JumpIfNonZero),
            _ => Err(BfError::InvalidOpcode {
                opcode: value as char,
                pos: 0,
            }),
        }
    }
}

/// Represents a single Brainf*ck operation with an optional operand
///
/// The operand serves different purposes depending on the operation:
/// - For `+`, `-`, `<`, `>`, `.`, `,`: Number of times to repeat the operation (optimization)
/// - For `[`: Index of the matching `]` instruction (for jump targets)
/// - For `]`: Index of the matching `[` instruction (for jump targets)
#[derive(Debug, Clone)]
pub struct Op {
    kind: OpKind,
    operand: usize,
}

/// Lexical analyzer for Brainf*ck source code
///
/// Provides character-by-character parsing with position tracking for error reporting
pub struct Lexer<'a> {
    content: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer for the given content
    pub const fn new(content: &'a [u8]) -> Self {
        Lexer { content, pos: 0 }
    }

    /// Consumes and returns the next character, advancing the position
    pub fn next(&mut self) -> Option<u8> {
        self.content.get(self.pos).copied().inspect(|_| {
            self.pos += 1;
        })
    }

    /// Returns the next character without consuming it
    pub fn peek(&self) -> Option<u8> {
        self.content.get(self.pos).copied()
    }

    /// Returns the current position in the source code
    pub const fn position(&self) -> usize {
        self.pos
    }
}

/// Brainf*ck interpreter with a dynamic memory model
///
/// The interpreter maintains a memory tape that grows dynamically as needed.
/// Memory cells are 8-bit unsigned integers that wrap on overflow/underflow.
pub struct Interpreter {
    /// The memory tape, grows dynamically as needed
    memory: Vec<u8>,
    /// Current position in the memory tape (data pointer)
    head: usize,
}

impl Interpreter {
    /// Creates a new interpreter with initial memory capacity
    ///
    /// Starts with a single memory cell initialized to 0
    pub fn new() -> Self {
        let mut memory = Vec::with_capacity(30_000);
        memory.push(0);
        Self { memory, head: 0 }
    }

    /// Executes a sequence of Brainf*ck operations
    ///
    /// # Arguments
    /// * `ops` - Slice of parsed operations to execute
    ///
    /// # Returns
    /// * `Ok(())` on successful execution
    /// * `Err(BfError)` on runtime errors (memory underflow, I/O errors)
    pub fn interpret(&mut self, ops: &[Op]) -> Result<(), BfError> {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut stdout_lock = stdout.lock();

        let mut ip = 0; // Instruction pointer
        while ip < ops.len() {
            let op = &ops[ip];
            match op.kind {
                OpKind::Inc => {
                    // Increment the current memory cell, with wrapping arithmetic
                    self.memory[self.head] =
                        self.memory[self.head].wrapping_add(u8::try_from(op.operand).unwrap());
                    ip += 1;
                }
                OpKind::Dec => {
                    // Decrement the current memory cell, with wrapping arithmetic
                    self.memory[self.head] =
                        self.memory[self.head].wrapping_sub(u8::try_from(op.operand).unwrap());
                    ip += 1;
                }
                OpKind::Left => {
                    // Move data pointer left, check for underflow
                    if self.head < op.operand {
                        return Err(BfError::Runtime("Memory underflow".to_string()));
                    }
                    self.head -= op.operand;
                    ip += 1;
                }
                OpKind::Right => {
                    // Move data pointer right, extend memory if needed
                    self.head += op.operand;
                    while self.head >= self.memory.len() {
                        self.memory.push(0);
                    }
                    ip += 1;
                }
                OpKind::Input => {
                    // Read bytes from stdin
                    for _ in 0..op.operand {
                        let mut input = [0u8; 1];
                        stdin.lock().read_exact(&mut input)?;
                        self.memory[self.head] = input[0];
                    }
                    ip += 1;
                }
                OpKind::Output => {
                    // Write bytes to stdout
                    for _ in 0..op.operand {
                        stdout_lock.write_all(&[self.memory[self.head]])?;
                    }
                    ip += 1;
                }
                OpKind::JumpIfZero => {
                    // Jump forward if current cell is zero
                    ip = if self.memory[self.head] == 0 {
                        op.operand
                    } else {
                        ip + 1
                    };
                }
                OpKind::JumpIfNonZero => {
                    // Jump backward if current cell is non-zero
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

/// Represents compiled JIT code that can be executed
///
/// This struct manages executable memory allocated via mmap and provides
/// a safe interface for executing JIT-compiled Brainf*ck code.
pub struct JitCode {
    /// Function pointer to the compiled code
    run: unsafe fn(*mut u8),
    /// Length of the allocated memory region
    len: usize,
}

impl JitCode {
    /// Executes the JIT-compiled code with the given memory pointer
    ///
    /// # Safety
    /// The caller must ensure that `memory_ptr` points to valid, writable memory
    /// of sufficient size for the Brainf*ck program's execution.
    ///
    /// # Arguments
    /// * `memory_ptr` - Pointer to the Brainf*ck memory tape
    pub unsafe fn execute(&self, memory_ptr: *mut u8) {
        unsafe {
            (self.run)(memory_ptr);
        }
    }
}

impl Drop for JitCode {
    /// Automatically deallocates the executable memory when `JitCode` is dropped
    fn drop(&mut self) {
        unsafe {
            munmap(self.run as *mut _, self.len);
        }
    }
}

/// JIT compiler for Brainf*ck that generates x86-64 machine code
///
/// The compiler translates Brainf*ck operations into native x86-64 assembly
/// instructions for maximum performance. It handles jump patching for loops
/// and generates optimized code for repeated operations.
pub struct JitCompiler;

impl JitCompiler {
    /// Compiles a sequence of Brainf*ck operations into executable machine code
    ///
    /// # Arguments
    /// * `ops` - Slice of parsed Brainf*ck operations
    ///
    /// # Returns
    /// * `Ok(JitCode)` - Successfully compiled executable code
    /// * `Err(BfError)` - Compilation or memory allocation error
    pub fn compile(ops: &[Op]) -> Result<JitCode, BfError> {
        let mut code_buffer = Vec::with_capacity(ops.len() * 32);
        let mut backpatches = Vec::with_capacity(64);
        let mut instruction_addresses = Vec::with_capacity(ops.len() + 1);

        // Generate machine code for each operation
        for op in ops {
            instruction_addresses.push(code_buffer.len());
            Self::emit_instruction(op, &mut code_buffer, &mut backpatches)?;
        }
        instruction_addresses.push(code_buffer.len());

        // Resolve jump addresses
        Self::apply_backpatches(&mut code_buffer, &backpatches, &instruction_addresses);

        // Add return instruction
        code_buffer.push(0xC3); // RET

        let executable_memory = Self::allocate_executable_memory(&code_buffer)?;

        Ok(JitCode {
            run: unsafe {
                mem::transmute::<*mut libc::c_void, unsafe fn(*mut u8)>(executable_memory)
            },
            len: code_buffer.len(),
        })
    }

    /// Generates x86-64 machine code for a single Brainf*ck operation
    ///
    /// # Arguments
    /// * `op` - The operation to compile
    /// * `buffer` - Code buffer to append machine code to
    /// * `backpatches` - Vector to store jump targets that need to be resolved later
    fn emit_instruction(
        op: &Op,
        buffer: &mut Vec<u8>,
        backpatches: &mut Vec<Backpatch>,
    ) -> Result<(), BfError> {
        match op.kind {
            OpKind::Inc => {
                // ADD BYTE PTR [rdi], operand
                if op.operand >= 256 {
                    return Err(BfError::Jit("Operand too large for INC".to_string()));
                }
                buffer.extend_from_slice(&[0x80, 0x07]);
                buffer.push(u8::try_from(op.operand).unwrap());
            }
            OpKind::Dec => {
                // SUB BYTE PTR [rdi], operand
                if op.operand >= 256 {
                    return Err(BfError::Jit("Operand too large for DEC".to_string()));
                }
                buffer.extend_from_slice(&[0x80, 0x2F]);
                buffer.push(u8::try_from(op.operand).unwrap());
            }
            OpKind::Left => {
                // SUB rdi, operand (move pointer left)
                buffer.extend_from_slice(&[0x48, 0x81, 0xEF]);
                buffer.extend_from_slice(&(u32::try_from(op.operand).unwrap()).to_le_bytes());
            }
            OpKind::Right => {
                // ADD rdi, operand (move pointer right)
                buffer.extend_from_slice(&[0x48, 0x81, 0xC7]);
                buffer.extend_from_slice(&(u32::try_from(op.operand).unwrap()).to_le_bytes());
            }
            OpKind::Output => {
                // Emit system call for each output operation
                for _ in 0..op.operand {
                    Self::emit_syscall(buffer, SYSCALL_WRITE, FD_STDOUT);
                }
            }
            OpKind::Input => {
                // Emit system call for each input operation
                for _ in 0..op.operand {
                    Self::emit_syscall(buffer, SYSCALL_READ, FD_STDIN);
                }
            }
            OpKind::JumpIfZero => {
                // MOV al, BYTE PTR [rdi]; TEST al, al; JE target
                buffer.extend_from_slice(&[0x8A, 0x07, 0x84, 0xC0, 0x0F, 0x84]);
                let operand_addr = buffer.len();
                buffer.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]); // Placeholder for jump offset
                backpatches.push(Backpatch {
                    operand_addr,
                    src_addr: buffer.len(),
                    target_idx: op.operand,
                });
            }
            OpKind::JumpIfNonZero => {
                // MOV al, BYTE PTR [rdi]; TEST al, al; JNE target
                buffer.extend_from_slice(&[0x8A, 0x07, 0x84, 0xC0, 0x0F, 0x85]);
                let operand_addr = buffer.len();
                buffer.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]); // Placeholder for jump offset
                backpatches.push(Backpatch {
                    operand_addr,
                    src_addr: buffer.len(),
                    target_idx: op.operand,
                });
            }
        }
        Ok(())
    }

    /// Generates x86-64 system call code for I/O operations
    ///
    /// # Arguments
    /// * `buffer` - Code buffer to append to
    /// * `syscall` - System call number (read/write)
    /// * `fd` - File descriptor (stdin/stdout)
    fn emit_syscall(buffer: &mut Vec<u8>, syscall: u8, fd: u8) {
        buffer.extend_from_slice(&[
            0x57, // PUSH rdi (save memory pointer)
            0x48, 0xC7, 0xC0, syscall, 0x00, 0x00, 0x00, // MOV rax, syscall
            0x48, 0x89, 0xFE, // MOV rsi, rdi (buffer = memory pointer)
            0x48, 0xC7, 0xC7, fd, 0x00, 0x00, 0x00, // MOV rdi, fd (file descriptor)
            0x48, 0xC7, 0xC2, 0x01, 0x00, 0x00, 0x00, // MOV rdx, 1 (count = 1 byte)
            0x0F, 0x05, // SYSCALL
            0x5F, // POP rdi (restore memory pointer)
        ]);
    }

    /// Resolves jump addresses by patching placeholder offsets with actual values
    ///
    /// # Arguments
    /// * `buffer` - Code buffer containing the machine code
    /// * `backpatches` - Vector of jump locations to patch
    /// * `instruction_addresses` - Array mapping instruction indices to code buffer positions
    fn apply_backpatches(
        buffer: &mut [u8],
        backpatches: &[Backpatch],
        instruction_addresses: &[usize],
    ) {
        for bp in backpatches {
            let src_addr = i32::try_from(bp.src_addr).unwrap();
            let target_addr = i32::try_from(instruction_addresses[bp.target_idx]).unwrap();
            let offset = target_addr - src_addr;
            buffer[bp.operand_addr..bp.operand_addr + 4].copy_from_slice(&offset.to_le_bytes());
        }
    }

    /// Allocates executable memory and copies the generated code into it
    ///
    /// # Arguments
    /// * `code` - The machine code to make executable
    ///
    /// # Returns
    /// * `Ok(ptr)` - Pointer to executable memory
    /// * `Err(BfError)` - Memory allocation failure
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
            ptr::copy_nonoverlapping(code.as_ptr(), ptr.cast::<u8>(), code.len());
        }

        Ok(ptr)
    }
}

/// Represents a jump instruction that needs address patching during compilation
///
/// Used internally by the JIT compiler to track locations where jump offsets
/// need to be filled in once all instruction addresses are known.
struct Backpatch {
    /// Address in code buffer where the operand (jump offset) is stored
    operand_addr: usize,
    /// Address immediately after the jump instruction (for calculating relative offset)
    src_addr: usize,
    /// Index of the target instruction in the operations array
    target_idx: usize,
}

/// Parser for Brainf*ck source code
///
/// Converts raw source code into an optimized sequence of operations.
/// Performs instruction folding to combine consecutive identical operations
/// for better performance.
pub struct Parser;

impl Parser {
    /// Parses a Brainf*ck source file into a sequence of operations
    ///
    /// # Arguments
    /// * `path` - Path to the source file
    ///
    /// # Returns
    /// * `Ok(Vec<Op>)` - Successfully parsed operations
    /// * `Err(BfError)` - Parse error or I/O error
    pub fn parse_file<P: AsRef<Path>>(path: P) -> Result<Vec<Op>, BfError> {
        let content = fs::read(path.as_ref())?;
        Self::parse_bytes(&content, path.as_ref().to_string_lossy().into_owned())
    }

    /// Parses Brainf*ck source code from a byte array
    ///
    /// # Arguments
    /// * `content` - Raw source code as bytes
    /// * `source_name` - Name of the source (for error reporting)
    ///
    /// # Returns
    /// * `Ok(Vec<Op>)` - Successfully parsed and optimized operations
    /// * `Err(BfError)` - Parse error (unmatched brackets, invalid characters)
    pub fn parse_bytes(content: &[u8], source_name: String) -> Result<Vec<Op>, BfError> {
        let mut lexer = Lexer::new(content);
        let mut ops = Vec::with_capacity(content.len());
        let mut loop_stack = Vec::new();

        while let Some(c) = lexer.next() {
            if OpKind::is_foldable_byte(c) {
                // Parse and fold consecutive identical operations
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
                // Start of loop - push address onto stack for later patching
                let addr = ops.len();
                ops.push(Op {
                    kind: OpKind::JumpIfZero,
                    operand: 0, // Will be patched when matching ']' is found
                });
                loop_stack.push(addr);
            } else if c == b']' {
                // End of loop - patch the matching '[' instruction
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
            // All other characters are treated as comments and ignored
        }

        // Check for unmatched opening brackets
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

/// Convenience function to interpret a Brainf*ck file using the interpreter
///
/// # Arguments
/// * `path` - Path to the Brainf*ck source file
///
/// # Returns
/// * `Ok(())` - Successful execution
/// * `Err(BfError)` - Parse error, I/O error, or runtime error
pub fn interpret_file<P: AsRef<Path>>(path: P) -> Result<(), BfError> {
    let ops = Parser::parse_file(path)?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&ops)
}

/// Convenience function to JIT compile a Brainf*ck file
///
/// # Arguments
/// * `path` - Path to the Brainf*ck source file
///
/// # Returns
/// * `Ok(JitCode)` - Successfully compiled executable code
/// * `Err(BfError)` - Parse error, compilation error, or I/O error
pub fn jit_compile_file<P: AsRef<Path>>(path: P) -> Result<JitCode, BfError> {
    let ops = Parser::parse_file(path)?;
    JitCompiler::compile(&ops)
}
