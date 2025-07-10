# bfjit - Brainfuck JIT Compiler in Rust

A high-performance Brainfuck interpreter and JIT compiler written in Rust, featuring both traditional interpretation and native code generation for maximum performance.

## Features

- **Dual Execution Modes**:
  - **Interpreter mode**: Safe, portable execution with comprehensive error handling
  - **JIT compiler mode**: Native x86-64 code generation for maximum performance
- **Advanced Optimizations**:
  - **Instruction folding**: Combines consecutive operations (e.g., `+++` becomes `add 3`)
  - **Direct syscalls**: Efficient I/O operations bypassing standard library overhead
  - **Smart memory management**: Dynamic memory allocation with bounds checking
- **Memory Safety**:
  - Expandable memory tape (starts with 30,000 cells, grows as needed)
  - Comprehensive bounds checking on memory access
  - Memory underflow protection
- **Robust Error Handling**:
  - Detailed parse error messages with file position
  - Runtime error detection and reporting
  - JIT compilation error handling

## Installation

```bash
git clone https://github.com/kraytos17/bfjit.git
cd bfjit
cargo build --release
```

## Usage

### Basic Usage

```bash
# Run with JIT compilation (default, fastest)
./target/release/bfjit program.bf

# Run with interpreter (safer, slower)
./target/release/bfjit --no-jit program.bf
```

### Command Line Options

- `--no-jit`: Disable JIT compilation and use interpreter mode
- `<input.bf>`: Path to the Brainfuck source file

### Example

```bash
# Create a simple "Hello World" program
echo '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.' > hello.bf

# Run with JIT (fast)
./target/release/bfjit hello.bf

# Run with interpreter (safe)
./target/release/bfjit --no-jit hello.bf
```

## Architecture

### Core Components

1. **Lexer** (`Lexer`): Tokenizes Brainfuck source code
2. **Parser** (`Parser`): Converts tokens to optimized operations with instruction folding
3. **Interpreter** (`Interpreter`): Virtual machine execution engine
4. **JIT Compiler** (`JitCompiler`): Native code generator for x86-64

### Supported Operations

| Brainfuck | Operation | Optimization |
|-----------|-----------|--------------|
| `+` | Increment cell | Folded (e.g., `+++` → `add 3`) |
| `-` | Decrement cell | Folded (e.g., `---` → `sub 3`) |
| `>` | Move right | Folded (e.g., `>>>` → `move_right 3`) |
| `<` | Move left | Folded (e.g., `<<<` → `move_left 3`) |
| `.` | Output | Folded (multiple outputs) |
| `,` | Input | Folded (multiple inputs) |
| `[` | Jump if zero | Jump optimization |
| `]` | Jump if non-zero | Jump optimization |

### Memory Model

- **Initial size**: 30,000 cells (standard Brainfuck)
- **Growth**: Automatic expansion when accessing cells beyond current size
- **Bounds checking**: Prevents memory underflow (moving left from position 0)
- **Cell size**: 8-bit unsigned integers with wrapping arithmetic

## Error Handling

The compiler provides detailed error messages for various scenarios:

```rust
// Parse errors with file position
BfError::Parse { file, pos, message }

// Runtime errors during execution
BfError::Runtime(message)

// JIT compilation errors
BfError::Jit(message)

// I/O errors
BfError::Io(io_error)
```

## API Usage

The library can be used programmatically:

```rust
use bfjit::{Parser, Interpreter, JitCompiler};

// Parse Brainfuck source
let ops = Parser::parse_bytes(b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", "hello.bf".to_string())?;

// Option 1: Use interpreter
let mut interpreter = Interpreter::new();
interpreter.interpret(&ops)?;

// Option 2: Use JIT compiler
let jit_code = JitCompiler::compile(&ops)?;
let mut memory = vec![0u8; 30000];
unsafe {
    jit_code.execute(memory.as_mut_ptr());
}

// Option 3: Use convenience functions
bfjit::interpret_file("program.bf")?;
let jit_code = bfjit::jit_compile_file("program.bf")?;
```

## Technical Details

### JIT Compilation

The JIT compiler generates x86-64 machine code using:
- **Direct syscalls**: `read(0, ptr, 1)` for input, `write(1, ptr, 1)` for output
- **Memory-mapped execution**: Uses `mmap` with `PROT_EXEC` for executable memory
- **Jump optimization**: Efficient conditional jumps with backpatching
- **Register allocation**: Uses `RDI` as the memory pointer register

### Safety Considerations

- **Memory safety**: All memory operations are bounds-checked
- **Overflow handling**: Uses wrapping arithmetic for cell values
- **Resource management**: Automatic cleanup of JIT-compiled code
- **Error propagation**: Comprehensive error handling throughout the pipeline

## License

This project is licensed under the MIT License - see the LICENSE file for details.
