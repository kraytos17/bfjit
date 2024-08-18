use std::io::{self, Read, Write};

const JIT_MEMORY_CAP: usize = 10 * 1000 * 1000;

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

impl From<u8> for OpKind {
    fn from(value: u8) -> Self {
        match value {
            b'+' => OpKind::Inc,
            b'-' => OpKind::Dec,
            b'<' => OpKind::Left,
            b'>' => OpKind::Right,
            b'.' => OpKind::Output,
            b',' => OpKind::Input,
            b'[' => OpKind::JumpIfZero,
            b']' => OpKind::JumpIfNonZero,
            _ => panic!("Invalid OpKind value"),
        }
    }
}

impl Into<u8> for OpKind {
    fn into(self) -> u8 {
        self as u8
    }
}

#[derive(Debug, Clone)]
pub struct Op {
    pub kind: OpKind,
    pub operand: usize,
}

#[derive(Debug)]
pub struct Ops {
    pub items: Vec<Op>,
}

impl Ops {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, op: Op) {
        self.items.push(op);
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, index: usize) -> Option<&Op> {
        self.items.get(index)
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    content: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        Self { content, pos: 0 }
    }

    pub fn next(&mut self) -> Option<u8> {
        while self.pos < self.content.len() && !Self::is_bf_cmd(self.content[self.pos]) {
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

    fn is_bf_cmd(ch: u8) -> bool {
        matches!(ch, b'+' | b'-' | b'<' | b'>' | b'.' | b',' | b'[' | b']')
    }
}

pub fn interpret(ops: Ops) -> bool {
    let mut memory: Vec<u8> = vec![0; 1];
    let mut head: usize = 0;
    let mut ip: usize = 0;

    while ip < ops.len() {
        let op = ops.get(ip).unwrap();
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
                    eprintln!("RUNTIME ERROR: Memory underflow");
                    return false;
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
                    let mut buf = [0];
                    if io::stdin().read_exact(&mut buf).is_err() {
                        eprintln!("RUNTIME ERROR: Input error");
                        return false;
                    }
                    memory[head] = buf[0];
                }
                ip += 1;
            }
            OpKind::Output => {
                for _ in 0..op.operand {
                    if io::stdout().write_all(&[memory[head]]).is_err() {
                        eprintln!("RUNTIME ERROR: Output error");
                        return false;
                    }
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

    true
}

#[derive(Debug)]
pub struct Addrs {
    size: Vec<usize>,
}

impl Addrs {
    pub fn new() -> Self {
        Addrs { size: Vec::new() }
    }
}
