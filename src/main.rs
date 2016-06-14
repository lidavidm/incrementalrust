extern crate sexp;
extern crate tempfile;

use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use sexp::{Atom, Sexp, parse};

pub const MIN_INT: i64 = -536870912; // -i32::pow(2, 29)
pub const MAX_INT: i64 = 536870911; // i32::pow(2, 29) - 1

#[derive(Debug)]
enum CompileError {
    // Procedure, expected, actual
    NumArgs(String, usize, usize),
}

type Result<T> = std::result::Result<T, CompileError>;

struct Environment<'a> {
    bindings: HashMap<String, usize>,
    containing_env: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    fn new() -> Environment<'a> {
        Environment {
            bindings: HashMap::new(),
            containing_env: None,
        }
    }

    fn new_under(env: &'a Environment<'a>) -> Environment<'a> {
        Environment {
            bindings: HashMap::new(),
            containing_env: Some(env),
        }
    }

    fn lookup(&self, key: &str) -> Option<usize> {
        match self.bindings.get(key) {
            Some(x) => Some(*x),
            None => self.containing_env.and_then(|env| env.lookup(key)),
        }
    }

    fn update(&mut self, key: &str, location: usize) {
        self.bindings.insert(key.to_owned(), location);
    }
}

macro_rules! emit {
    ($self_: ident, $asm: expr) => ($self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"))));

    ($self_: ident, $asm: expr, $($arg:tt)*) => {
        $self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"), $($arg)*));
    }
}

macro_rules! primitives {
    ($self_: ident, $expected: expr, $op: ident, $tail: ident, $($pat: pat => $result: expr)*) => {{
        match $op {
            $(
                $pat => {
                    if $tail.len() != $expected {
                        return Err(CompileError::NumArgs($op.to_owned(), $expected, $tail.len()));
                    }

                    $result
                }
            ),*
                _ => {
                    return Ok(false);
                }
        }
    }}
}

macro_rules! try_or_panic {
    ($expr: expr) => {
        match $expr {
            Ok(val) => val,
            Err(e) => panic!("{:?}", e),
        }
    }
}

enum Comparison {
    Eq,
    Lt,
    Gt,
    Leq,
    Geq,
}

struct Amd64Backend {
    buffer: String,
    stack_location: usize,
}

impl Amd64Backend {
    fn new() -> Amd64Backend {
        let mut buffer = String::new();
        buffer.push_str("\t.text\n");
        buffer.push_str("\t.p2align 4,,15\n");
        buffer.push_str(".globl scheme_entry\n");
        buffer.push_str("\t.type scheme_entry, @function\n");
        buffer.push_str("scheme_entry:\n");
        Amd64Backend {
            buffer: buffer,
            stack_location: 1,
        }
    }

    fn immediate_rep(atom: &Atom) -> u32 {
        use std::ascii::AsciiExt;
        match *atom {
            Atom::I(integer) => {
                if integer < MIN_INT || integer > MAX_INT {
                    panic!("Integer out of bounds: {}", integer);
                }
                (integer << 2) as u32
            },
            Atom::F(_) => panic!("Unimplemented: representation of float"),
            Atom::S(_) => panic!("Unimplemented: representation of string"),
            Atom::B(true) => 0b10011111,
            Atom::B(false) => 0b00011111,
            Atom::C(chr) => if chr.is_ascii() {
                ((chr as u32) << 8) | 0b00001111
            }
            else {
                panic!("Unimplemented: non-ASCII characters");
            }
        }
    }

    fn push(&mut self) -> isize {
        let location = self.peek();
        emit!(self, "movl %eax, {}(%rsp)", location);
        self.stack_location += 1;
        location
    }

    fn peek(&self) -> isize {
        (-4) * (self.stack_location as isize)
    }

    fn pop(&mut self) {

    }

    // Emits the representation for true or false, based on the result
    // of a prior comparison.
    fn emit_boolean(&mut self, comparison: Comparison) {
        use Comparison::*;
        emit!(self, "movl $0, %eax");
        emit!(self, "set{} %al", match comparison {
            Eq => "e",
            Lt => "l",
            Gt => "g",
            Leq => "le",
            Geq => "ge",
        });
        emit!(self, "sall $7, %eax");
        emit!(self, "orl $31, %eax");
    }

    fn compile_unary_primitive(&mut self, op: &str, tail: &[Sexp], environment: &mut Environment) -> Result<bool> {
        primitives!(self, 1, op, tail,
            "add1" => {
                self.compile(&tail[0], environment);
                emit!(self, "addl ${}, %eax", Self::immediate_rep(&Atom::I(1)));
            }

            "char->integer" => {
                self.compile(&tail[0], environment);
                // TODO: check the tag first
                emit!(self, "shr $6, %eax");
            }

            "integer->char" => {
                self.compile(&tail[0], environment);
                emit!(self, "shl $6, %eax");
                emit!(self, "orl $15, %eax");
            }

            "null?" => {
                self.compile(&tail[0], environment);
                emit!(self, "andl $255, %eax");
                emit!(self, "cmpl $47, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "zero?" => {
                self.compile(&tail[0], environment);
                emit!(self, "cmpl $0, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "integer?" => {
                self.compile(&tail[0], environment);
                emit!(self, "andl $3, %eax");
                emit!(self, "cmpl $0, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "boolean?" => {
                self.compile(&tail[0], environment);
                emit!(self, "andl $127, %eax");
                emit!(self, "cmpl $31, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "not" => {
                self.compile(&tail[0], environment);
                emit!(self, "xorl $128, %eax");
            }
        );

        Ok(true)
    }

    fn compile_binary_primitive(&mut self, op: &str, tail: &[Sexp], environment: &mut Environment) -> Result<bool> {
        primitives!(self, 2, op, tail,
            "+" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "addl {}(%rsp), %eax", location);
            }

            "-" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "subl {}(%rsp), %eax", location);
            }

            "*" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "imull {}(%rsp), %eax", location);
                emit!(self, "sarl $2, %eax");
            }

            "=" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "cmpl {}(%rsp), %eax", location);
                self.emit_boolean(Comparison::Eq);
            }

            "<" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "cmpl {}(%rsp), %eax", location);
                self.emit_boolean(Comparison::Lt);
            }

            ">" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "cmpl {}(%rsp), %eax", location);
                self.emit_boolean(Comparison::Gt);
            }

            "<=" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "cmpl {}(%rsp), %eax", location);
                self.emit_boolean(Comparison::Leq);
            }

            ">=" => {
                self.compile(&tail[1], environment);
                let location = self.push();
                self.compile(&tail[0], environment);
                emit!(self, "cmpl {}(%rsp), %eax", location);
                self.emit_boolean(Comparison::Geq);
            }
        );

        Ok(true)
    }

    fn compile(&mut self, sexp: &Sexp, environment: &mut Environment) -> &str {
        match *sexp {
            Sexp::Atom(ref atom) => {
                emit!(self, "movl ${}, %eax", Self::immediate_rep(atom));
            },
            Sexp::List(ref items) => if items.is_empty() {
                emit!(self, "movl ${}, %eax", 0b00101111);
            }
            else {
                let (head, tail) = items.split_at(1);
                if let Sexp::Atom(Atom::S(ref arg)) = head[0] {
                    // TODO: make this less messy
                    if try_or_panic!(self.compile_unary_primitive(arg, tail, environment)) {

                    }
                    else if try_or_panic!(self.compile_binary_primitive(arg, tail, environment)) {

                    }
                    else {
                        panic!("Unrecognized primitive: {}", arg);
                    }
                }
            }
        }

        &self.buffer
    }

    fn compile_program(&mut self, sexp: &Sexp) -> &str {
        let mut environment = Environment::new();
        self.compile(sexp, &mut environment);

        emit!(self, "ret");
        &self.buffer
    }
}

pub fn compile_program(sexp: &Sexp) -> String {
    let mut backend = Amd64Backend::new();
    backend.compile_program(sexp).to_owned()
}


fn assemble(asm_path: &Path) -> String {
    let obj_file = tempfile::NamedTempFileOptions::new()
        .prefix("incrementalrust")
        .suffix(".o")
        .create().expect("Could not create temp object file");
    let obj_path = obj_file.path();
    let cmd = Command::new("as")
        .arg("-o")
        .arg(obj_path)
        .arg(asm_path)
        .status().expect("Could not assemble");
    assert!(cmd.success());

    let bin_file = tempfile::NamedTempFileOptions::new()
        .prefix("incrementalrust")
        .suffix(".bin")
        .create().expect("Could not create temp binary file");
    let bin_path = bin_file.path();
    let cmd = Command::new("gcc")
        .arg("-o")
        .arg(bin_path)
        .arg(obj_path)
        .arg("driver.c")
        .status().expect("Could not compile");
    assert!(cmd.success());

    let actual_bin_path = bin_path.with_extension("exec");
    fs::copy(bin_path, &actual_bin_path).expect("Could not copy file!");

    let result = String::from_utf8_lossy(
        &Command::new(&actual_bin_path)
            .output()
            .expect("Could not execute")
            .stdout)
        .into_owned();

    fs::remove_file(&actual_bin_path).expect("Could not remove temp file!");

    result
}

pub fn compile_and_execute(input: &str) -> String {
    let source = compile_program(&parse(input).unwrap());
    let mut f = tempfile::NamedTempFile::new().expect("Could not create temp file");
    f.write_all(&source.into_bytes()).expect("Could not write generated asm to test file");
    f.sync_all().expect("Could not sync test file");
    assemble(f.path())
}

fn main() {
    use std::io::{self, BufRead};
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let handle = stdin.lock();
    print!("> ");
    stdout.flush().expect("Could not flush stdout");

    for line in handle.lines() {
        let line = line.unwrap();
        if let Ok(result) = parse(&line) {
            println!("Parsed: {:?}", result);
            println!("{}", compile_program(&result));
            println!("");
            println!("{}", compile_and_execute(&line));
        }
        else {
            println!("{:?}", line);
        }

        print!("> ");
        stdout.flush().expect("Could not flush stdout");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fixnum() {
        assert_eq!(compile_and_execute("42"), "42");
    }

    #[test]
    fn empty_list() {
        assert_eq!(compile_and_execute("()"), "()");
    }

    #[test]
    fn boolean() {
        assert_eq!(compile_and_execute("#t"), "#t");
        assert_eq!(compile_and_execute("#f"), "#f");
    }

    #[test]
    fn simple_char() {
        assert_eq!(compile_and_execute("#\\a"), "#\\a");
    }

    #[test]
    fn add1() {
        assert_eq!(compile_and_execute("(add1 1)"), "2");
        assert_eq!(compile_and_execute("(add1 (add1 1))"), "3");
    }

    #[test]
    fn char_to_integer() {
        assert_eq!(compile_and_execute("(char->integer #\\a)"), "97");
        assert_eq!(compile_and_execute("(char->integer #\\newline)"), "10");
        assert_eq!(compile_and_execute("(char->integer #\\space)"), "32");
    }

    #[test]
    fn integer_to_char() {
        assert_eq!(compile_and_execute("(integer->char 97)"), "#\\a");
        assert_eq!(compile_and_execute("(integer->char 10)"), "#\\newline");
        assert_eq!(compile_and_execute("(integer->char 32)"), "#\\space");
    }

    #[test]
    fn null_p() {
        assert_eq!(compile_and_execute("(null? ())"), "#t");
        assert_eq!(compile_and_execute("(null? 0)"), "#f");
    }

    #[test]
    fn zero_p() {
        assert_eq!(compile_and_execute("(zero? ())"), "#f");
        assert_eq!(compile_and_execute("(zero? 0)"), "#t");
    }

    #[test]
    fn integer_p() {
        assert_eq!(compile_and_execute("(integer? ())"), "#f");
        assert_eq!(compile_and_execute("(integer? 0)"), "#t");
        assert_eq!(compile_and_execute("(integer? #t)"), "#f");
        assert_eq!(compile_and_execute("(integer? #\\a)"), "#f");
    }

    #[test]
    fn boolean_p() {
        assert_eq!(compile_and_execute("(boolean? ())"), "#f");
        assert_eq!(compile_and_execute("(boolean? 0)"), "#f");
        assert_eq!(compile_and_execute("(boolean? 1385901)"), "#f");
        assert_eq!(compile_and_execute("(boolean? #t)"), "#t");
        assert_eq!(compile_and_execute("(boolean? #\\a)"), "#f");
    }

    #[test]
    fn not() {
        assert_eq!(compile_and_execute("(not #t)"), "#f");
        assert_eq!(compile_and_execute("(not #f)"), "#t");
    }

    #[test]
    fn binary_add() {
        assert_eq!(compile_and_execute("(+ 0 0)"), "0");
        assert_eq!(compile_and_execute("(+ 0 2)"), "2");
        assert_eq!(compile_and_execute("(+ 2 0)"), "2");
        assert_eq!(compile_and_execute("(+ 1 3)"), "4");
        assert_eq!(compile_and_execute("(+ (char->integer #\\a) 3)"), "100");
    }

    #[test]
    fn binary_sub() {
        assert_eq!(compile_and_execute("(- 0 0)"), "0");
        assert_eq!(compile_and_execute("(- 0 2)"), "-2");
        assert_eq!(compile_and_execute("(- 2 0)"), "2");
        assert_eq!(compile_and_execute("(- 1 3)"), "-2");
        assert_eq!(compile_and_execute("(- (char->integer #\\a) 3)"), "94");
    }

    #[test]
    fn binary_mul() {
        assert_eq!(compile_and_execute("(* 0 0)"), "0");
        assert_eq!(compile_and_execute("(* 0 2)"), "0");
        assert_eq!(compile_and_execute("(* 2 0)"), "0");
        assert_eq!(compile_and_execute("(* 1 3)"), "3");
        assert_eq!(compile_and_execute("(* 3 5)"), "15");
        assert_eq!(compile_and_execute("(* (char->integer #\\a) 1)"), "97");
    }

    #[test]
    fn eq_p() {
        let maxint = format!("{}", MAX_INT);
        let items = vec!["()", "0", &maxint, "#t", "#f", "#\\a", "#\\newline"];
        for a in items.iter() {
            for b in items.iter() {
                assert_eq!(compile_and_execute(&format!("(= {} {})", a, b)), if a == b {
                    "#t"
                } else {
                    "#f"
                });
            }
        }
    }

    #[test]
    fn comparison_p() {
        let items = vec![0, -10, MIN_INT, MAX_INT, 1];
        for a in items.iter() {
            for b in items.iter() {
                assert_eq!(compile_and_execute(&format!("(> {} {})", a, b)), if a > b {
                    "#t"
                } else {
                    "#f"
                });
                assert_eq!(compile_and_execute(&format!("(< {} {})", a, b)), if a < b {
                    "#t"
                } else {
                    "#f"
                });
                assert_eq!(compile_and_execute(&format!("(>= {} {})", a, b)), if a >= b {
                    "#t"
                } else {
                    "#f"
                });
                assert_eq!(compile_and_execute(&format!("(<= {} {})", a, b)), if a <= b {
                    "#t"
                } else {
                    "#f"
                });
            }
        }
    }
}
