extern crate sexp;
extern crate tempfile;

use std::collections::{HashMap, HashSet};
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
    UnknownPrimitive(String),
}

type Result<T> = std::result::Result<T, CompileError>;

struct Environment<'a> {
    bindings: HashMap<String, isize>,
    labels: HashSet<String>,
    containing_env: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    fn new() -> Environment<'a> {
        Environment {
            bindings: HashMap::new(),
            // TODO: map label to bookkeeping of args
            labels: HashSet::new(),
            containing_env: None,
        }
    }

    fn new_under(env: &'a Environment<'a>) -> Environment<'a> {
        Environment {
            bindings: HashMap::new(),
            labels: HashSet::new(),
            containing_env: Some(env),
        }
    }

    fn lookup(&self, key: &str) -> Option<isize> {
        match self.bindings.get(key) {
            Some(x) => Some(*x),
            None => self.containing_env.and_then(|env| env.lookup(key)),
        }
    }

    fn update(&mut self, key: &str, location: isize) {
        self.bindings.insert(key.to_owned(), location);
    }

    fn add_label(&mut self, label: &str) {
        self.labels.insert(label.to_owned());
    }

    fn check_label(&self, label: &str) -> bool {
        self.containing_env.map_or(self.labels.contains(label), |env| env.check_label(label))
    }
}

macro_rules! emit {
    ($self_: ident, $asm: expr) => ($self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"))));

    ($self_: ident, $asm: expr, $($arg:tt)*) => {
        $self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"), $($arg)*));
    }
}

macro_rules! emit_comment {
    ($self_: ident, $comment: expr) => ($self_.buffer.push_str(&format!(concat!("\t# ", $comment, "\n"))));

    ($self_: ident, $comment: expr, $($arg:tt)*) => {
        $self_.buffer.push_str(&format!(concat!("\t# ", $comment, "\n"), $($arg)*));
    }
}

macro_rules! emit_label {
    ($self_: ident, $label: expr) => {
        $self_.buffer.push_str(&format!("\t{}:\n", $label));
    }
}

macro_rules! primitives {
    ($self_: ident, $op: ident, $($pat: pat => $result: expr)*) => {{
        match $op {
            $(
                $pat => {
                    $result;
                    Ok(())
                }
            ),*
                _ => {
                    Err(CompileError::UnknownPrimitive($op.to_owned()))
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

#[derive(Copy,Debug,Clone)]
enum BindingForm {
    Let,
    Labels,
}

#[derive(Debug)]
enum Form<'a> {
    UnaryPrimitive(&'a str, &'a Sexp),
    BinaryPrimitive(&'a str, &'a Sexp, &'a Sexp),
    LetLike(BindingForm, Vec<(&'a Sexp, &'a Sexp)>, &'a Sexp),
    // Body of a label
    Code(Vec<&'a str>, &'a Sexp),
    If(&'a Sexp, &'a Sexp, &'a Sexp),
    Labelcall(&'a str, &'a [Sexp]),
    // Funcall(&'a Sexp, &'a [Sexp]),
    Closure(&'a str, &'a [Sexp]),
}

#[derive(Debug)]
enum FormError<'a> {
    InvalidForm(&'a Sexp),
    InvalidBinding(&'a Sexp),
    InvalidIf(&'a Sexp),
    InvalidLabelcall(&'a Sexp, String),
    InvalidCode(&'a Sexp, String),
    NumArgs(&'a Sexp),
}

fn parse_form(sexp: &Sexp) -> ::std::result::Result<Form, FormError> {
    if let Sexp::List(ref items) = *sexp {
        if !items.is_empty() {
            let (head, tail) = items.split_at(1);
            if let Sexp::Atom(Atom::N(ref construct)) = head[0] {
                if construct == "let" || construct == "labels" {
                    let (bindings, expr) = tail.split_at(tail.len() - 1);
                    let mut result = vec![];

                    for item in bindings {
                        if let Sexp::List(ref items) = *item {
                            if items.len() != 2 {
                                return Err(FormError::InvalidBinding(item))
                            }

                            let name = &items[0];
                            let value = &items[1];

                            result.push((name, value));
                        }
                        else {
                            return Err(FormError::InvalidBinding(item))
                        }
                    }

                    return Ok(Form::LetLike(match construct.as_ref() {
                        "let" => BindingForm::Let,
                        "labels" => BindingForm::Labels,
                        _ => unreachable!(),
                    }, result, &expr[0]));
                }
                else if construct == "code" {
                    if tail.len() != 2 {
                        return Err(FormError::InvalidCode(sexp, "Code form is incorrect".to_owned()));
                    }
                    let arguments = if let Sexp::List(ref arguments) = tail[0] {
                        arguments
                    } else {
                        return Err(FormError::InvalidCode(sexp, "Invalid arguments list".to_owned()));
                    };
                    let body = &tail[1];
                    let mut result = Vec::new();
                    for arg in arguments.iter() {
                        if let Sexp::Atom(Atom::N(ref arg)) = *arg {
                            result.push((*arg).as_ref());
                        }
                        else {
                            return Err(FormError::InvalidCode(sexp, format!("Code form argument {} is incorrect", arg)));
                        }
                    }

                    return Ok(Form::Code(result, body));
                }
                else if construct == "closure" {
                    if tail.len() < 1 {
                        return Err(FormError::InvalidCode(sexp, "Code form is incorrect".to_owned()));
                    }
                    let (label, closed_over) = tail.split_at(1);
                    let label = if let Sexp::Atom(Atom::N(ref label)) = label[0] {
                        label
                    } else {
                        return Err(FormError::InvalidCode(sexp, "Invalid arguments list".to_owned()));
                    };
                    return Ok(Form::Closure(label, closed_over));
                }
                else if construct == "if" {
                    if tail.len() != 3 {
                        return Err(FormError::InvalidIf(sexp));
                    }
                    let cond = &tail[0];
                    let true_branch = &tail[1];
                    let false_branch = &tail[2];

                    return Ok(Form::If(cond, true_branch, false_branch));
                }
                else if construct == "labelcall" {
                    if tail.is_empty() {
                        return Err(FormError::InvalidLabelcall(sexp, "No label specified".to_owned()));
                    }
                    let (label, args) = tail.split_at(1);
                    if let Sexp::Atom(Atom::N(ref label)) = label[0] {
                        return Ok(Form::Labelcall(label, args));
                    }
                    else {
                        return Err(FormError::InvalidLabelcall(sexp, "Invalid label".to_owned()));
                    }
                }
                else if tail.len() == 1 {
                    return Ok(Form::UnaryPrimitive(construct, &tail[0]));
                }
                else if tail.len() == 2 {
                    return Ok(Form::BinaryPrimitive(construct, &tail[0], &tail[1]));
                }
                else {
                    return Err(FormError::NumArgs(sexp));
                }
            }
        }
    }

    Err(FormError::InvalidForm(sexp))
}

enum Comparison {
    Eq,
    Lt,
    Gt,
    Leq,
    Geq,
}

struct StackFrame {
    location: usize,
}

impl StackFrame {
    fn new() -> StackFrame {
        StackFrame {
            // Leave room for return address
            location: 1,
        }
    }

    fn push(&mut self) {
        self.location += 1;
    }

    /// Get the offset from %esp
    fn peek(&self) -> isize {
        (-4) * (self.location as isize)
    }

    fn pop(&mut self) {
        self.location -= 1;
        if self.location < 1 {
            panic!("Stack underflow");
        }
    }
}

struct Amd64Backend {
    buffer: String,
    stack: Vec<StackFrame>,
    label_counter: usize,
}

impl Amd64Backend {
    fn new() -> Amd64Backend {
        let mut buffer = String::new();
        buffer.push_str("\t.text\n");
        buffer.push_str("\t.p2align 4,,15\n");
        buffer.push_str(".globl scheme_entry\n");
        buffer.push_str("\t.type scheme_entry, @function\n");

        let mut stack = Vec::new();
        stack.push(StackFrame::new());

        Amd64Backend {
            buffer: buffer,
            stack: stack,
            label_counter: 0,
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
            Atom::N(ref name) => panic!("Unbound name: {}", name),
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

    fn make_label(&mut self) -> String {
        let label = format!("label{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn push(&mut self) -> isize {
        let location = self.peek();
        emit!(self, "movl %eax, {}(%esp)", location);
        self.stack.last_mut().unwrap().push();
        location
    }

    fn peek(&self) -> isize {
        self.stack.last().unwrap().peek()
    }

    fn pop(&mut self) {
        self.stack.last_mut().unwrap().pop()
    }

    fn push_stack(&mut self) {
        self.stack.push(StackFrame::new());
    }

    fn pop_stack(&mut self) {
        self.stack.pop();
        if self.stack.is_empty() {
            panic!("Stack underflow");
        }
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

    fn compile_unary_primitive(&mut self, op: &str, arg: &Sexp, environment: &mut Environment) -> Result<()> {
        primitives!(self, op,
            "add1" => {
                self.compile(arg, environment);
                emit!(self, "addl ${}, %eax", Self::immediate_rep(&Atom::I(1)));
            }

            "char->integer" => {
                self.compile(arg, environment);
                // TODO: check the tag first
                emit!(self, "shr $6, %eax");
            }

            "integer->char" => {
                self.compile(arg, environment);
                emit!(self, "shl $6, %eax");
                emit!(self, "orl $15, %eax");
            }

            "null?" => {
                self.compile(arg, environment);
                emit!(self, "andl $255, %eax");
                emit!(self, "cmpl $47, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "zero?" => {
                self.compile(arg, environment);
                emit!(self, "cmpl $0, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "integer?" => {
                self.compile(arg, environment);
                emit!(self, "andl $3, %eax");
                emit!(self, "cmpl $0, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "boolean?" => {
                self.compile(arg, environment);
                emit!(self, "andl $127, %eax");
                emit!(self, "cmpl $31, %eax");
                self.emit_boolean(Comparison::Eq);
            }

            "not" => {
                self.compile(arg, environment);
                emit!(self, "xorl $128, %eax");
            }

            "car" => {
                self.compile(arg, environment);
                emit!(self, "movl -1(%eax), %eax");
            }

            "cdr" => {
                self.compile(arg, environment);
                emit!(self, "movl 3(%eax), %eax");
            }
        )
    }

    fn compile_binary_primitive(&mut self, op: &str, arg1: &Sexp, arg2: &Sexp, environment: &mut Environment) -> Result<()> {
        primitives!(self, op,
            "+" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "addl {}(%esp), %eax", location);
            }

            "-" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "subl {}(%esp), %eax", location);
            }

            "*" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "imull {}(%esp), %eax", location);
                emit!(self, "sarl $2, %eax");
            }

            "=" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "cmpl {}(%esp), %eax", location);
                self.emit_boolean(Comparison::Eq);
            }

            "<" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "cmpl {}(%esp), %eax", location);
                self.emit_boolean(Comparison::Lt);
            }

            ">" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "cmpl {}(%esp), %eax", location);
                self.emit_boolean(Comparison::Gt);
            }

            "<=" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "cmpl {}(%esp), %eax", location);
                self.emit_boolean(Comparison::Leq);
            }

            ">=" => {
                self.compile(arg2, environment);
                let location = self.push();
                self.compile(arg1, environment);
                emit!(self, "cmpl {}(%esp), %eax", location);
                self.emit_boolean(Comparison::Geq);
            }

            "cons" => {
                emit_comment!(self, "cons");
                self.compile(arg1, environment);
                let location1 = self.push();
                self.compile(arg2, environment);
                let location2 = self.push();
                emit!(self, "movl {}(%esp), %eax", location1);
                emit!(self, "movl %eax, 0(%esi)");
                emit!(self, "movl {}(%esp), %eax", location2);
                emit!(self, "movl %eax, 4(%esi)");
                emit!(self, "movl %esi, %eax");
                emit!(self, "orl $1, %eax");
                emit!(self, "addl $8, %esi");
            }
        )
    }

    fn compile(&mut self, sexp: &Sexp, environment: &mut Environment) -> &str {
        match *sexp {
            Sexp::Atom(ref atom) => {
                if let Atom::N(ref name) = *atom {
                    if let Some(location) = environment.lookup(name) {
                        emit_comment!(self, "name {}", name);
                        emit!(self, "movl {}(%esp), %eax", location);
                    }
                    else {
                        panic!("Unbound name: {}", name);
                    }
                }
                else if let Atom::S(ref string) = *atom {
                    emit_comment!(self, "string '{}'", string);

                    let bytes = string.to_owned().into_bytes();
                    emit!(self, "movl %esi, %eax");
                    emit!(self, "orl $0b011, %eax");
                    emit!(self, "movl ${}, 0(%esi)", bytes.len());
                    emit!(self, "addl $4, %esi");
                    for byte in bytes.iter() {
                        emit!(self, "movb ${}, 0(%esi)", byte);
                        emit!(self, "addl $1, %esi");
                    }
                    // Round up allocation to 8 bytes
                    let remainder = (4 + bytes.len()) % 8;
                    if remainder > 0 {
                        emit!(self, "addl ${}, %esi", 8 - remainder);
                    }
                }
                else {
                    emit_comment!(self, "literal {}", atom);
                    emit!(self, "movl ${}, %eax", Self::immediate_rep(atom));
                }
            },
            Sexp::List(ref items) => if items.is_empty() {
                emit_comment!(self, "literal nil");
                emit!(self, "movl ${}, %eax", 0b00101111);
            }
            else {
                match parse_form(sexp) {
                    Ok(Form::UnaryPrimitive(name, arg1)) => try_or_panic!(self.compile_unary_primitive(name, arg1, environment)),
                    Ok(Form::BinaryPrimitive(name, arg1, arg2)) => try_or_panic!(self.compile_binary_primitive(name, arg1, arg2, environment)),
                    Ok(Form::LetLike(name, bindings, expr)) => {
                        match name {
                            BindingForm::Let => {
                                let environment = &mut Environment::new_under(environment);
                                for (name, value) in bindings {
                                    if let Sexp::Atom(Atom::N(ref name)) = *name {
                                        emit_comment!(self, "let {}", name);
                                        self.compile(value, environment);
                                        let location = self.push();
                                        environment.update(name, location);
                                    }
                                    else {
                                        panic!("Invalid name in let expression: {}", name);
                                    }
                                }
                                self.compile(expr, environment);
                            }
                            _ => panic!("Invalid binding form: {:?}", name),
                        }
                    }
                    Ok(Form::If(cond, true_branch, false_branch)) => {
                        emit_comment!(self, "if");
                        let (label0, label1) = (self.make_label(), self.make_label());

                        self.compile(cond, environment);
                        emit!(self, "cmpl ${}, %eax", Self::immediate_rep(&Atom::B(false)));
                        emit!(self, "je {}", label0);
                        // Need unique label generation, plus jumps
                        self.compile(true_branch, environment);
                        emit!(self, "jmp {}", label1);
                        emit_label!(self, label0);
                        self.compile(false_branch, environment);
                        emit_label!(self, label1);
                    }
                    Ok(Form::Labelcall(label, args)) => {
                        if environment.check_label(label) {
                            emit_comment!(self, "call {}", label);

                            let frame_size = self.peek().abs();
                            emit_comment!(self, "Save slot for return code");
                            self.push();
                            for arg in args.iter() {
                                self.compile(arg, environment);
                                self.push();
                            }

                            self.push_stack();
                            emit!(self, "subl ${}, %esp", frame_size - 4);
                            emit!(self, "call {}", label);
                            self.pop_stack();
                            emit!(self, "addl ${}, %esp", frame_size - 4);
                        }
                        else {
                            panic!("Label {} does not exist", label);
                        }
                    }
                    Ok(Form::Code(_, _)) => {
                        panic!("Code form not allowed outside of labels form");
                    }
                    Ok(Form::Closure(label, closed_over)) => {
                        emit!(self, "lea {}, %eax", label);
                        emit!(self, "movl %eax, 0(%esi)");
                        emit!(self, "movl %esi, %eax");
                        emit!(self, "orl $6, %eax");
                    }
                    Err(err) => panic!("Error parsing form: {:?}", err),
                }
            }
        }

        &self.buffer
    }

    fn compile_main(&mut self, sexp: &Sexp, environment: &mut Environment) {
        self.buffer.push_str("scheme_entry:\n");
        // Align rdi (base of heap) to 8-byte boundary
        emit!(self, "movl %edi, %esi");
        emit!(self, "addl $7, %esi");
        emit!(self, "andl $0xfffffff8, %esi");

        self.compile(sexp, environment);
        emit!(self, "ret");
    }

    fn compile_program(&mut self, sexp: &Sexp) -> &str {
        let mut environment = Environment::new();

        if let Ok(Form::LetLike(BindingForm::Labels, bindings, expr)) = parse_form(sexp) {
            // TODO: only allow code form as label value
            for (name, value) in bindings {
                if let Sexp::Atom(Atom::N(ref name)) = *name {
                    let form = parse_form(value);
                    if let Ok(Form::Code(arguments, body)) = form {
                        self.buffer.push_str(name);
                        self.buffer.push_str(":\n");
                        {
                            let mut label_env = Environment::new_under(&environment);
                            let mut offset = -4;
                            for arg in arguments {
                                emit_comment!(self, "Argument to {}: {}", name, arg);
                                label_env.update(arg, offset);
                                offset -= 4;
                                self.stack.last_mut().unwrap().location += 1;
                            }

                            self.compile(body, &mut label_env);
                            emit!(self, "ret");
                        }
                        self.stack.last_mut().unwrap().location = 1;
                        environment.add_label(name);
                    }
                    else {
                        panic!("Invalid body of label (must be code form): {}. Reason: {:?}", value, form);
                    }
                }
                else {
                    panic!("Invalid name in labels expression: {}", name);
                }
            }

            self.compile_main(expr, &mut environment);
        }
        else {
            self.compile_main(sexp, &mut environment);
        }

        &self.buffer
    }
}

pub fn compile_program(sexp: &Sexp) -> String {
    let mut backend = Amd64Backend::new();
    backend.compile_program(sexp).to_owned()
}


fn assemble(input: &str, bin_path: &Path) {
    let source = compile_program(&parse(input).unwrap());
    let mut f = tempfile::NamedTempFile::new().expect("Could not create temp file");
    f.write_all(&source.into_bytes()).expect("Could not write generated asm to test file");
    f.sync_all().expect("Could not sync test file");

    let asm_path = f.path();

    let obj_file = tempfile::NamedTempFileOptions::new()
        .prefix("incrementalrust")
        .suffix(".o")
        .create().expect("Could not create temp object file");
    let obj_path = obj_file.path();
    let cmd = Command::new("as")
        .arg("--32")
        .arg("-o")
        .arg(obj_path)
        .arg(asm_path)
        .status().expect("Could not assemble");
    assert!(cmd.success());

    let cmd = Command::new("gcc")
        .arg("-m32")
        .arg("-o")
        .arg(bin_path)
        .arg(obj_path)
        .arg("driver.c")
        .status().expect("Could not compile");
    assert!(cmd.success());
}

pub fn compile_and_execute(input: &str) -> String {
    let bin_file = tempfile::NamedTempFileOptions::new()
        .prefix("incrementalrust")
        .suffix(".bin")
        .create().expect("Could not create temp binary file");
    let bin_path = bin_file.path();
    assemble(input, bin_path);

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

fn main() {
    use std::fs::File;
    use std::io::{self, BufRead, Read};
    use std::path::Path;

    if std::env::args().count() > 1 {
        let arg = std::env::args().nth(1).unwrap();
        let path = Path::new(&arg);
        let mut file = match File::open(&path) {
            Err(e) => panic!("Couldn't open {}: {:?}", path.display(), e),
            Ok(f) => f,
        };

        let mut s = String::new();
        match file.read_to_string(&mut s) {
            Err(e) => panic!("Couldn't open {}: {:?}", path.display(), e),
            Ok(_) => (),
        };

        let output_path = path.with_extension("exec");
        assemble(&s, &output_path);

        return;
    }

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

    #[test]
    fn let_bindings() {
        assert_eq!(compile_and_execute("(let (a 2) (+ a 1))"), "3");
        assert_eq!(compile_and_execute("(let (a 2) (+ a a))"), "4");
        assert_eq!(compile_and_execute("(let (a 2) (b 3) (+ a b))"), "5");
        assert_eq!(compile_and_execute("(let (a 2) (b (+ a 2)) (+ a b))"), "6");
        assert_eq!(compile_and_execute("(let (a 2) (b (+ a 2)) (let (a 3) (* a b)))"), "12");
        assert_eq!(compile_and_execute("(let (a 2) (b (let (a 3) (+ a 2))) (+ a b))"), "7");
    }

    #[test]
    fn if_expression() {
        assert_eq!(compile_and_execute("(if #t 2 3)"), "2");
        assert_eq!(compile_and_execute("(if #f 2 3)"), "3");
        assert_eq!(compile_and_execute("(if (= 2 3) 2 3)"), "3");
        assert_eq!(compile_and_execute("(if (= 2 (+ 1 1)) 2 3)"), "2");
        assert_eq!(compile_and_execute("(if (= 2 (+ 1 1)) #f 3)"), "#f");
        assert_eq!(compile_and_execute("(let (a 2) (b 3) (if (> a b) 3 5))"), "5");
    }

    #[test]
    fn cons_car_cdr() {
        assert_eq!(compile_and_execute("(cons 10 20)"), "(10 20)");
        assert_eq!(compile_and_execute("(cons 10 ())"), "(10 ())");
        assert_eq!(compile_and_execute("(let (a (cons 10 20)) (car a))"), "10");
        assert_eq!(compile_and_execute("(let (a (cons 10 20)) (cdr a))"), "20");
        assert_eq!(compile_and_execute("(let (a (cons 10 (cons 20 ()))) (car (cdr a)))"), "20");
        assert_eq!(compile_and_execute("(let (a (cons 10 (cons 20 ()))) (cdr (cdr a)))"), "()");
    }

    #[test]
    fn string_representation() {
        assert_eq!(compile_and_execute("\"Hello, world!\""), "\"Hello, world!\"");
        assert_eq!(compile_and_execute("(cons 20 \"Hello, world!\")"), "(20 \"Hello, world!\")");
    }

    #[test]
    fn labelcalls() {
        assert_eq!(compile_and_execute("(labels (a (code (x) x)) (labelcall a 2))"), "2");
        assert_eq!(compile_and_execute("(labels (a (code (x) (+ x 1))) (b (code (x y) (+ (labelcall a x) y))) (labelcall b 21 20))"), "42");
        assert_eq!(compile_and_execute("(labels (a (code (x) \"Hello, world!\")) (labelcall a 2))"), "\"Hello, world!\"");
    }
}
