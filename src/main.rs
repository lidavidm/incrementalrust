extern crate sexp;

use sexp::{Atom, Sexp, parse};

struct Amd64Backend {
    buffer: String,
}

macro_rules! emit {
    ($self_: ident, $asm: expr) => ($self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"))));

    ($self_: ident, $asm: expr, $($arg:tt)*) => {
        $self_.buffer.push_str(&format!(concat!("\t", $asm, "\n"), $($arg)*));
    }
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
        }
    }

    fn immediate_rep(atom: &Atom) -> u32 {
        use std::ascii::AsciiExt;
        match *atom {
            Atom::I(integer) => (integer << 2) as u32,
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

    fn compile(&mut self, sexp: &Sexp) -> &str {
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
                    if arg == "add1" {
                        if tail.len() == 1 {
                            self.compile(&tail[0]);
                            emit!(self, "addl ${}, %eax", Self::immediate_rep(&Atom::I(1)));
                        }
                        else {
                            panic!("add1 has multiple args");
                        }
                    }
                }
            }
        }

        &self.buffer
    }

    fn compile_program(&mut self, sexp: &Sexp) -> &str {
        self.compile(sexp);

        emit!(self, "ret");
        &self.buffer
    }
}

pub fn compile_program(sexp: &Sexp) -> String {
    let mut backend = Amd64Backend::new();
    backend.compile_program(sexp).to_owned()
}

fn main() {
    use std::io::{self, BufRead};
    let stdin = io::stdin();
    let handle = stdin.lock();
    for line in handle.lines() {
        let line = line.unwrap();
        if let Ok(result) = parse(&line) {
            println!("Parsed: {:?}", result);
            println!("{}", compile_program(&result));
        }
        else {
            println!("{:?}", line);
        }
    }
}

#[cfg(test)]
mod test {
    extern crate tempfile;
    use sexp::parse;
    use super::*;

    use std::fs;
    use std::io::Write;
    use std::path::Path;
    use std::process::Command;

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

        String::from_utf8_lossy(&Command::new(actual_bin_path)
            .output().expect("Could not execute").stdout).into_owned()
    }

    fn compile_and_execute(input: &str) -> String {
        let source = compile_program(&parse(input).unwrap());
        let mut f = tempfile::NamedTempFile::new().expect("Could not create temp file");
        f.write_all(&source.into_bytes()).expect("Could not write generated asm to test file");
        f.sync_all().expect("Could not sync test file");
        assemble(f.path())
    }

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
    }
}
