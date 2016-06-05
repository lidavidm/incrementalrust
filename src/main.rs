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

    fn immediate_rep(sexp: &Sexp) -> u32 {
        match *sexp {
            Sexp::Atom(ref atom) => match *atom {
                Atom::I(integer) => (integer << 2) as u32,
                Atom::F(_) => panic!("Unimplemented: representation of float"),
                Atom::S(_) => panic!("Unimplemented: representation of string"),
                Atom::B(true) => 0b10011111,
                Atom::B(false) => 0b00011111,
            },
            Sexp::List(ref items) => if items.is_empty() {
                0b00101111
            }
            else {
                panic!("Unimplemented: representation of non-empty list");
            }
        }
    }

    fn compile(&mut self, sexp: &Sexp) -> &str {
        emit!(self, "movl ${}, %eax", Self::immediate_rep(sexp));
        emit!(self, "ret");

        &self.buffer
    }
}

pub fn compile_program(sexp: &Sexp) -> String {
    let mut backend = Amd64Backend::new();
    backend.compile(sexp).to_owned()
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

    use std::fs::{self, File};
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
        fs::copy(bin_path, &actual_bin_path);

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
        assert_eq!(compile_and_execute("42"), "42\n");
    }

    #[test]
    fn empty_list() {
        assert_eq!(compile_and_execute("()"), "()\n");
    }

    #[test]
    fn boolean() {
        assert_eq!(compile_and_execute("#t"), "#t\n");
        assert_eq!(compile_and_execute("#f"), "#f\n");
    }
}
