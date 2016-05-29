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

    fn compile(&mut self, sexp: &Sexp) -> &str {
        if let Sexp::Atom(Atom::I(num)) = *sexp {
            emit!(self, "movl ${}, %eax", num);
            emit!(self, "ret");
        }

        &self.buffer
    }
}

fn compile_program(sexp: &Sexp) -> String {
    let mut backend = Amd64Backend::new();
    backend.compile(sexp).to_owned()
}

fn main() {
    print!("{}", compile_program(&parse("42").unwrap()));
}

#[test]
fn it_works() {
    let source = compile_program(&parse("42").unwrap());
    print!("{}", source);
}
