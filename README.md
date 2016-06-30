# incrementalrust

A Rust implementation of the paper "An Incremental Approach to
Compiler Construction", a minimal Scheme to AMD64 compiler.

Currently depends on a minimal C shim.

Expects to have access to GCC (`gas` and `gcc` itself).

## Usage

REPL:

    cargo run

Compiler:

    cargo run file.scm

Tests:

    cargo test

## Features

This is by no means a complete or conformant Scheme compiler - in
fact, it's a scheme compiler only in the sense that there are lots of
parentheses. That said, there are

- Integers, strings, characters, pairs
- `let` form
- Closures (no `lambda`; instead the primitives `labels`, `code`,
  `closure` and `funcall` from the paper are exposed)
- `if` form
- Various operators

## Examples

    > (add1 1)

    Parsed: (add1 1)
        .text
        .p2align 4,,15
    .globl scheme_entry
        .type scheme_entry, @function
    scheme_entry:
        movl $4, %eax
        addl $4, %eax
        ret


    2
    > (* (char->integer #\a) 5)
    Parsed: (* (char->integer #\a) 5)
        .text
        .p2align 4,,15
    .globl scheme_entry
        .type scheme_entry, @function
    scheme_entry:
        movl $20, %eax
        movl %eax, -4(%rsp)
        movl $24847, %eax
        shr $6, %eax
        imull -4(%rsp), %eax
        sarl $2, %eax
        ret


    485
    > (null? ())
    Parsed: (null? ())
        .text
        .p2align 4,,15
    .globl scheme_entry
        .type scheme_entry, @function
    scheme_entry:
        movl $47, %eax
        andl $255, %eax
        cmpl $47, %eax
        sete %al
        sall $7, %eax
        orl $31, %eax
        ret


    #t
    > (zero? (+ 5 (- 5 10)))
    Parsed: (zero? (+ 5 (- 5 10)))
        .text
        .p2align 4,,15
    .globl scheme_entry
        .type scheme_entry, @function
    scheme_entry:
        movl $40, %eax
        movl %eax, -4(%rsp)
        movl $20, %eax
        subl -4(%rsp), %eax
        movl %eax, -8(%rsp)
        movl $20, %eax
        addl -8(%rsp), %eax
        cmpl $0, %eax
        sete %al
        sall $7, %eax
        orl $31, %eax
        ret


    #t
