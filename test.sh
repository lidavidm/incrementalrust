#!/bin/sh

cargo run --quiet > scheme.s
as -o scheme.o scheme.s
gcc -o temp scheme.o driver.c
./temp
rm temp
