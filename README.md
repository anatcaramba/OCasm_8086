For now the only feature available is disassembling a 8086 binary file, outputing the corresponding assembly instructions to standard output.

## Building
Requires `dune` (and therefore `opam`).
Project can be tested by executing `dune exec the_proc <filename>` on the command line. `<filename>` is a binary file consisting of 8086 assembly.

## Notes
- Particular ESC instructions (FPU and others) are not implemented.

## Inspired by
Heavily inspired to undertake this by Casey Muratori and his course.

