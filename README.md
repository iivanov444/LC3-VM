# LC3-VM
Simple virtual machine for running assembly language programs for the LC3 (Little Computer 3) architecture written in C99.

The goal is to understand how Virtual Machines work (in this case registry based one). Some registry based virtual machines are Erlang's BEAM VM, Lua's VM (which is written in C89) and Dalvik VM used in Android operating system to execute applications written in Java (which also uses VM but stack based one).

## Build
Navigate to the main.c source file with terminal and compile it with any C99 supporting compiler (recommend GCC or Clang)
Examples:
gcc main.c -> outputs the binary with "a" as name
gcc main.c -o vm -> outputs the binary with "vm" as name
clang main.c -o vm

For generating debugging information:
gcc -g1 main.c -o vm // Minimal debugging information, sufficient for backtraces but lacking local variable details
gcc -g3 main.c -o vm // Maximum debugging information, including macro definitions and all local variables
gcc -ggdb main.c -o vm // Produces debugging information specifically for GDB, using the most expressive format available

## Resources
https://www.jmeiners.com/lc3-vm/ - Step by step guide with abundance of more resources inside, including binary files for LC3 (the games '2048' and 'Rogue')
https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf - Instruction Set Architecture (ISA) of LC3

## license
MIT License