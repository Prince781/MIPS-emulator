Author: Princeton Ferro
CSC 252
Project 3 - MIPS Emulator

Please see these source code files for more detail:
- Instruction.[ch]
- RegFile.[ch]

Basic ideas:
- Five stages (IF, ID, EX, MEM, WB)
- Instruction "microcode" is specified by two things:
  - flags (16-bit) that specify ALU source, read/write, mem/reg, etc
  - ALU operation
  - note: I used struct packing for the flags; I use an ifdef for
    endianness to ensure that fields are stored in a predictable sequence
    in memory. I'm 92% confident that this should work the same way on
    big- and little- endian machines. My test machine was little-endian.
(see Instruction.h and parts of Instruction.c for more detail)
- Branching/Jumping:
 - save information, execute next instruction, branch/jump depending on conditions
 - for bnel/etc we retire the next instruction by writing a nop

##Running
`./eMIPS [path to binary] [max instructions]`
