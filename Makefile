
COMPILER = gcc 

COMPFLAGS = -Wall -g -std=gnu99

SIMPATH = src/

FILELIST = $(SIMPATH)elf_reader/elf_reader.c $(SIMPATH)utils/heap.c $(SIMPATH)RegFile.c $(SIMPATH)Syscall.c $(SIMPATH)PROC.c $(SIMPATH)Instruction.c

MEMU: 
	$(COMPILER) $(COMPFLAGS) $(FILELIST) -o eMIPS

CLEAN:
	rm -rf RUN

