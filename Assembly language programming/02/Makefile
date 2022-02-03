CC = gcc
all: flow

flow: flow.o flowmain.c err.o
	$(CC) -o flow flowmain.c flow.o err.o

.SECONDARY:

err.o: err.c err.h

flow.o: flow.asm
	nasm -f elf64 -F dwarf -g $<

%: %.o 
	ld $< -o $@ -lc --dynamic-linker=/lib64/ld-linux-x86-64.so.2

.PHONY: clean test

test: flow
	./test.sh flow tests

clean:
	rm -f *.o flow
