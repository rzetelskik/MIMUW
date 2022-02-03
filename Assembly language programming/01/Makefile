CC = gcc
all: life

life: life.o lifemain.c err.o
	$(CC) -o life lifemain.c life.o err.o

.SECONDARY:

err.o: err.c err.h

life.o: life.asm
	nasm -f elf64 -F dwarf -g $<

%: %.o 
	ld $< -o $@ -lc --dynamic-linker=/lib64/ld-linux-x86-64.so.2

.PHONY: clean test

test: life
	./test.sh life tests

clean:
	rm -f *.o life