all: zad3
 
zad3: zad3_main.c zad3.o err.o libppm.o
	gcc -std=c99 -o $@ $^

.SECONDARY:

zad3.o: zad3.s
	as -o $@ $<

err.o: err.c err.h
	gcc -std=c99 -c -o $@ $<

libppm.o: libppm.c libppm.h
	gcc -std=c99 -c -o $@ $<

%: %.o
	ld $< -o $@ -lc --dynamic-linker=/lib64/ld-linux-x86-64.so.2

.PHONY: clean test

test: zad3
	./test.sh zad3 tests

clean:
	rm -vf zad3 *.o

