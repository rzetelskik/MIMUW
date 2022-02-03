TARGET: testhttp_raw

CC	= cc
CFLAGS	= -Wall -O2
LFLAGS	= -Wall

err.o testhttp_raw.o: err.h

testhttp_raw: testhttp_raw.o err.o
	$(CC) $(LFLAGS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f testhttp_raw *.o *~ *.bak
