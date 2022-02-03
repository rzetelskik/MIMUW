TARGET: nk-client-tcp nk-server-tcp

CC	= cc
CFLAGS	= -Wall -O2
LFLAGS	= -Wall

err.o nk-client-tcp.o nk-server-tcp.o: err.h

nk-client-tcp: nk-client-tcp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

nk-server-tcp: nk-server-tcp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f nk-client-tcp nk-server-tcp *.o *~ *.bak
