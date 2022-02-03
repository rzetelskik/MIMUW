TARGET: nk-client-udp nk-server-udp

CC	= gcc
CFLAGS	= -Wall -O2
LFLAGS	= -Wall

nk-client-udp.o nk-server-udp.o err.o: err.h

nk-client-udp: nk-client-udp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

nk-server-udp: nk-server-udp.o err.o
	$(CC) $(LFLAGS) $^ -o $@

.PHONY: clean TARGET
clean:
	rm -f output nk-client-udp nk-server-udp *.o *~ *.bak
