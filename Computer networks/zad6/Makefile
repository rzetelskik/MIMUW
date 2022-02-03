CC = gcc
CFLAGS = -Wall
TARGETS = file-client-tcp file-server-tcp

all: $(TARGETS)

err.o: err.c err.h

file-client-tcp.o: file-client-tcp.c err.h

file-client-tcp: file-client-tcp.o err.o

file-server-tcp.o: file-server-tcp.c err.h

file-server-tcp: file-server-tcp.o err.o
	$(CC) $(CFLAGS) $^ -o $@ -lpthread

clean:
	rm -f *.o *~ $(TARGETS)
