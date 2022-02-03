#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "err.h"

int main(int argc, char *argv[])
{
    int sock, n, k;
    struct addrinfo addr_hints;
    struct addrinfo *addr_result;

    int err;
    ssize_t rcv_len;

    if (argc < 5) {
        fatal("Usage: %s host | port | number of packets | size of packet ...\n", argv[0]);
    }

    // 'converting' host/port in string to struct addrinfo
    memset(&addr_hints, 0, sizeof(struct addrinfo));
    addr_hints.ai_family = AF_INET; // IPv4
    addr_hints.ai_socktype = SOCK_STREAM;
    addr_hints.ai_protocol = IPPROTO_TCP;
    err = getaddrinfo(argv[1], argv[2], &addr_hints, &addr_result);
    if (err == EAI_SYSTEM) { // system error
        syserr("getaddrinfo: %s", gai_strerror(err));
    }
    else if (err != 0) { // other error (host not found, etc.)
        fatal("getaddrinfo: %s", gai_strerror(err));
    }

    // initialize socket according to getaddrinfo results
    sock = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
    if (sock < 0)
        syserr("socket");

    // connect socket to the server
    if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) < 0)
        syserr("connect");

    freeaddrinfo(addr_result);

    n = atoi(argv[3]), k = atoi(argv[4]);
    char buffer[k + 1];
    (void) memset(buffer, 0, sizeof(buffer));

    for (int i = 0; i < n; i++) {
        (void) memset(buffer, 'a' + (rand() % 26), k);


        printf("writing to socket: %s\n", buffer);
        if (write(sock, buffer, k) != k) {
            syserr("partial / failed write");
        }

        memset(buffer, 0, sizeof(buffer));
        rcv_len = read(sock, buffer, k);
        if (rcv_len < 0) {
            syserr("read");
        }
        printf("read from socket: %zd bytes: %s\n", rcv_len, buffer);
    }

    (void) close(sock); // socket would be closed anyway when the program ends

    return 0;
}
