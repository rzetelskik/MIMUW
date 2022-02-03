#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include <zconf.h>
#include "err.h"

#define BUFFSIZE 1024

char buff[BUFFSIZE];

int main(int argc, char *argv[]) {
    int res, sock;
    size_t filesize;
    struct addrinfo addr_hints, *addr_result;
    FILE *file;

    if (argc != 4)
        fatal("Usage: %s host port file", argv[0]);

    if ((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1)
        syserr("socket");

    memset(&addr_hints, 0, sizeof(struct addrinfo));
    addr_hints.ai_flags = 0;
    addr_hints.ai_family = AF_INET;
    addr_hints.ai_socktype = SOCK_STREAM;
    addr_hints.ai_protocol = IPPROTO_TCP;

    if ((res = getaddrinfo(argv[1], argv[2], &addr_hints, &addr_result))) {
        fprintf(stderr, "res=%d", res);
        syserr("getaddrinfo: %s", gai_strerror(res));
    }

    if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) != 0) {
        syserr("connect");
    }
    freeaddrinfo(addr_result);

    if (!(file = fopen(argv[3], "r")))
        syserr("open\n");

    fseek(file, 0L, SEEK_END);
    filesize = ftell(file);
    fseek(file, 0L, SEEK_SET);
    snprintf(buff, BUFFSIZE, "%ld:%s", filesize, argv[3]);

    if (write(sock, buff, strlen(buff)) == -1)
        syserr("write");

    memset(buff, 0, BUFFSIZE);
    while (fgets(buff, BUFFSIZE, file)) {
        if (write(sock, buff, strlen(buff)) < 0)
            syserr("write");
    }

    fclose(file);
    if (close(sock) == -1)
        syserr("close");

    return 0;
}