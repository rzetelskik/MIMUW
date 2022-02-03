#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "err.h"

#define BUFFSIZE 1024

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
unsigned long long total;

void increase_and_print_total(size_t filesize) {
    pthread_mutex_lock(&mutex);
    total += filesize;
    printf("total size of uploaded files %llu\n", total);
    pthread_mutex_unlock(&mutex);
}

void *handle_connection(void *s_ptr) {
    int res, sock;
    socklen_t len;
    char buff[BUFFSIZE + 1], peername[BUFFSIZE + 1], peeraddr[BUFFSIZE + 1], filename[BUFFSIZE + 1];
    size_t filesize;
    struct sockaddr_in addr;
    FILE *file;

    sock = *(int *) s_ptr;
    free(s_ptr);

    len = sizeof(addr);

    if (getpeername(sock, (struct sockaddr *) &addr, &len) == -1)
        syserr("getsockname");

    inet_ntop(AF_INET, &addr.sin_addr, peeraddr, BUFFSIZE);
    snprintf(peername, 2 * BUFFSIZE, "%s:%d", peeraddr, ntohs(addr.sin_port));

    memset(buff, 0, sizeof(buff));
    if (read(sock, buff, sizeof(buff) - 1) == -1)
        syserr("read");

    memset(filename, 0, sizeof(filename));
    sscanf(buff, "%ld:%s", &filesize, filename);

    printf("new client %s size=%ld file=%s\n", peername, filesize, filename);

    file = fopen(filename, "w");

    for (;;) {
        memset(buff, 0, sizeof(buff));
        res = read(sock, buff, sizeof(buff));
        if (res == -1)
            syserr("read");
        else if (res == 0) {
            break;
        }
        fputs(buff, file);
    }
    fclose(file);

    printf("client %s has sent its file of size=%ld\n", peername, filesize);
    increase_and_print_total(filesize);
    close(sock);
    return 0;
}

int main(int argc, char *argv[]) {
    int res, sock;
    socklen_t len;
    struct sockaddr_in server;

    if (argc != 2)
        fatal("Usage: %s port", argv[0]);

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
        syserr("socket");

    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    server.sin_port = htons(atoi(argv[1]));
    if (bind(sock, (struct sockaddr *) &server, sizeof(server)) == -1)
        syserr("bind");

    len = (socklen_t) sizeof(server);
    if ((res = getsockname(sock, (struct sockaddr *) &server, &len)) == -1)
        syserr("getsockname");

    printf("Listening at port %d\n", (int) ntohs(server.sin_port));

    if (listen(sock, 5) == -1)
        syserr("listen");

    for (;;) {
        int msgsock;
        int *con;
        pthread_t t;

        if ((msgsock = accept(sock, (struct sockaddr *) NULL, NULL)) == -1)
            syserr("accept");

        if (!(con = malloc(sizeof(int))))
            syserr("malloc");

        *con = msgsock;

        if (pthread_create(&t, 0, handle_connection, con) == -1)
            syserr("pthread_create");

        if (pthread_detach(t) == -1)
            syserr("pthread_detach");
    }


    return 0;
}
