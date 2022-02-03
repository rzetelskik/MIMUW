/*
 Ten program używa poll(), aby równocześnie obsługiwać wielu klientów
 bez tworzenia procesów ani wątków.
*/

#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include "err.h"

#define TRUE 1
#define FALSE 0
#define BUF_SIZE 1024

static int finish = FALSE;

static void print_usage(char *arg) {
    fatal("Usage: %s messsage_port diagnostic_port\n", arg);
}

/* Obsługa sygnału kończenia */
static void catch_int(int sig) {
    finish = TRUE;
    fprintf(stderr,
            "Signal %d caught. No new connections will be accepted.\n", sig);
}

void init_socket(int *fd, uint16_t port) {
    struct sockaddr_in server;
    size_t length;

    *fd = socket(PF_INET, SOCK_STREAM, 0);
    if (*fd == -1)
        syserr("Opening stream socket");

    /* Co do adresu nie jesteśmy wybredni */
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    server.sin_port = htons(port);
    if (bind(*fd, (struct sockaddr *) &server,
             (socklen_t)
                     sizeof(server)) == -1)
        syserr("Binding stream socket");

    /* Dowiedzmy się, jaki to port i obwieśćmy to światu */
    length = sizeof(server);
    if (getsockname(*fd, (struct sockaddr *) &server,
                    (socklen_t *) &length) == -1)
        syserr("Getting socket name");
    printf("Socket port #%u\n", (unsigned) ntohs(server.sin_port));

    /* Zapraszamy klientów */
    if (listen(*fd, 5) == -1)
        syserr("Starting to listen");
}

int accept_in(int fd, struct pollfd *client) {
    int msgsock, retval = -1;
    if ((msgsock = accept(fd, (struct sockaddr *) 0, (socklen_t *) 0)) == -1)
        syserr("accept");
    else {
        int i = 2;
        for (; i < _POSIX_OPEN_MAX; ++i) {
            if (client[i].fd == -1) {
                fprintf(stderr, "Received new connection (%d)\n", i);
                client[i].fd = msgsock;
                client[i].events = POLLIN;
                retval = i;
                break;
            }
        }
        if (i >= _POSIX_OPEN_MAX) {
            fprintf(stderr, "Too many clients\n");
            if (close(msgsock) < 0)
                syserr("close");
        }
    }

    return retval;
}

int main(int argc, char *argv[]) {
    struct pollfd client[_POSIX_OPEN_MAX];
    char buf[BUF_SIZE];
    ssize_t rval;
    int activeClients, totalClients, i, ret;
    struct sigaction action;
    sigset_t block_mask;
    uint16_t message_port, diagnostic_port;
    int is_diagnostic[_POSIX_OPEN_MAX];

    if (argc < 3)
        print_usage(argv[0]);

    message_port = strtol(argv[1], 0, 10);
    diagnostic_port = strtol(argv[2], 0, 10);

    fprintf(stderr, "_POSIX_OPEN_MAX = %d\n", _POSIX_OPEN_MAX);

    /* Po Ctrl-C kończymy */
    sigemptyset(&block_mask);
    action.sa_handler = catch_int;
    action.sa_mask = block_mask;
    action.sa_flags = SA_RESTART;

    if (sigaction(SIGINT, &action, 0) == -1)
        syserr("sigaction");

    /* Inicjujemy tablicę z gniazdkami klientów, client[0] to gniazdko centrali */
    for (i = 0; i < _POSIX_OPEN_MAX; ++i) {
        client[i].fd = -1;
        client[i].events = POLLIN;
        client[i].revents = 0;
        is_diagnostic[i] = 0;
    }
    totalClients = activeClients = 0;

    init_socket(&client[0].fd, message_port);
    init_socket(&client[1].fd, diagnostic_port);
    is_diagnostic[1] = 1;

    /* Do pracy */
    do {
        for (i = 0; i < _POSIX_OPEN_MAX; ++i)
            client[i].revents = 0;

        /* Po Ctrl-C zamykamy gniazdko centrali */
        if (finish == TRUE && client[0].fd >= 0) {
            if (close(client[0].fd) < 0)
                syserr("close");
            client[0].fd = -1;
        }

        ret = poll(client, _POSIX_OPEN_MAX, 5000);
        if (ret == -1)
            if (errno == EINTR)
                fprintf(stderr, "Interrupted system call\n");
            else
                syserr("poll");
        else if (ret > 0) {
            if (finish == FALSE && (client[0].revents & POLLIN)) {
                /* Przyjmuję nowe połączenie */
                int retval = accept_in(client[0].fd, client);
                if (retval != -1) {
                    activeClients++;
                    totalClients++;
                    is_diagnostic[retval] = 0;
                }
            }

            if (client[1].revents & POLLIN) {
                int retval = accept_in(client[1].fd, client);
                if (retval != -1)
                    is_diagnostic[retval] = 1;
            }

            for (i = 2; i < _POSIX_OPEN_MAX; ++i) {
                if (client[i].fd != -1 && (client[i].revents & (POLLIN | POLLERR))) {
                    memset(buf, 0, BUF_SIZE);
                    rval = read(client[i].fd, buf, BUF_SIZE);

                    if (is_diagnostic[i]) {
                        if (rval < 0) {
                            fprintf(stderr, "Reading message (%d, %s)\n", errno, strerror(errno));
                        } else if (strcmp(buf, "count\r\n") == 0) {
                            fprintf(stderr, "Read count command\n");
                            size_t len =
                                    snprintf(NULL, 0, "Number of active clients: %d\nTotal number of clients: %d\n",
                                             activeClients, totalClients) + 1;
                            char tmpbuff[len];
                            memset(tmpbuff, 0, len);
                            snprintf(tmpbuff, len, "Number of active clients: %d\nTotal number of clients: %d\n",
                                     activeClients, totalClients);
                            if (write(client[i].fd, tmpbuff, len - 1) != len - 1)
                                syserr("partial / failed write");
                        } else {
                            fprintf(stderr, "Read unrecognised command\n");
                        }

                        fprintf(stderr, "Ending connection\n");
                        if (close(client[i].fd) < 0)
                            syserr("close");
                        client[i].fd = -1;
                    } else {
                        if (rval < 0) {
                            fprintf(stderr, "Reading message (%d, %s)\n", errno, strerror(errno));
                            if (close(client[i].fd) < 0)
                                syserr("close");
                            client[i].fd = -1;
                            activeClients--;
                        } else if (rval == 0) {
                            fprintf(stderr, "Ending connection\n");
                            if (close(client[i].fd) < 0)
                                syserr("close");
                            client[i].fd = -1;
                            activeClients--;
                        } else
                            printf("-->%.*s\n", (int) rval, buf);
                    }
                }
            }
        } else
            fprintf(stderr, "Do something else\n");
    } while (finish == FALSE || activeClients > 0);

    if (client[0].fd >= 0)
        if (close(client[0].fd) < 0)
            syserr("Closing message socket");
    if (client[1].fd >= 0)
        if (close(client[1].fd) < 0)
            syserr("Closing diagnostic socket");
    exit(EXIT_SUCCESS);
}
