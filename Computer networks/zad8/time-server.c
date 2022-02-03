#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <time.h>
#include <signal.h>
#include "err.h"

#define BSIZE         1024
#define REPEAT_COUNT  30

int main(int argc, char *argv[]) {

    /* argumenty wywołania programu */
    char *multicast_dotted_address;
    in_port_t local_port;

    /* zmienne i struktury opisujące gniazda */
    int sock;
    struct sockaddr_in local_address;
    struct ip_mreq ip_mreq;

    /* zmienne obsługujące komunikację */
    char buffer[BSIZE];

    time_t time_buffer;
    struct sockaddr src_addr;
    socklen_t addrlen;
    size_t length;

    /* parsowanie argumentów programu */
    if (argc != 3)
        fatal("Usage: %s multicast_dotted_address local_port\n", argv[0]);
    multicast_dotted_address = argv[1];
    local_port = (in_port_t) atoi(argv[2]);

    /* otwarcie gniazda */
    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock < 0)
        syserr("socket");

    /* podłączenie do grupy rozsyłania (ang. multicast) */
    ip_mreq.imr_interface.s_addr = htonl(INADDR_ANY);
    if (inet_aton(multicast_dotted_address, &ip_mreq.imr_multiaddr) == 0) {
        fprintf(stderr, "ERROR: inet_aton - invalid multicast address\n");
        exit(EXIT_FAILURE);
    }
    if (setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *) &ip_mreq, sizeof ip_mreq) < 0)
        syserr("setsockopt");

    /* ustawienie adresu i portu lokalnego */
    local_address.sin_family = AF_INET;
    local_address.sin_addr.s_addr = htonl(INADDR_ANY);
    local_address.sin_port = htons(local_port);
    if (bind(sock, (struct sockaddr *) &local_address, sizeof local_address) < 0)
        syserr("bind");


    for (;;) {
        memset(buffer, 0, BSIZE);
        if (recvfrom(sock, &buffer, BSIZE, 0, &src_addr, &addrlen) < 0)
            syserr("recvfrom");

        printf("Request from: %s\n", inet_ntoa(((struct sockaddr_in *) &src_addr)->sin_addr));

        if (strcmp(buffer, "GET TIME") != 0)
            printf("Received unknown command: %s\n", buffer);
        else {
            time(&time_buffer);
            memset(buffer, 0, BSIZE);
            strncpy(buffer, ctime(&time_buffer), BSIZE);
            length = strnlen(buffer, BSIZE);
            if (sendto(sock, buffer, length, 0, &src_addr, addrlen) < 0)
                syserr("sendto");
        }
    }

    /* odłączenie od grupy rozsyłania */
    if (setsockopt(sock, IPPROTO_IP, IP_DROP_MEMBERSHIP, (void *) &ip_mreq, sizeof ip_mreq) < 0)
        syserr("setsockopt");

    /* koniec */
    close(sock);
    exit(EXIT_SUCCESS);
}
