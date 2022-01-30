// Authors:
// Filip Plata, fp371335
// Wojciech Ciszewski, wc385540

#ifndef ATDD_TCP_H
#define ATDD_TCP_H

#include <linux/socket.h>
#include <linux/inet.h>

#include "domain.h"

int tcp_connect(struct socket *conn_socket, struct tcp_addr_info *target);

int tcp_send(struct socket *sock, char *data, int left);

int tcp_read(struct socket *sock, char *buf, int left);

#endif /* ATDD_TCP_H */
