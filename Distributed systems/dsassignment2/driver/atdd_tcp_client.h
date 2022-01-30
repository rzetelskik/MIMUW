// Authors:
// Filip Plata, fp371335
// Wojciech Ciszewski, wc385540

#ifndef ATDD_TCP_CLIENT_H
#define ATDD_TCP_CLIENT_H

#include <linux/blkdev.h>

#include "domain.h"
#include "tcp.h"

struct atdd_tcp_client;

// initializes with information about atdd processes location
struct atdd_tcp_client *atdd_client_create(struct atdd_client_config config);

void atdd_client_delete(struct atdd_tcp_client *atdd_c);

// accepts a command for processing
int atdd_send_command(struct atdd_tcp_client *atdd_c, struct request *rq, uint64_t ident);

// returns whether there is some data and atdd_fetch_command should be called
bool atdd_client_has_data(struct atdd_tcp_client *atdd_c);

bool atdd_client_has_error(struct atdd_tcp_client *atdd_c);

void atdd_client_clear_error(struct atdd_tcp_client *atdd_c);

// returns first completed command if present, 0 otherwise
int atdd_fetch_command(struct atdd_tcp_client *atdd_c, struct atdd_cmd_return *res);

#endif /* ATDD_TCP_CLIENT_H */
