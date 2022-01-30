#ifndef ATDD_PARSE_ARGS_H
#define ATDD_PARSE_ARGS_H

#include "domain.h"

char *parse_hmac_key(char *hmac_key_desc);

int parse_atdd_addrs(struct atdd_client_config *config, char *atdd_addrs_desc);

#endif /* ATDD_PARSE_ARGS_H */
