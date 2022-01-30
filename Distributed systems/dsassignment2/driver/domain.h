// Authors:
// Filip Plata, fp371335
// Wojciech Ciszewski, wc385540

#ifndef ATDD_DOMAIN_H
#define ATDD_DOMAIN_H

#define HMAC_KEY_SIZE 32
#define SECTOR_SIZE 4096
#define ATDD_MAGIC "atdd"

struct tcp_addr_info {
  uint32_t ip_addr;
  int port;
};

struct atdd_client_config {
  struct tcp_addr_info *atdd_addrs;
  size_t addrs_count;
  size_t preferred_addr;
  char *hmac_key;
  wait_queue_head_t *client_state_change_wq;
};

#endif /* ATDD_DOMAIN_H */
