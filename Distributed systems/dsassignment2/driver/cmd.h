#ifndef ATDD_CMD_H
#define ATDD_CMD_H

#define ATDD_RESPONSE_HEADER_SIZE 16
#define HMAC_TAG_SIZE 32
#define ATDD_REQUEST_HEADER_SIZE 24
#define MAX_RESPONSE_SIZE (MIN_RESPONSE_SIZE + SECTOR_SIZE)
#define MIN_RESPONSE_SIZE (ATDD_RESPONSE_HEADER_SIZE + HMAC_TAG_SIZE)
#define MAX_CMD_SIZE (ATDD_REQUEST_HEADER_SIZE + HMAC_TAG_SIZE + SECTOR_SIZE)

enum status_code {
  OK,
  AUTH_FAILURE,
  INVALID_SECTOR_INDEX,
};

enum cmd_type {
  INVALID_CMD_TYPE,
  READ_CMD,
  WRITE_CMD
};

struct atdd_cmd_return {
  uint64_t ident;
  char *data;
  enum status_code rcode;
};

#endif /* ATDD_CMD_H */
