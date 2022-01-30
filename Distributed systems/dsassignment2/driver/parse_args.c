#include <linux/string.h>
#include <linux/ctype.h>
#include <linux/inet.h>
#include <linux/slab.h>

#include "parse_args.h"

#define MAX_PROCESSES 32

static int hex_digit_to_int(char d)
{
  if (d <= '9')
    return d - '0';

  if (d <= 'F')
    return d - 'A' + 10;

  return d - 'a' + 10;
}

char *parse_hmac_key(char *hmac_key_desc)
{
  int i;
  char *res;
  if (strlen(hmac_key_desc) != HMAC_KEY_SIZE * 2)
    return ERR_PTR(-EINVAL);

  for (i = 0; i < HMAC_KEY_SIZE * 2; i++) {
    if (!isxdigit(hmac_key_desc[i]))
      return ERR_PTR(-EINVAL);
  }

  res = kmalloc(HMAC_KEY_SIZE, GFP_KERNEL);
  if (!res)
    return ERR_PTR(-ENOMEM);

  for (i = 0; i < HMAC_KEY_SIZE; i++) {
    res[i] = (hex_digit_to_int(hmac_key_desc[2 * i]) << 4) +
             hex_digit_to_int(hmac_key_desc[2 * i + 1]);
  }

  return res;
}

int parse_atdd_addrs(struct atdd_client_config *config, char *atdd_addrs_desc)
{
  struct tcp_addr_info addrs[MAX_PROCESSES];
  int i = 0, addrs_count = 0, err;
  char *parse_begin = atdd_addrs_desc, *parse_end = atdd_addrs_desc;

  while (true) {
    if (*parse_end == ':' && i == 0) {
      *parse_end = '\0';
      parse_end++;
      addrs[addrs_count].ip_addr = in_aton(parse_begin);
      i = 1;
      parse_begin = parse_end;
    } else if (*parse_end == ',' && i == 1) {
      *parse_end = '\0';
      if ((err = kstrtoint(parse_begin, 10, &(addrs[addrs_count++].port))))
        goto err_parse;
      parse_end++;
      i = 0;
      parse_begin = parse_end;
    } else if (*parse_end == '\0' && i == 1) {
      if ((err = kstrtoint(parse_begin, 10, &(addrs[addrs_count++].port))))
        goto err_parse;
      break;
    } else if (*parse_end == '\0') {
      // unexpected EOL
      err = -EINVAL;
      goto err_parse;
    } else if (addrs_count > MAX_PROCESSES) {
      printk(KERN_WARNING "Limit of target processes: %d was reached\n", MAX_PROCESSES);
      err = -EINVAL;
      goto err_parse;
    } else {
      parse_end++;
    }
  }

  config->addrs_count = addrs_count;
  config->atdd_addrs = kmalloc(addrs_count * sizeof(struct tcp_addr_info), GFP_KERNEL);

  if (!config->atdd_addrs) {
    err = -ENOMEM;
    goto err_parse;
  }
  memcpy(config->atdd_addrs, &addrs, addrs_count * sizeof(struct tcp_addr_info));

  return 0;
  err_parse:
  return err;
}
