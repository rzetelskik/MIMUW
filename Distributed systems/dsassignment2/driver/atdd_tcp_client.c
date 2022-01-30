// Authors:
// Filip Plata, fp371335
// Wojciech Ciszewski, wc385540

#include <net/sock.h>
#include <linux/slab.h>
#include <crypto/hash.h>

#include "cmd.h"
#include "atdd_tcp_client.h"

#define SEND_BUF_POS_START 7

struct atdd_pending_cmd {
  uint64_t cmd;
};

struct atdd_cmd_parse {
  enum status_code st;
  uint64_t rq_ident;
  bool completed;
};

struct pending_buf {
  struct atdd_pending_cmd *cmds;
  uint8_t completed_pos;
  uint8_t accepted_pos;
};

struct atdd_tcp_client {
  struct atdd_client_config config;
  struct socket *sock;
  size_t connection_target;
  bool connected;
  bool error;
  char *send_buf;
  size_t send_buf_pos;
  struct pending_buf pending;
  struct crypto_shash* hmac_alg;
  char *parse_buf;
  size_t parse_buf_pos;
  struct atdd_cmd_parse parse_state;
  wait_queue_head_t *client_state_change_wq;
};

void atdd_client_delete(struct atdd_tcp_client *atdd_c)
{
  crypto_free_shash(atdd_c->hmac_alg);
  if (atdd_c->connected)
    sock_release(atdd_c->sock);
  kfree(atdd_c->pending.cmds);
  kfree(atdd_c->send_buf);
  kfree(atdd_c);
}

static void atdd_sk_data_ready(struct sock *sk)
{
  wake_up(((struct atdd_tcp_client *) sk->sk_user_data)->client_state_change_wq);
}

static void atdd_sk_error_report(struct sock *sk)
{
  struct atdd_tcp_client *atdd_c = sk->sk_user_data;
  atdd_c->error = true;
  wake_up(atdd_c->client_state_change_wq);
}

static void setup_send_buf(struct atdd_tcp_client *atdd_c)
{
    strcpy(atdd_c->send_buf, ATDD_MAGIC);
}

static int atdd_create_sock(struct atdd_tcp_client *atdd_c) {
  int err;
  if ((err = sock_create_kern(&init_net, PF_INET, SOCK_STREAM, IPPROTO_TCP, &atdd_c->sock)))
    return err;

  atdd_c->client_state_change_wq = atdd_c->config.client_state_change_wq;
  atdd_c->sock->sk->sk_user_data = atdd_c;
  atdd_c->sock->sk->sk_data_ready = atdd_sk_data_ready;
  atdd_c->sock->sk->sk_error_report = atdd_sk_error_report;

  return 0;
}

struct atdd_tcp_client * atdd_client_create(struct atdd_client_config config)
{
  int err;
  struct atdd_tcp_client *atdd_c;

  atdd_c = kmalloc(sizeof(struct atdd_tcp_client), GFP_KERNEL);
  if (!atdd_c) {
    err = -ENOMEM;
    goto err_atdd_c;
  }
  memset(atdd_c, '\0', sizeof(struct atdd_tcp_client));
  atdd_c->config = config;
  atdd_c->connected = false;
  atdd_c->connection_target = config.addrs_count;

  atdd_c->send_buf = kmalloc(MAX_CMD_SIZE, GFP_KERNEL);
  if (!atdd_c->send_buf) {
    err = -ENOMEM;
    goto err_send_buf;
  }

  atdd_c->pending.cmds = kmalloc(256 * sizeof(struct atdd_pending_cmd), GFP_KERNEL);
  if (!atdd_c->pending.cmds) {
    err = -ENOMEM;
    goto err_pending_buf;
  }

  atdd_c->parse_buf_pos = 0;
  atdd_c->parse_buf = kmalloc(MAX_RESPONSE_SIZE, GFP_KERNEL);
  if (!atdd_c->parse_buf) {
    err = -ENOMEM;
    goto err_parse_buf;
  }

  atdd_c->hmac_alg = crypto_alloc_shash("hmac(sha256)", CRYPTO_ALG_TYPE_SHASH, 0);
  if (IS_ERR(atdd_c->hmac_alg)) {
    err = PTR_ERR(atdd_c->hmac_alg);
    goto err_hmac_alg;
  }
  BUG_ON(crypto_shash_digestsize(atdd_c->hmac_alg) != HMAC_TAG_SIZE);

  if ((err = crypto_shash_setkey(atdd_c->hmac_alg, atdd_c->config.hmac_key, HMAC_KEY_SIZE)))
    goto err_shash_setkey;

  if ((err = atdd_create_sock(atdd_c)))
    goto err_sock;

  setup_send_buf(atdd_c);
  return atdd_c;
  err_sock:
  err_shash_setkey:
  crypto_free_shash(atdd_c->hmac_alg);
  err_hmac_alg:
  kfree(atdd_c->parse_buf);
  err_parse_buf:
  kfree(atdd_c->pending.cmds);
  err_pending_buf:
  kfree(atdd_c->send_buf);
  err_send_buf:
  kfree(atdd_c);
  err_atdd_c:
  return ERR_PTR(err);
}

static void atdd_disconnect(struct atdd_tcp_client *atdd_c)
{
  atdd_c->connected = false;
  atdd_c->sock->ops->release(atdd_c->sock);
  atdd_c->parse_buf_pos = 0;
  atdd_c->pending.accepted_pos = atdd_c->pending.completed_pos;
  atdd_create_sock(atdd_c);
}

static int hmac_tag_prepare(struct atdd_tcp_client *atdd_c)
{
  SHASH_DESC_ON_STACK(shash, atdd_c->hmac_alg);

  shash->tfm = atdd_c->hmac_alg;
  shash->flags = 0x0;
  return crypto_shash_digest(shash, atdd_c->send_buf,
    atdd_c->send_buf_pos, atdd_c->send_buf + atdd_c->send_buf_pos);
}

int prepare_cmd_content(struct atdd_tcp_client *atdd_c, struct request *rq, uint64_t ident)
{
  int err;
  uint64_t temp;

  atdd_c->send_buf_pos = SEND_BUF_POS_START;
  atdd_c->send_buf[atdd_c->send_buf_pos++] = rq_data_dir(rq) == 1 ? WRITE_CMD : READ_CMD;

  temp = cpu_to_be64(ident);
  memcpy(atdd_c->send_buf + atdd_c->send_buf_pos, &temp, sizeof(temp));
  atdd_c->send_buf_pos += 8;

  temp = cpu_to_be64(blk_rq_pos(rq) >> 3);
  memcpy(atdd_c->send_buf + atdd_c->send_buf_pos, &temp, sizeof(temp));
  atdd_c->send_buf_pos += 8;

  // write requires sector of bytes to write
  if (atdd_c->send_buf[7] == WRITE_CMD) {
    memcpy(atdd_c->send_buf + atdd_c->send_buf_pos, bio_data(rq->bio), SECTOR_SIZE);
    atdd_c->send_buf_pos += SECTOR_SIZE;
  }

  if ((err = hmac_tag_prepare(atdd_c)))
    return err;

  atdd_c->send_buf_pos += HMAC_TAG_SIZE;
  return 0;
}

int send_atdd_cmd_over_tcp(struct atdd_tcp_client *atdd_c)
{
  int i, err;
  if (!atdd_c->connected && atdd_c->connection_target != atdd_c->config.preferred_addr) {
    if ((err = tcp_connect(atdd_c->sock, atdd_c->config.atdd_addrs + atdd_c->config.preferred_addr)) == 0) {
      atdd_c->connection_target = atdd_c->config.preferred_addr;
      atdd_c->connected = true;
      atdd_c->error = false;
    }
  }
  if (!atdd_c->connected) {
    for (i = 0; i < atdd_c->config.addrs_count; i++) {
      atdd_c->connection_target = (atdd_c->connection_target + 1) % atdd_c->config.addrs_count;
      if ((err = tcp_connect(atdd_c->sock, atdd_c->config.atdd_addrs + atdd_c->connection_target)) == 0) {
        atdd_c->connected = true;
        atdd_c->error = false;
        break;
      }
    }

    if (!atdd_c->connected)
      return -EIO;
  }

  if ((err = tcp_send(atdd_c->sock, atdd_c->send_buf, atdd_c->send_buf_pos)))
    atdd_disconnect(atdd_c);

  return err;
}

int atdd_send_command(struct atdd_tcp_client *atdd_c, struct request *rq, uint64_t ident)
{
  int err;

  if ((err = prepare_cmd_content(atdd_c, rq, ident)))
    return err;

  if ((err = send_atdd_cmd_over_tcp(atdd_c)))
    return err;

  atdd_c->pending.cmds[atdd_c->pending.accepted_pos++] = (struct atdd_pending_cmd) {
    .cmd = ident
  };

  return err;
}


bool atdd_client_has_error(struct atdd_tcp_client *atdd_c)
{
  return atdd_c->error;
}

void atdd_client_clear_error(struct atdd_tcp_client *atdd_c) {
  atdd_c->error = false;
}

bool atdd_client_has_data(struct atdd_tcp_client *atdd_c)
{
  return atdd_c->sock->sk != NULL &&
    !skb_queue_empty(&atdd_c->sock->sk->sk_receive_queue) &&
    atdd_c->pending.accepted_pos != atdd_c->pending.completed_pos;
}

static int parse_status_code(enum status_code *code, char byte)
{
  switch (byte) {
      case 0:
        *code = OK;
        return 0;
      case 1:
        *code = AUTH_FAILURE;
        return 0;
      case 2:
        *code = INVALID_SECTOR_INDEX;
        return 0;
      default:
        return -EINVAL;
  }
}

static int slide_to_atdd_magic(struct atdd_tcp_client *atdd_c)
{
  int slide = 0;
  while (slide + strlen(ATDD_MAGIC) <= atdd_c->parse_buf_pos) {
    if (memcmp(ATDD_MAGIC, atdd_c->parse_buf + slide, strlen(ATDD_MAGIC)) == 0)
      break;
    slide++;
  }

  BUG_ON(slide != 0);

  if (slide != 0) {
    memmove(atdd_c->parse_buf, atdd_c->parse_buf + slide, atdd_c->parse_buf_pos - slide);
    atdd_c->parse_buf_pos -= slide;
  }
  return atdd_c->parse_buf_pos >= strlen(ATDD_MAGIC) ? 0 : -EINVAL;
}

static int progress_tcp_data_to_limit(struct atdd_tcp_client *atdd_c, size_t limit)
{
  int res;
  if ((res = tcp_read(atdd_c->sock, atdd_c->parse_buf + atdd_c->parse_buf_pos,
      limit - atdd_c->parse_buf_pos)) < 0)
    return res;
  atdd_c->parse_buf_pos = limit;
  return 0;
}

static int progress_tcp_data(struct atdd_tcp_client *atdd_c)
{
  int res, limit = MIN_RESPONSE_SIZE;

  while (atdd_c->parse_buf_pos < limit) {
    if ((res = progress_tcp_data_to_limit(atdd_c, limit)) < 0)
      return res;

    slide_to_atdd_magic(atdd_c);
  }

  if (atdd_c->parse_buf_pos == limit) {
    if ((res = parse_status_code(&atdd_c->parse_state.st, atdd_c->parse_buf[6])) < 0) {
      printk(KERN_WARNING "Invalid response status code");
      atdd_c->parse_buf_pos = 0;
      return res;
    }
  }
  atdd_c->parse_state.rq_ident = be64_to_cpu(*((uint64_t *) (atdd_c->parse_buf + 8)));

  if (atdd_c->parse_state.st == OK && atdd_c->parse_buf[7] == (0x40 + READ_CMD))
    limit += SECTOR_SIZE;

  if ((res = progress_tcp_data_to_limit(atdd_c, limit)) < 0)
    return res;

  atdd_c->parse_state.completed = true;
  return 0;
}

int validate_cmd_parse_hmac(struct atdd_tcp_client *atdd_c)
{
  int err;
  char hmac_signature[HMAC_TAG_SIZE];
  SHASH_DESC_ON_STACK(shash, atdd_c->hmac_alg);

  shash->tfm = atdd_c->hmac_alg;
  shash->flags = 0x0;
  if ((err = crypto_shash_digest(shash, atdd_c->parse_buf,
    atdd_c->parse_buf_pos - HMAC_TAG_SIZE, hmac_signature)))
    return err;

  return memcmp(hmac_signature, atdd_c->parse_buf + atdd_c->parse_buf_pos - HMAC_TAG_SIZE,
    HMAC_TAG_SIZE) == 0 ? 0 : -EINVAL;
}

int atdd_fetch_command(struct atdd_tcp_client *atdd_c, struct atdd_cmd_return *cmd_r)
{
  int err;

  if (atdd_c->pending.accepted_pos == atdd_c->pending.completed_pos)
    return 0;

  if ((err = progress_tcp_data(atdd_c)))
    return err;

  if (!atdd_c->parse_state.completed) {
    return 0;
  }

  if ((err = validate_cmd_parse_hmac(atdd_c)) < 0) {
    printk(KERN_WARNING "Invalid hmac tag");
    atdd_c->parse_buf_pos = 0;
    return err;
  }

  cmd_r->data = atdd_c->parse_buf + ATDD_RESPONSE_HEADER_SIZE;
  cmd_r->ident = atdd_c->parse_state.rq_ident;
  cmd_r->rcode = atdd_c->parse_state.st;
  atdd_c->parse_buf_pos = 0;
  atdd_c->parse_state.completed = false;
  atdd_c->pending.completed_pos++;

  return 1;
}
