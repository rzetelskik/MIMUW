// Authors:
// Filip Plata, fp371335
// Paweł Charyło, pc394130
// Wojciech Ciszewski, wc385540

#include <linux/net.h>
#include <net/sock.h>
#include <linux/tcp.h>
#include <linux/in.h>
#include <asm/uaccess.h>
#include <linux/socket.h>

#include "tcp.h"

int tcp_connect(struct socket *conn_socket, struct tcp_addr_info *target)
{
  struct sockaddr_in saddr;

  memset(&saddr, '\0', sizeof(saddr));

  saddr.sin_family = AF_INET;
  saddr.sin_port = htons(target->port);
  saddr.sin_addr.s_addr = target->ip_addr;

  return kernel_connect(conn_socket, (struct sockaddr *)&saddr,
                  sizeof(saddr), O_RDWR);
}

int tcp_send(struct socket *sock, char *data, int left)
{
  struct msghdr msg;
  int written = 0, len;
  mm_segment_t oldmm;
  struct kvec vec;

  msg.msg_name    = 0;
  msg.msg_namelen = 0;

  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  oldmm = get_fs();
  set_fs(KERNEL_DS);

  while (left > 0) {
    vec.iov_len = left;
    vec.iov_base = (char *) data + written;

    len = kernel_sendmsg(sock, &msg, &vec, 1, left);
    if (len >= 0) {
      written += len;
      left -= len;
    } else if(len != -ERESTARTSYS && len != -EAGAIN)
        break;
  }

  set_fs(oldmm);
  return len < 0 ? len : 0;
}

int tcp_read(struct socket *sock, char *buf, int left)
{
  struct msghdr msg = (struct msghdr) {
    .msg_name = 0,
    .msg_namelen = 0,
    .msg_control = NULL,
    .msg_controllen = 0,
    .msg_flags = MSG_DONTWAIT
  };
  struct kvec vec;
  int res, read = 0;

  while (left > 0) {
    vec.iov_len = left;
    vec.iov_base = buf + read;
    res = kernel_recvmsg(sock, &msg, &vec, 1, left, MSG_DONTWAIT);
    if (res > 0) {
      read += res;
      left -= res;
    } else if (res != -ERESTARTSYS && res != -EAGAIN)
      break;
  }

  if (left == 0) {
    return 0;
  }

  if (res == 0) {
    return -ECONNRESET;
  }

  return res;
}
