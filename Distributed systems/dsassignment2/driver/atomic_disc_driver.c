// Authors:
// Filip Plata, fp371335
// Wojciech Ciszewski, wc385540

#include <linux/init.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/uaccess.h>
#include <linux/kthread.h>
#include <linux/wait.h>
#include <linux/delay.h>

#include <linux/blkdev.h>
#include <linux/genhd.h>

#include "parse_args.h"
#include "cmd.h"
#include "atdd_tcp_client.h"

MODULE_LICENSE("Dual MIT/GPL");
MODULE_AUTHOR("Filip Plata");
MODULE_DESCRIPTION("A driver for atomic disc");
MODULE_VERSION("1.0");

// ATomic Distributed Disc
#define ATDD_BLKDEV_NAME "atdd"
#define SECTORS_NUMBER (1 << 20)
#define ATDD_REQUESTS_BUF_SIZE_BITS 8
#define ATDD_CLIENT_SEND_AHEAD ((uint8_t) 255)
#define ATDD_REQUESTS_BUF_SIZE (1 << ATDD_REQUESTS_BUF_SIZE_BITS)
#define ATDD_THREADS_COUNT (10)

static char *hmac = NULL;
module_param(hmac, charp, 0660);

static char *processes = NULL;
module_param(processes, charp, 0660);

static int ATDD_MAJOR;

static int atdd_open(struct block_device *bdev, fmode_t mode)
{
    return 0;
}

static void atdd_release(struct gendisk *gd, fmode_t mode)
{
}

int atdd_ioctl (struct block_device *dev, fmode_t mode, unsigned cmd, unsigned long arg)
{
    return -ENOTTY; /* ioctl not supported */
}

static struct block_device_operations atdd_ops = {
        .owner = THIS_MODULE,
        .open = atdd_open,
        .release = atdd_release,
        .ioctl = atdd_ioctl
};

struct atdd_request {
  uint64_t ident;
  struct request *content;
};

struct atdd_ktread_data {
  uint8_t buf_tcp_forwarded_pos;
  uint64_t request_ident_counter;
  struct atdd_tcp_client *atdd_c;
  struct atdd_request *requests_buf;
  uint8_t buf_accepted_pos;
  uint8_t buf_completed_pos;
  wait_queue_head_t atdd_op_queue;
};

struct atdd_dev {
  spinlock_t lock;
  struct gendisk *gdisk;
  struct atdd_ktread_data *relay_data[ATDD_THREADS_COUNT];
  struct task_struct *relay_tasks[ATDD_THREADS_COUNT];
  struct atdd_client_config shared_conf;
  uint8_t thread_rq_target;
};

static struct atdd_dev atdd;


struct atdd_ktread_data *atdd_kthread_data_create(struct atdd_client_config *shared_conf, int id)
{
  int err;
  struct atdd_client_config client_config;
  struct atdd_ktread_data *data = kmalloc(sizeof(struct atdd_ktread_data), GFP_KERNEL);

  if (!data) {
    err = -ENOMEM;
    goto err_data;
  }

  init_waitqueue_head(&data->atdd_op_queue);

  data->requests_buf = vmalloc(ATDD_REQUESTS_BUF_SIZE * sizeof(struct atdd_request));
  if (!data->requests_buf) {
    err = -ENOMEM;
    goto err_requests_buf;
  }

  data->buf_accepted_pos = 0;
  data->buf_completed_pos = 0;
  data->buf_tcp_forwarded_pos = 0;
  data->request_ident_counter = id;

  memcpy(&client_config, shared_conf, sizeof(struct atdd_client_config));
  client_config.client_state_change_wq = &data->atdd_op_queue;
  client_config.preferred_addr = id % client_config.addrs_count;
  data->atdd_c = atdd_client_create(client_config);
  if (IS_ERR(data->atdd_c)) {
    err = PTR_ERR(data->atdd_c);
    goto err_atdd_client;
  }

  return data;
  err_atdd_client:
  vfree(data->requests_buf);
  err_requests_buf:
  kfree(data);
  err_data:
  return ERR_PTR(err);
}

static void atdd_kthread_data_free(struct atdd_ktread_data *data)
{
  atdd_client_delete(data->atdd_c);
  vfree(data->requests_buf);
  kfree(data);
}

int convert_atdd_to_blk_status(enum status_code rcode)
{
  switch (rcode) {
    case OK:
      return 0;
    case AUTH_FAILURE:
      return -EIO;
    case INVALID_SECTOR_INDEX:
      return -EIO;
    default:
      return -EINVAL;
  }
}

static int atdd_find_request_pos(struct atdd_request *buf, uint64_t ident) {
  int pos;
  for (pos = 0; pos < ATDD_REQUESTS_BUF_SIZE; ++pos) {
    if (buf[pos].ident == ident) {
      return pos;
    }
  }
  return -1;
}

static int atdd_finish_all_completed_requests(struct atdd_ktread_data *data)
{
  int err;
  int rq_status;
  struct atdd_cmd_return cmd_r;
  struct request *rq;
  int rq_pos;

  while ((err = atdd_fetch_command(data->atdd_c, &cmd_r)) > 0) {
    rq_pos = atdd_find_request_pos(data->requests_buf, cmd_r.ident);
    if (rq_pos < 0) {
      printk(KERN_WARNING "Command completed unsuccessfully");
      blk_end_request_all(rq, -EIO);
      continue;
    }

    rq = data->requests_buf[rq_pos].content;
    data->requests_buf[rq_pos] = data->requests_buf[data->buf_completed_pos];
    data->requests_buf[data->buf_completed_pos] = (struct atdd_request){
      .ident = cmd_r.ident,
      .content = rq
    };
    ++data->buf_completed_pos;
    rq_status = convert_atdd_to_blk_status(cmd_r.rcode);

    if (rq_status != 0) {
      printk(KERN_WARNING "Command completed unsuccessfully");
      blk_end_request_all(rq, rq_status);
      continue;
    }

    /* read, data must be copied */
    if (rq_data_dir(rq) == 0)
      memcpy(bio_data(rq->bio), cmd_r.data, SECTOR_SIZE);

    /* If command finishes */
    blk_end_request_all(rq, 0);
  }

  return err < 0 ? err : 0;
}

static size_t atdd_cmds_to_send(struct atdd_ktread_data *data)
{
  return min((uint8_t) (data->buf_accepted_pos - data->buf_tcp_forwarded_pos),
    (uint8_t) (ATDD_CLIENT_SEND_AHEAD - (uint8_t) (data->buf_tcp_forwarded_pos - data->buf_completed_pos)));
}

static int atdd_send_accepted_requests(struct atdd_ktread_data *data)
{
  int err;
  size_t cmds_to_send = atdd_cmds_to_send(data);

  while (cmds_to_send--) {
    if ((err = atdd_send_command(
        data->atdd_c, data->requests_buf[data->buf_tcp_forwarded_pos].content,
        data->requests_buf[data->buf_tcp_forwarded_pos].ident)) < 0)
      return err;

    data->buf_tcp_forwarded_pos++;
  }

  return 0;
}

static int atdd_main(void *atdd_data)
{
  int err;
  struct atdd_ktread_data *data = atdd_data;

  while (true) {
    wait_event_interruptible(data->atdd_op_queue,\
      kthread_should_stop() || atdd_client_has_data(data->atdd_c) ||
      atdd_cmds_to_send(data) != 0 ||
      atdd_client_has_error(data->atdd_c));

    if (kthread_should_stop())
      break;

    if (atdd_client_has_error(data->atdd_c)) {
      printk(KERN_WARNING "connection error");
      data->buf_tcp_forwarded_pos = data->buf_completed_pos;
      atdd_client_clear_error(data->atdd_c);
    }

    if (atdd_client_has_data(data->atdd_c) &&
        (err = atdd_finish_all_completed_requests(data)) < 0) {
        printk(KERN_WARNING "fetch error");
        data->buf_tcp_forwarded_pos = data->buf_completed_pos;
    }

    if ((err = atdd_send_accepted_requests(data)) < 0) {
      printk(KERN_WARNING "send error");
      data->buf_tcp_forwarded_pos = data->buf_completed_pos;
      msleep(500);
    }
  }

  atdd_kthread_data_free(data);
  return 0;
}

static int submit_atdd_request(struct atdd_ktread_data* relay,
    struct request *rq)
{
  if ((relay->buf_accepted_pos - relay->buf_completed_pos) == 255) {
    printk(KERN_DEBUG "Too fast!");
    return -ENOSPC;
  }
  relay->requests_buf[relay->buf_accepted_pos] = (struct atdd_request) {
    .ident = relay->request_ident_counter,
    .content = rq
  };
  ++relay->buf_accepted_pos;
  relay->request_ident_counter += ATDD_THREADS_COUNT;
  wake_up(&relay->atdd_op_queue);
  return 0;
}

static void atdd_block_request(struct request_queue *q)
{
    struct request *rq;
    struct atdd_dev *dev = q->queuedata;
    uint8_t target_thread;
    int err;

    while (true) {
        rq = blk_fetch_request(q);
        if (rq == NULL)
            break;

        if (blk_rq_is_passthrough(rq)) {
            printk(KERN_NOTICE "Skip non-fs request\n");
            __blk_end_request_all(rq, -EIO);
            continue;
        }

        target_thread = dev->thread_rq_target;
        while (true) {
          if ((err = submit_atdd_request(dev->relay_data[target_thread++], rq)) == 0)
            break;
          target_thread %= ATDD_THREADS_COUNT;
          if (target_thread == dev->thread_rq_target) {
            err = -ENOSPC;
            break;
          }
        };

        dev->thread_rq_target++;
        dev->thread_rq_target %= ATDD_THREADS_COUNT;

        if (err) {
          blk_requeue_request(q, rq);
          break;
        }
    }
}

static int atdd_dev_init(struct atdd_dev *dev)
{
  int err, i_data;

  memset(dev, '\0', sizeof(struct atdd_dev));
  dev->thread_rq_target = 0;

  dev->shared_conf.hmac_key = parse_hmac_key(hmac);
  if (IS_ERR(dev->shared_conf.hmac_key)) {
    err = PTR_ERR(dev->shared_conf.hmac_key);
    goto err_hmac_key;
  }

  if ((err = parse_atdd_addrs(&dev->shared_conf, processes)))
    goto err_atdd_addrs;

  for (i_data = 0; i_data < ATDD_THREADS_COUNT; i_data++) {
    dev->relay_data[i_data] = atdd_kthread_data_create(&dev->shared_conf, i_data);
    if (IS_ERR(dev->relay_data[i_data])) {
      err = PTR_ERR(dev->relay_data[i_data]);
      goto err_kthread_run;
    }

    dev->relay_tasks[i_data] = kthread_run(atdd_main, dev->relay_data[i_data],
      "atdd-tcp-relay-%d", i_data);
    if (IS_ERR(dev->relay_tasks[i_data])) {
      err = PTR_ERR(dev->relay_tasks[i_data]);
      atdd_kthread_data_free(dev->relay_data[i_data]);
      goto err_kthread_run;
    }
  }

  dev->gdisk = alloc_disk(1);
  if (!dev->gdisk) {
      err = -ENOMEM;
      goto err_disk;
  }

  spin_lock_init(&dev->lock);
  snprintf(dev->gdisk->disk_name, strlen(ATDD_BLKDEV_NAME) + 1,
    ATDD_BLKDEV_NAME);
  dev->gdisk->flags = GENHD_FL_NO_PART_SCAN;

  dev->gdisk->major = ATDD_MAJOR;
  dev->gdisk->fops = &atdd_ops;
  dev->gdisk->first_minor = 0;

  dev->gdisk->queue = blk_init_queue(atdd_block_request, &dev->lock);
  blk_queue_logical_block_size(dev->gdisk->queue, SECTOR_SIZE);
  queue_flag_set(QUEUE_FLAG_NOMERGES, dev->gdisk->queue);
  blk_queue_max_hw_sectors(dev->gdisk->queue, SECTOR_SIZE / 512);

  set_capacity(dev->gdisk, SECTORS_NUMBER);
  dev->gdisk->queue->queuedata = dev;

  add_disk(dev->gdisk);

  return 0;

  err_disk:
  err_kthread_run:
  while (i_data > 0)
    kthread_stop(dev->relay_tasks[--i_data]);
  kfree(dev->shared_conf.atdd_addrs);
  err_atdd_addrs:
  kfree(dev->shared_conf.hmac_key);
  err_hmac_key:
  return err;
}

static void delete_atdd_dev(struct atdd_dev *dev)
{
  int i;
  blk_cleanup_queue(dev->gdisk->queue);
  del_gendisk(dev->gdisk);
  put_disk(dev->gdisk);

  for (i = 0; i < ATDD_THREADS_COUNT; i++)
    kthread_stop(dev->relay_tasks[i]);

  kfree(dev->shared_conf.hmac_key);
  kfree(dev->shared_conf.atdd_addrs);
}

static int __init atdd_init(void)
{
  int err = 0;

  if (hmac == NULL) {
    printk(KERN_ERR "hmac: parameter not provided\n");
    return -EINVAL;
  }

  if (processes == NULL) {
    printk(KERN_ERR "processes: parameter not provided\n");
    return -EINVAL;
  }

  err = register_blkdev(ATDD_MAJOR, ATDD_BLKDEV_NAME);
  if (err < 0) {
    printk(KERN_ERR "register_blkdev: unable to register\n");
    return err;
  }
  ATDD_MAJOR = err;

  if ((err = atdd_dev_init(&atdd)) < 0)
    unregister_blkdev(ATDD_MAJOR, ATDD_BLKDEV_NAME);

  return err;
}

static void __exit atdd_exit(void)
{
    delete_atdd_dev(&atdd);
    unregister_blkdev(ATDD_MAJOR, ATDD_BLKDEV_NAME);
}

module_init(atdd_init);
module_exit(atdd_exit);
