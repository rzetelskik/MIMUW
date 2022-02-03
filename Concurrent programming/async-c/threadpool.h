#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "list.h"
#include <stddef.h>
#include <pthread.h>
#include <stdlib.h>

typedef struct runnable {
  void (*function)(void *, size_t);
  void *arg;
  size_t argsz;
} runnable_t;

typedef struct thread_pool {
    pthread_t *threads;
    volatile size_t num_threads;
    volatile int8_t terminate;
    pthread_mutex_t lock;
    pthread_cond_t idle;
    list_t task_queue;
} thread_pool_t;

int thread_pool_init(thread_pool_t *pool, size_t pool_size);

void thread_pool_destroy(thread_pool_t *pool);

int defer(thread_pool_t *pool, runnable_t runnable);

#endif
