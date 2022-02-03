#include "future.h"
#include "err.h"
#include <stdio.h>

typedef void *(*function_t)(void *, size_t, size_t *);

typedef struct async_data {
    callable_t callable;
    future_t *future;
} async_data_t;

typedef struct map_data {
    future_t *future;
    future_t *from;
    function_t function;
} map_data_t;

void async_data_init(async_data_t *async_data, callable_t callable, future_t *future) {
    async_data->callable = callable;
    async_data->future = future;
}

void map_data_init(map_data_t *map_data, future_t *future, future_t *from, function_t function) {
    map_data->future = future;
    map_data->from = from;
    map_data->function = function;
}

void future_init(future_t *future) {
    if (pthread_mutex_init(&future->lock, 0) != 0) syserr("pthread_mutex_init error\n");
    if (pthread_cond_init(&future->cond, 0) != 0) syserr("pthread_cond_init error\n");

    future->ready = 0;
    future->retval = NULL;
}

void future_destroy(future_t *future) {
    if (pthread_cond_destroy(&future->cond) != 0) syserr("pthread_cond_destory error\n");
    if (pthread_mutex_destroy(&future->lock) != 0) syserr("pthread_mutex_destory error\n");
}

void async_call(void *arg, __attribute__((unused)) size_t argsz) {
    async_data_t *async_data = (async_data_t *) arg;
    size_t discard;

    if (pthread_mutex_lock(&async_data->future->lock) != 0) syserr("pthread_mutex_lock error\n");

    async_data->future->retval =
            (void *) async_data->callable.function(async_data->callable.arg, async_data->callable.argsz, &discard);
    async_data->future->ready = 1;

    if (pthread_cond_broadcast(&async_data->future->cond) != 0) syserr("pthread_cond_broadcast error\n");
    if (pthread_mutex_unlock(&async_data->future->lock) != 0) syserr("pthread_mutex_unlock error\n");
    free(async_data);
}

int async(thread_pool_t *pool, future_t *future, callable_t callable) {
    async_data_t *async_data = malloc(sizeof(async_data_t));
    if (!async_data) return -1;

    future_init(future);
    async_data_init(async_data, callable, future);

    if (defer(pool,
              (runnable_t) {.function = async_call, .arg = async_data, .argsz = sizeof(*async_data)}) != 0) {
        future_destroy(future);
        free(async_data);
        return -1;
    }

    return 0;
}

void *await(future_t *future) {
    if (pthread_mutex_lock(&future->lock) != 0) syserr("pthread_mutex_lock error\n");

    while (!future->ready) {
        if (pthread_cond_wait(&future->cond, &future->lock) != 0) syserr("pthread_cond_wait error\n");
    }

    if (pthread_mutex_unlock(&future->lock) != 0) syserr("pthread_mutex_unlock error\n");

    return future->retval;
}

void map_call(void *arg, __attribute__((unused)) size_t argsz) {
    map_data_t *map_data = (map_data_t *) arg;
    size_t discard;

    if (pthread_mutex_lock(&map_data->future->lock) != 0) syserr("pthread_mutex_lock error\n");

    map_data->future->retval = (void *) map_data->function(await(map_data->from), 0, &discard);
    map_data->future->ready = 1;

    if (pthread_cond_broadcast(&map_data->future->cond) != 0) syserr("pthread_cond_broadcast error\n");
    if (pthread_mutex_unlock(&map_data->future->lock) != 0) syserr("pthread_mutex_unlock error\n");
    free(map_data);
}

int map(thread_pool_t *pool, future_t *future, future_t *from, function_t function) {
    map_data_t *map_data = malloc(sizeof(map_data_t));
    if (!map_data) return -1;

    future_init(future);
    map_data_init(map_data, future, from, function);

    if (defer(pool, (runnable_t) {.function = map_call, .arg = map_data, .argsz = 0}) != 0) {
        future_destroy(future);
        free(map_data);
        return -1;
    }

    return 0;
}


