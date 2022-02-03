#include "future.h"
#include <stdio.h>
#include <zconf.h>

#define POOL_SIZE 3

typedef struct iter {
    u_int64_t k;
    u_int64_t retval;
} iter_t;

void *multiply(void *arg, __attribute__((unused)) size_t size, __attribute__((unused)) size_t *retsz) {
    iter_t *iter = (iter_t *) arg;
    iter->retval *= iter->k++;
    return iter;
}

void destroy_futures(future_t *future, u_int64_t last_initialised) {
    for (u_int64_t i = 0; i < last_initialised; i++) {
        future_destroy(&future[i]);
    }
}

int main() {
    thread_pool_t pool;
    if (thread_pool_init(&pool, POOL_SIZE) != 0) {
        perror("thread_pool_init error");
        return -1;
    }

    u_int64_t n, k = 0;
    iter_t iter = {.k = 1, .retval = 1};

    scanf("%ld", &n);
    future_t futures[n ? n : 1];


    if (async(&pool, &futures[k],
              (callable_t) {.function = multiply, .arg = &iter, .argsz = sizeof(iter_t)}) != 0) {
        perror("async error");
        thread_pool_destroy(&pool);
        destroy_futures(futures, k);
        return -1;
    };

    while (++k < n) {
        if (map(&pool, &futures[k], &futures[k - 1], multiply) != 0) {
            perror("map error");
            thread_pool_destroy(&pool);
            destroy_futures(futures, k);
            return -1;
        };
    }

    await(&futures[k - 1]);
    printf("%lu\n", iter.retval);

    thread_pool_destroy(&pool);

    return 0;
}