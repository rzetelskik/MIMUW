#include "future.h"
#include <stdio.h>
#include <unistd.h>

#define POOL_SIZE 4

typedef struct cell_data {
    u_int64_t time;
    int64_t retval;
} cell_data_t;

void *calc_cell(void *arg, __attribute__((unused)) size_t size, __attribute__((unused)) size_t *retsz) {
    cell_data_t *cell_data = (cell_data_t *) arg;
    usleep(1000 * cell_data->time);
    return cell_data;
}

int main() {
    thread_pool_t pool;
    if (thread_pool_init(&pool, POOL_SIZE) != 0) {
        perror("thread_pool_init error");
        return -1;
    };

    u_int64_t k = 0, n = 0;
    scanf("%lu %lu", &k, &n);

    future_t futures[k][n];

    for (u_int64_t i = 0; i < k; i++) {
        for (u_int64_t j = 0; j < n; j++) {
            cell_data_t *cell_data = malloc(sizeof(cell_data_t));
            if (!cell_data) {
                perror("memory allocation error");
                thread_pool_destroy(&pool);
                return -1;
            }
            scanf("%ld %lu", &cell_data->retval, &cell_data->time);
            if (async(&pool, &futures[i][j],
                      (callable_t) {.function = calc_cell, .arg = cell_data, .argsz = 0}) != 0) {
                perror("async error");
                free(cell_data);
                thread_pool_destroy(&pool);
                return -1;
            };
        }
    }

    for (u_int64_t i = 0; i < k; i++) {
        int64_t retval = 0;
        for (u_int64_t j = 0; j < n; j++) {
            cell_data_t *cell_data = (cell_data_t *) await(&futures[i][j]);
            retval += cell_data->retval;
            free(cell_data);
            future_destroy(&futures[i][j]);
        }
        printf("%ld\n", retval);
    }

    thread_pool_destroy(&pool);
    return 0;
}