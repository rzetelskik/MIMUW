#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>

#include "minunit.h"
#include "threadpool.h"

int tests_run = 0;

#define NROUNDS 10

static void pong_ping(void *args, size_t argsz __attribute__((unused))) {
  sem_t *ping = args;
  sem_t *pong = (sem_t *)args + 1;

  for (int i = 0; i < NROUNDS; ++i) {
    sem_wait(ping);
    sem_post(pong);
  }

  sem_destroy(ping);
}

static char *ping_pong() {

  thread_pool_t pool;
  thread_pool_init(&pool, 1);

  sem_t *pingpong = malloc(sizeof(sem_t) * 2);
  sem_t *ping = pingpong;
  sem_t *pong = pingpong + 1;

  sem_init(ping, 0, 0);
  sem_init(pong, 0, 0);

  defer(&pool, (runnable_t){.function = pong_ping,
                            .arg = pingpong,
                            .argsz = sizeof(sem_t) * 2});

  for (int i = 0; i < NROUNDS; ++i) {
    sem_post(ping);
    sem_wait(pong);
  }

  sem_destroy(pong);
  free(pingpong);

  thread_pool_destroy(&pool);
  return 0;
}

static char *all_tests() {
  mu_run_test(ping_pong);
  return 0;
}

int main() {
  char *result = all_tests();
  if (result != 0) {
    printf(__FILE__ ": %s\n", result);
  } else {
    printf(__FILE__ ": ALL TESTS PASSED\n");
  }
  printf(__FILE__ "Tests run: %d\n", tests_run);

  return result != 0;
}
