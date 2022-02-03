#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "future.h"
#include "minunit.h"

int tests_run = 0;
static thread_pool_t pool;
static future_t future;

static void *squared(void *arg, size_t argsz __attribute__((unused)),
                     size_t *retsz __attribute__((unused))) {
  int n = *(int *)arg;
  int *ret = malloc(sizeof(int));
  *ret = n * n;
  return ret;
}

static char *test_await_simple() {
  thread_pool_init(&pool, 2);

  int n = 16;
  async(&pool, &future,
        (callable_t){.function = squared, .arg = &n, .argsz = sizeof(int)});
  int *m = await(&future);

  mu_assert("expected 256", *m == 256);
  free(m);

  thread_pool_destroy(&pool);
  return 0;
}

static char *all_tests() {
  mu_run_test(test_await_simple);
  return 0;
}

int main() {
  char *result = all_tests();
  if (result != 0) {
    printf(__FILE__ " %s\n", result);
  } else {
    printf(__FILE__ " ALL TESTS PASSED\n");
  }
  printf(__FILE__ " Tests run: %d\n", tests_run);

  return result != 0;
}
