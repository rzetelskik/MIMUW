#include "threadpool.h"
#include "err.h"
#include <stdio.h>
#include <signal.h>

pthread_rwlock_t no_defer_lock;
int8_t no_defer;
list_t threadpool_list;
struct sigaction act;
struct sigaction oldact;
sigset_t block_mask;

void thread_pool_handler_terminate(int sig, siginfo_t *siginfo, void *discard);

void set_sigint_block() {
    sigemptyset(&block_mask);
    sigaddset(&block_mask, SIGINT);
}

void set_sigaction() {
    act.sa_sigaction = thread_pool_handler_terminate;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGINT, &act, &oldact) != 0) syserr("sigaction error\n");
}

__attribute__((constructor))
void thread_pool_handler_init() {
    if (pthread_rwlock_init(&no_defer_lock, 0) != 0) syserr("pthread_mutex_init error\n");
    no_defer = 0;
    list_init(&threadpool_list);
    set_sigint_block();
    set_sigaction();
}

__attribute__((destructor))
void thread_pool_handler_destroy() {
    list_destroy(&threadpool_list);
    if (pthread_rwlock_destroy(&no_defer_lock) != 0) syserr("pthread_mutex_destroy error\n");
}

void thread_pool_destroy_all() {
    void *pool = list_front(&threadpool_list);
    while (pool) {
        thread_pool_destroy(pool);
        pool = list_front(&threadpool_list);
    }
}

void thread_pool_handler_terminate(__attribute__((unused)) int sig, __attribute__((unused)) siginfo_t *siginfo,
                                   __attribute__((unused)) void *discard) {
    if (pthread_rwlock_wrlock(&no_defer_lock) != 0) syserr("pthread_mutex_lock error\n");
    no_defer = 1;
    if (pthread_rwlock_unlock(&no_defer_lock) != 0) syserr("pthread_mutex_unlock error\n");

    thread_pool_destroy_all();
    thread_pool_handler_destroy();

    signal(SIGINT, oldact.sa_handler);
    raise(SIGINT);
}

int get_no_defer() {
    if (pthread_rwlock_rdlock(&no_defer_lock) != 0) syserr("pthread_mutex_lock error\n");

    int8_t retval = no_defer;

    if (pthread_rwlock_unlock(&no_defer_lock) != 0) syserr("pthread_mutex_unlock error\n");

    return retval;
}

void thread_pool_work(void *data) {
    if (pthread_sigmask(SIG_BLOCK, &block_mask, 0) != 0) syserr("pthread_sigmask error/n");
    thread_pool_t *pool = (thread_pool_t *) data;

    if (pthread_mutex_lock(&pool->lock) != 0) syserr("pthread_mutex_lock error\n");

    while (!(pool->terminate || get_no_defer()) || !list_is_empty(&pool->task_queue)) {
        while (!(pool->terminate || get_no_defer()) && list_is_empty(&pool->task_queue)) {
            if (pthread_cond_wait(&pool->idle, &pool->lock) != 0) syserr("pthread_cond_wait error\n");
        }
        runnable_t *runnable = (runnable_t *) list_pop_front(&pool->task_queue);
        if (pthread_mutex_unlock(&pool->lock) != 0) syserr("pthread_mutex_unlock error\n");

        if (runnable) {
            runnable->function(runnable->arg, runnable->argsz);
            free(runnable);
        }

        if (pthread_mutex_lock(&pool->lock) != 0) syserr("pthread_mutex_lock error\n");
    }

    if (pthread_mutex_unlock(&pool->lock) != 0) syserr("pthread_mutex_unlock error\n");
}

int thread_pool_init(thread_pool_t *pool, size_t num_threads) {
    if (pthread_mutex_init(&pool->lock, 0) != 0) syserr("pthread_mutex_init error\n");
    if (pthread_cond_init(&pool->idle, 0) != 0) syserr("pthread_cond_init error\n");
    pool->terminate = 0;
    pool->num_threads = num_threads;

    pool->threads = malloc(sizeof(pthread_t) * num_threads);
    if (!pool->threads) return -1;

    list_init(&pool->task_queue);

    for (size_t i = 0; i < num_threads; i++) {
        if (pthread_create(&pool->threads[i], 0, (void *) thread_pool_work, pool) != 0)
            syserr("pthread_create error\n");
    }

    if (list_push_back(&threadpool_list, pool) != 0) {
        thread_pool_destroy(pool);
        return -1;
    }

    return 0;
}

void thread_pool_stop(thread_pool_t *pool) {
    if (pthread_mutex_lock(&pool->lock) != 0) syserr("pthread_mutex_lock error\n");

    pool->terminate = 1;
    if (pthread_cond_broadcast(&pool->idle) != 0) syserr("pthread_cond_broadcast error\n");

    if (pthread_mutex_unlock(&pool->lock) != 0) syserr("pthread_mutex_unlock error\n");
}

void thread_pool_destroy(thread_pool_t *pool) {
    thread_pool_stop(pool);
    list_erase(&threadpool_list, pool);

    for (size_t i = 0; i < pool->num_threads; i++) {
        if (pthread_join(pool->threads[i], 0) != 0) syserr("pthread_join error\n");
    }

    if (pthread_cond_destroy(&pool->idle) != 0) syserr("pthread_cond_destroy error\n");
    if (pthread_mutex_destroy(&pool->lock) != 0) syserr("pthread_mutex_destroy error\n");

    list_destroy(&pool->task_queue);
    free(pool->threads);
}

int thread_pool_terminated(thread_pool_t *pool) {
    if (pthread_mutex_lock(&pool->lock) != 0) syserr("pthread_mutex_lock error\n");

    int terminated = pool->terminate;

    if (pthread_mutex_unlock(&pool->lock) != 0) syserr("pthread_mutex_unlock error\n");

    return terminated;
}

int defer(struct thread_pool *pool, runnable_t runnable) {
    if (get_no_defer() || thread_pool_terminated(pool)) return -1;

    runnable_t *task = malloc(sizeof(runnable_t));
    if (!task) return -1;

    task->function = runnable.function;
    task->arg = runnable.arg;
    task->argsz = runnable.argsz;

    if (list_push_back(&pool->task_queue, (void *) task) != 0) {
        free(task);
        return -1;
    }
    if (pthread_cond_signal(&pool->idle) != 0) syserr("pthread_cond_signal error\n");

    return 0;
}
