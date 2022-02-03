#ifndef ASYNC_LIST_H
#define ASYNC_LIST_H

#include <pthread.h>

typedef struct list {
    pthread_mutex_t lock;
    struct list_node *head;
    struct list_node *tail;
} list_t;

void list_init(list_t *list);

int list_push_back(list_t *list, void *elem);

void *list_pop_front(list_t *list);

void *list_front(list_t *list);

int list_erase(list_t *list, void *elem);

int list_is_empty(list_t *list);

void list_destroy(list_t *list);


#endif //ASYNC_LIST_H
