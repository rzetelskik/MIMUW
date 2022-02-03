#include "list.h"
#include "err.h"
#include <stdlib.h>

typedef struct list_node {
    struct list_node *prev;
    struct list_node *next;
    void *elem;
} list_node_t;

void list_init(list_t *list) {
    if (pthread_mutex_init(&list->lock, 0) != 0) syserr("pthread_mutex_init error\n");

    list->head = list->tail = NULL;
}

void list_node_init(list_node_t *list_node, void *elem) {
    list_node->elem = elem;
    list_node->next = list_node->prev = NULL;
}

int list_push_back(list_t *list, void *elem) {
    list_node_t *new_node = malloc(sizeof(list_node_t));
    if (!new_node) return -1;

    list_node_init(new_node, elem);

    if (pthread_mutex_lock(&list->lock) != 0) syserr("pthread_mutex_lock error\n");

    if (list->head) {
        list->tail->next = new_node;
        new_node->prev = list->tail;
    } else {
        list->head = new_node;
    }
    list->tail = new_node;

    if (pthread_mutex_unlock(&list->lock) != 0) syserr("pthread_mutex_unlock error\n");

    return 0;
}

void *list_pop_front(list_t *list) {
    void *retval = NULL;

    if (pthread_mutex_lock(&list->lock) != 0) syserr("pthread_mutex_lock error\n");
    list_node_t *node = list->head;

    if (node) {
        retval = node->elem;
        if (node->next) {
            list->head = node->next;
            list->head->prev = NULL;
        } else {
            list->head = list->tail = NULL;
        }
    }

    if (pthread_mutex_unlock(&list->lock) != 0) syserr("pthread_mutex_unlock error\n");
    free(node);

    return retval;
}

void *list_front(list_t *list) {
    if (pthread_mutex_lock(&list->lock) != 0) syserr("pthread_mutex_lock error\n");

    void *retval = (list->head) ? list->head->elem : NULL;

    if (pthread_mutex_unlock(&list->lock) != 0) syserr("pthread_mutex_unlock error\n");

    return retval;
}

int list_erase(list_t *list, void *elem) {
    if (pthread_mutex_lock(&list->lock) != 0) syserr("pthread_mutex_lock error\n");
    list_node_t *node = list->head;

    while (node && node->elem != elem) {
        node = node->next;
    }

    if (node && node->prev) {
        node->prev->next = node->next;
    }
    if (node && node->next) {
        node->next->prev = node->prev;
    }
    if (node && node == list->head) {
        list->head = node->next;
    }
    if (node && node == list->tail) {
        list->tail = node->prev;
    }
    if (pthread_mutex_unlock(&list->lock) != 0) syserr("pthread_mutex_unlock error\n");

    if (!node) return -1;

    free(node);
    return 0;
}

int list_is_empty(list_t *list) {
    if (pthread_mutex_lock(&list->lock) != 0) syserr("pthread_mutex_lock error\n");
    int is_empty = !list->head;
    if (pthread_mutex_unlock(&list->lock) != 0) syserr("pthread_mutex_unlock error\n");

    return is_empty;
}

void list_destroy(list_t *list) {
    if (pthread_mutex_destroy(&list->lock) != 0) syserr("pthread_mutex_destroy error\n");
}