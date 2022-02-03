#include "gen_list.h"
#include "gen_pqueue.h"

static ListNode *newListNode(void *data) {
    ListNode *node = malloc(sizeof(ListNode));

    if (node != NULL) {
        node->next = NULL;
        node->prev = NULL;
        node->data = data;
    }

    return node;
}

void initList(List *list, freeFunc freeFunc) {
    if (list != NULL) {
        list->first = NULL;
        list->last = NULL;
        list->freeFunc = freeFunc;
    }
}

bool isListEmpty(List *list) {
    if (list != NULL) {
        if (list->first != NULL) {
            return false;
        }
    }
    return true;
}

void freeList(List *list, bool freeData, bool freeList) {
    if (list != NULL) {
        ListNode *curr = list->first;
        ListNode *tmp;

        while (curr != NULL) {
            if (freeData) {
                list->freeFunc(curr->data);
            }
            tmp = curr->next;
            free(curr);
            curr = tmp;
        }
        if (freeList) {
            free(list);
        } else {
            list->first = NULL;
            list->last = NULL;
        }
    }
}

bool listPrepend(List *list, void *data) {
    if (list != NULL) {
        ListNode *node = newListNode(data);
        if (node != NULL) {
            if (list->first != NULL) {
                node->next = list->first;
                list->first->prev = node;
            } else {
                list->last = node;
            }
            list->first = node;
            return true;
        }
    }
    return false;
}

bool listAppend(List *list, void *data) {
    if (list != NULL) {
        ListNode *node = newListNode(data);
        if (node != NULL) {
            if (list->last != NULL) {
                node->prev = list->last;
                list->last->next = node;
            } else {
                list->first = node;
            }
            list->last = node;
            return true;
        }
    }
    return false;
}

bool listInsertBefore(List *list, ListNode *listNode, void *data) {
    if (list != NULL && listNode != NULL) {
        ListNode *newNode = newListNode(data);
        if (newNode != NULL) {
            newNode->next = listNode;
            if (listNode->prev != NULL) {
                newNode->prev = listNode->prev;
                listNode->prev->next = newNode;
            }
            listNode->prev = newNode;
            if (list->first == listNode) {
                list->first = newNode;
            }
            return true;
        }
    }
    return false;
}

bool listInsertAfter(List *list, ListNode *listNode, void *data) {
    if (list != NULL && listNode != NULL) {
        ListNode *newNode = newListNode(data);
        if (newNode != NULL) {
            newNode->prev = listNode;
            if (listNode->next != NULL) {
                newNode->next = listNode->next;
                listNode->next->prev = newNode;
            }
            listNode->next = newNode;
            if (list->last == listNode) {
                list->last = newNode;
            }
            return true;
        }
    }
    return false;
}

ListNode *findInList(List *list, void *data) {
    if (list != NULL) {
        ListNode *curr = list->first;
        while (curr != NULL && curr->data != data) {
            curr = curr->next;
        }
        if (curr != NULL && curr->data == data) {
            return curr;
        }
    }
    return NULL;
}

bool isInList(List *list, void *data) {
    return (findInList(list, data) != NULL);
}

void removeListNode(List *list, ListNode *node) {
    if (list != NULL && node != NULL) {
        if (node == list->first) {
            list->first = node->next;
        }
        if (node == list->last) {
            list->last = node->prev;
        }
        if (node->next != NULL) {
            node->next->prev = node->prev;
        }
        if (node->prev != NULL) {
            node->prev->next = node->next;
        }
        free(node);
    }
}

void removeFromList(List *list, void *data) {
    if (list != NULL) {
        ListNode *curr = list->first;
        while (curr != NULL && curr->data != data) {
            curr = curr->next;
        }
        if (curr != NULL && curr->data == data) {
            removeListNode(list, curr);
        }
    }
}

void* getFirst(List *list) {
    if (list != NULL) {
        return list->first->data;
    }
    return NULL;
}

void* getLast(List *list) {
    if (list != NULL) {
        return list->last->data;
    }
    return NULL;
}
