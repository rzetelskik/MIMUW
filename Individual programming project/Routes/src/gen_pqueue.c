#include "gen_pqueue.h"
#include <stdio.h>
#include "city.h"

PQueueEntry *newPQueueEntry(unsigned long priority, void *data) {
    PQueueEntry *pQueueEntry = malloc(sizeof(PQueueEntry));
    if (pQueueEntry != NULL) {
        pQueueEntry->data = data;
        pQueueEntry->priority = priority;
    }
    return pQueueEntry;
}

void freePQueueEntry(void *pQueueEntry) {
    if (pQueueEntry != NULL) {
        free(pQueueEntry);
    }
}

void initPQueue(PQueue *pQueue) {
    initList(&(pQueue->list), &freePQueueEntry);
}

void* pQueuePop(PQueue *pQueue) {
    if (pQueue != NULL) {
        if (pQueue->list.first != NULL) {
            PQueueEntry *entry = pQueue->list.first->data;
            removeFromList(&(pQueue->list), entry);
            void *tmp = entry->data;
            free(entry);

            return tmp;
        }
    }
    return NULL;
}

//Priority is inversed - lowest priority goes first.
bool pQueuePush(PQueue *pQueue, unsigned long priority, void *data) {
    if (pQueue != NULL) {
        PQueueEntry *entry = newPQueueEntry(priority, data);
        if (entry != NULL) {
            ListNode *curr = pQueue->list.first;
            bool memOK;

            if (curr == NULL || priority < ((PQueueEntry*)curr->data)->priority) {
                memOK = listPrepend(&(pQueue->list), entry);
            } else {
                while (curr != NULL && ((PQueueEntry*)curr->data)->priority <= priority) {
                    curr = curr->next;
                }
                if (curr != NULL) {
                    memOK = listInsertBefore(&pQueue->list, curr, entry);
                } else {
                    memOK = listAppend(&pQueue->list, entry);
                    }
                }
            if (memOK) {
                return true;
            } else {
                free(entry);
                return false;
            }
        }
    }
    return false;
}

bool isPQueueEmpty(PQueue *pQueue) {
    if (pQueue != NULL) {
        return isListEmpty(&(pQueue->list));
    }
    return false;
}

void clearPQueue(PQueue *pQueue) {
    if (pQueue != NULL) {
        freeList(&(pQueue->list), true, false);
    }
}

