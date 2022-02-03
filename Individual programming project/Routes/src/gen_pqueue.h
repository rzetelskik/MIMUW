/** @file
 * Generic priority queue class based on linked list.
 * NOTE: priority is inversed! It means the lower the value,
 * the higher the actual priority is.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */


#ifndef GEN_PQUEUE_H
#define GEN_PQUEUE_H

#include <stdint.h>
#include "gen_list.h"

/**
 * A priority queue entry structure.
 */
typedef struct PQueueEntry {
    ///Priority of the entry.
    uint64_t priority;
    ///Pointer to the data.
    void *data;
} PQueueEntry;

/**
 * A priority queue structure.
 */
typedef struct PQueue {
    ///Generic list.
    List list;
} PQueue;

/**@brief Initialises a priority queue.
 * @param[in,out] pQueue - pointer to priority queue.
 */
void initPQueue(PQueue *pQueue);

/**@brief Pops the node with the highest priority.
 * @param[in ,out] pQueue - pointer to priority queue.
 * @return Pointer to queue if not empty, NULL otherwise.
 */
void* pQueuePop(PQueue *pQueue);

/**@brief Pushes the data to the queue.
 * @param[in,out] pQueue - pointer to queue.
 * @param[in] priority - priority value.
 * @param[in] data - pointer to data to be inserted.
 * @return @p true if inserted successfully, @p false otherwise.
 */
bool pQueuePush(PQueue *pQueue, uint64_t priority, void *data);

/**@brief Checks if the queue is empty.
 * @param[in] pQueue - pointer to priority queue.
 * @return @p true if empty, @p false otherwise or if pQueue is NULL.
 */
bool isPQueueEmpty(PQueue *pQueue);

/**@brief Clears the contents of the priority queue.
 * @param[in,out] pQueue - pointer to priority queue.
 */
void clearPQueue(PQueue *pQueue);

#endif //GEN_PQUEUE_H
