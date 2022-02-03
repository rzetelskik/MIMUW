/** @file
 * Generic linked list class interface.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */

#ifndef LIST_H
#define LIST_H

#include <stdlib.h>
#include <stdbool.h>

///Type used for memory freeing functions.
typedef void (*freeFunc)(void*);

/**
 * Generic linked list node structure.
 */
typedef struct ListNode {
    ///Pointer to the previous node.
    struct ListNode *prev;
    ///Pointer to the next node.
    struct ListNode *next;
    ///Pointer to the data stored in the node.
    void *data;
} ListNode;

/**
 * Generic linked list structure.
 */
typedef struct List {
    ///Pointer to the first element of the list.
    ListNode *first;
    ///Pointer to the last element of the list.
    ListNode *last;
    ///Function used for freeing the memory.
    freeFunc freeFunc;
} List;

/** @brief Initialises a generic list.
 * @param[in,out] list - pointer to a list.
 * @param[in] freeFunc - pointer to a function freeing data from memory.
 */
void initList(List *list, freeFunc freeFunc);

/** @brief Frees a new generic list.
 * @param[in,out] list - pointer to a list.
 * @param[in] freeData - boolean value determining if data should be freed.
 * @param[in] freeList - boolean value determining if the container should be freed.
 */
void freeList(List *list, bool freeData, bool freeList);

/** @brief Inserts data to a list before a selected list node.
 * @param[in,out] list - pointer to a list.
 * @param[in] listNode - pointer to a list node.
 * @param[in] data - pointer to data to be inserted.
 * @return @p true if data was inserted successfully.
 * @p false if memory could not be allocated, list node was not in the list.
 */
bool listInsertBefore(List *list, ListNode *listNode, void *data);

/** @brief Inserts data to a list after a selected list node.
 * @param[in,out] list - pointer to a list.
 * @param[in] listNode - pointer to a list node.
 * @param[in] data - data to be inserted.
 * @return @p true if data was inserted successfully.
 * @p false if memory could not be allocated, list node was not in the list.
 */
bool listInsertAfter(List *list, ListNode *listNode, void *data);

/** @brief Prepends data to a list.
 * @param[in,out] list - pointer to a list.
 * @param[in] data - pointer data to be inserted.
 * @return @p true if data was inserted successfully.
 */
bool listPrepend(List *list, void *data);

/** @brief Appends data to a list.
 * @param[in,out] list - pointer to a list.
 * @param[in] data - pointer data to be inserted.
 * @return @p true if data was inserted successfully.
 */
bool listAppend(List *list, void *data);

/** @brief Removes data from list.
 * @param[in,out] list - pointer to a list.
 * @param[in] data - pointer to data to be removed.
 */
void removeFromList(List *list, void *data);

/** @brief Checks if the list is empty.
 * @param[in] list - pointer to a list.
 * @return @p true if list is empty, @p false otherwise.
 */
bool isListEmpty(List *list);

/** @brief Searches for data in a list.
 * @param[in] list - pointer to a list.
 * @param[in] data - pointer to data.
 * @return Pointer to a list node if data was found, NULL otherwise.
 */
ListNode *findInList(List *list, void *data);

/** @brief Searches for data in a list.
 * @param[in] list - pointer to a list.
 * @param[in] data - pointer to data.
 * @return @p true if found, @p otherwise.
 */
bool isInList(List *list, void *data);

/**@brief Searches for the first element in a list.
 * @param[in] list - pointer to a list.
 * @return Pointer to data kept in the first node if the list was empty, NULL otherwise.
 */
void* getFirst(List *list);

/**@brief Searches for the last element in a list.
 * @param[in] list - pointer to a list.
 * @return Pointer to data kept in the first node if the list was empty, NULL otherwise.
 */
void* getLast(List *list);

#endif //LIST_H
