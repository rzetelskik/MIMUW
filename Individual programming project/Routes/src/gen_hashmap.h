/** @file
 * Generic hashMap with linked list in buckets
 * class inteface.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */

#ifndef GEN_HASHMAP_H
#define GEN_HASHMAP_H

#include "gen_list.h"

///Type used for hashing functions.
typedef unsigned int (*hashFunc)(void *data);

///Type used for key comparing functions.
typedef bool (*compareFunc)(void *e1, void *e2);

/**
 * Generic hashMap entry structure.
 */
typedef struct HashMapEntry {
    ///Pointer to a value to be stored in an entry.
    void *value;
    ///Pointer to a key assigned to it.
    void *key;
} HashMapEntry;

/**
 * Generic hashMap bucket structure used for storing the entries.
 */
typedef struct HashMapBucket {
    ///Generic list.
    List entries;
} HashMapBucket;

/**
 * Generic hashMap structure.
 */
typedef struct HashMap {
    ///Array of pointers to hashMap buckets.
    HashMapBucket *buckets;
    ///Size of the array.
    int size;
    ///Hashing function.
    hashFunc hashFunc;
    ///Function comparing the entries' keys.
    compareFunc compareFunc;
} HashMap;

/** @brief Initialises a new generic hashMap.
 * @param[in,out] hashMap - pointer to a hashMap.
 * @param[in] size - size of the array to be initialised.
 * @param[in] hashFunc - pointer to a hashing function.
 * @param[in] compareFunc - pointer to a function comparing the keys.
 * @param[in] freeFunc - pointer to a function freeing data from memory.
 */
bool initHashmap(HashMap *hashMap, int size, hashFunc hashFunc, compareFunc compareFunc, freeFunc freeFunc);

/**@brief Inserts a pointer to value to a hashMap using a key.
 * @param[in,out] hashMap - pointer to hashMap.
 * @param[in] key - key associated to a value.
 * @param[in] value - pointer to value to be inserted
 * @return @p true if data was inserted successfully, @p false otherwise.
 */
bool hashmapInsert(HashMap *hashMap, void *key, void *value);

/**@brief Gets a value from a hashMap using a key.
 * @param[in,out] hashMap - pointer to hashMap.
 * @param[in] key - key associated to a value.
 * @return Pointer to the value if found, NULL otherwise.
 */
void *hashmapGet(HashMap *hashMap, void *key);

/**@brief Clears the contents of the hashMap.
 * @param[in, out] hashMap - pointer to hashMap.
 */
void clearHashMap(HashMap *hashMap);

#endif //GEN_HASHMAP_H
