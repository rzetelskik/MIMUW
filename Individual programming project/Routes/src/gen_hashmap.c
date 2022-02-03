#include "gen_hashmap.h"

bool initHashmap(HashMap *hashmap, int length, hashFunc hashFunc,
        compareFunc compareFunc, freeFunc freeFunc) {

    if (hashmap != NULL) {
        hashmap->size = length;
        hashmap->hashFunc = hashFunc;
        hashmap->compareFunc = compareFunc;
        hashmap->buckets = malloc(length * sizeof(HashMapBucket));

        if (hashmap->buckets != NULL) {
            for (int i = 0; i < length; i++) {
                List *list = &(hashmap->buckets[i].entries);
                initList(list, freeFunc);
            }
            return true;
        }
    }
    return false;
}

bool hashmapInsert(HashMap *hashmap, void *key, void *value) {
    HashMapEntry *entry = malloc(sizeof(HashMapEntry));
    if (entry != NULL) {
        int index = hashmap->hashFunc(key);
        entry->key = key;
        entry->value = value;

        List *entries = &(hashmap->buckets[index].entries);
        return listAppend(entries, entry);
    }
    return false;
}

void *hashmapGet(HashMap *hashmap, void *key) {
    int index = hashmap->hashFunc(key);
    List *entries = &(hashmap->buckets[index].entries);

    if (entries->first != NULL) {
        ListNode *curr = entries->first;
        while(curr != NULL) {
            HashMapEntry *entry = (HashMapEntry*) curr->data;
            if (hashmap->compareFunc(entry->key, key)) {
                return entry->value;
            }
            curr = curr->next;
        }

    }
    return NULL;
}

void clearHashMap(HashMap *hashmap) {
    if (hashmap != NULL) {
        for (int i = 0; i < hashmap->size; i++) {
            List *curr = &(hashmap->buckets[i].entries);
            freeList(curr, true, false);
        }
        free(hashmap->buckets);
    }
}
