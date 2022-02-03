#ifndef QUANTIZATION_TRIE_H
#define QUANTIZATION_TRIE_H

#include "equivalence.h"
#include <stdint.h>
#include <stdbool.h>

#define CHILDREN_SIZE 4

/* Trie data structure is used to store declared histories and
 * search through them. Each node represents an ending of a history,
 * hence it can point to four of its children nodes. If the history
 * has an energy assigned, the ending node points to a corresponding
 * equivalence class.
 */

typedef struct trieNode {
    EquivalenceClass *equivalenceClass;
    struct trieNode *children[CHILDREN_SIZE];
} TrieNode;

TrieNode *newTrieNode();

TrieNode *findTrieNode(TrieNode *root, char *string, size_t stringSize);

// Append a history and all its prefixes to the structure.
void createTriePath(TrieNode *root, char *string, size_t stringSize);

/* Instead of a recursive approach, node removal uses an iterative one
 * with a proprietary stack implementation.
 */
void removeTrieNode(TrieNode *node);

void removeTriePath(TrieNode *root, char *string, size_t stringSize);

bool hasEquivalenceClass(TrieNode *node);

// Add a pointer to the corresponding find-union subset and increase its counter.
void appendEquivalenceClass(TrieNode *node, EquivalenceClass *equivalenceClass);

#endif //QUANTIZATION_TRIE_H
