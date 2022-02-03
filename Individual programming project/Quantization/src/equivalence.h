#ifndef QUANTIZATION_EQUIVALENCE_H
#define QUANTIZATION_EQUIVALENCE_H

#include <stdint.h>
#include <stdlib.h>

/* Equivalence classes are designed using a disjoint-set
 * data structure with path compression. Every node
 * keeps a record of how many other elements
 * and Trie nodes point to it. It only gets removed when
 * no other element (including itself) points to it.
 * Path compression is applied whenever a node is being searched for.
 */

typedef struct equivalenceClass {
    size_t size;
    struct equivalenceClass *parent;
    uint64_t energy;
} EquivalenceClass;

EquivalenceClass *newEquivalenceClass(uint64_t energy);

//Remove current node and subtract one from parent node's counter.
void subtractFromEquivalenceClass(EquivalenceClass *node);

void expandEquivalenceClass(EquivalenceClass *node);

//Combine subsets by attaching one's root to the other by size (smaller to greater).
void unionEquivalenceClasses(EquivalenceClass *nodeA, EquivalenceClass *nodeB);

//Iterate through nodes until the top node. Compress path in the process.
EquivalenceClass *findRoot(EquivalenceClass *node);

//Get energy value from the top node in path.
uint64_t getEnergy(EquivalenceClass *node);

#endif //QUANTIZATION_EQUIVALENCE_H
