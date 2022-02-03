#include "equivalence.h"

#include <stdlib.h>
#include <stdbool.h>


EquivalenceClass *newEquivalenceClass(uint64_t energy) {
    EquivalenceClass *equivalenceClass = malloc(sizeof(EquivalenceClass));
    if (equivalenceClass == NULL) exit(1);

    equivalenceClass->size = 1;
    equivalenceClass->energy = energy;
    equivalenceClass->parent = equivalenceClass;

    return equivalenceClass;
}

static bool isEmpty(EquivalenceClass *node) {
    return (node != NULL && node->size == 0);
}

static void removeEquivalenceClass(EquivalenceClass *node) {
    if (node != NULL) {
        if (node != node->parent) {
            subtractFromEquivalenceClass(node->parent);
        }
        free(node);
    }
}

void subtractFromEquivalenceClass(EquivalenceClass *node) {
    if (node != NULL) {
        node->size--;
        if (isEmpty(node)) {
            removeEquivalenceClass(node);
        }
    }
}

void expandEquivalenceClass(EquivalenceClass *node) {
    if (node != NULL) {
        node->size++;
    }
}

EquivalenceClass *findRoot(EquivalenceClass *node) {
    if (node != NULL) {
        EquivalenceClass *prev_parent, *curr_parent;
        if (node->parent != node) {
            prev_parent = node->parent;
            curr_parent = findRoot(prev_parent);
            if (prev_parent != curr_parent) {
                subtractFromEquivalenceClass(prev_parent);
                expandEquivalenceClass(curr_parent);
                node->parent = curr_parent;
            }
        }
        if (isEmpty(node)) {
            removeEquivalenceClass(node);
        }
        return node->parent;
    } else {
        return NULL;
    }
}

static uint64_t energyAverage(uint64_t a, uint64_t b) {
    return a/2 + b/2 + (a%2) * (b%2);
}

void unionEquivalenceClasses(EquivalenceClass *nodeA, EquivalenceClass *nodeB) {
    EquivalenceClass *rootA = findRoot(nodeA), *rootB = findRoot(nodeB);
    EquivalenceClass *unionRoot = rootA;

    if (rootA != NULL && rootB != NULL && rootA != rootB) {
        if (rootA->size > rootB->size) {
            rootB->parent = rootA;
        } else {
            rootA->parent = rootB;
            unionRoot = rootB;
        }
        expandEquivalenceClass(unionRoot);
        unionRoot->energy = energyAverage(rootA->energy, rootB->energy);
    }
}

uint64_t getEnergy(EquivalenceClass *node) {
    uint64_t energy = 0;

    if (node != NULL) {
        energy = findRoot(node)->energy;
    }

    return energy;
}
