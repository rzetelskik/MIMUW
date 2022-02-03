#include "structure.h"
#include "equivalence.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define ARRAY_REALLOCATION_CONSTANT 2

// Stack used to keep the Trie nodes during the removal process.
typedef struct trieNodeStack {
    TrieNode **array;
    size_t realSize;
    size_t length;
} TrieNodeStack;

static TrieNodeStack *newTrieNodeStack() {
    TrieNodeStack* new = malloc(sizeof(TrieNodeStack));
    if (new == NULL) exit(1);
    new->realSize = 0;
    new->length = 0;
    new->array = NULL;

    return new;
};

static void newArray(TrieNodeStack **stack) {
    (*stack)->realSize = 2;
    (*stack)->array = malloc((*stack)->realSize * sizeof(TrieNode*));
    if ((*stack)->array == NULL) exit(1);
}

static void expandArray(TrieNodeStack **stack) {
    (*stack)->realSize = ARRAY_REALLOCATION_CONSTANT * (*stack)->realSize;
    (*stack)->array = realloc((*stack)->array, (*stack)->realSize * sizeof(TrieNode*));
    if ((*stack)->array == NULL) exit(1);
}

static void pushToStack(TrieNodeStack **stack, TrieNode *node) {
    if ((*stack)->length == (*stack)->realSize) {
        if ((*stack)->array == NULL) {
            newArray(stack);
        } else {
            expandArray(stack);
        }
    }
    (*stack)->array[(*stack)->length] = node;
    (*stack)->length++;
}

static TrieNode *popFromStack(TrieNodeStack *stack) {
    TrieNode *node = stack->array[stack->length - 1];
    stack->length--;

    return node;
}

static bool isStackEmpty(TrieNodeStack *stack) {
    return (stack->length == 0);
}

static void freeStack(TrieNodeStack *stack) {
    free(stack->array);
    free(stack);
}

TrieNode *newTrieNode() {
    TrieNode *node = malloc(sizeof(TrieNode));
    if (node == NULL) exit(1);

    node->equivalenceClass = NULL;
    for (int i = 0; i < CHILDREN_SIZE; i++) {
        node->children[i] = NULL;
    }

    return node;
}

void createTriePath(TrieNode *root, char *string, size_t stringSize) {
    size_t i = 0, value;
    TrieNode *currNode = root;

    while (i < stringSize) {
        value = string[i];
        if (currNode->children[value] == NULL){
            currNode->children[value] = newTrieNode();
        }
        currNode = currNode->children[value];
        i++;
    }
}

TrieNode *findTrieNode(TrieNode *root, char *string, size_t stringSize) {
    size_t i = 0, value;
    TrieNode *currNode = root;

    while (i < stringSize && currNode != NULL) {
        value = string[i];
        currNode = currNode->children[value];
        i++;
    }

    return currNode;
}

// Push node and its children nodes to stack and remove them in LIFO order.
void removeTrieNode(TrieNode *node) {
    if (node != NULL) {
        TrieNodeStack *stack = newTrieNodeStack();
        pushToStack(&stack, node);

        while (!isStackEmpty(stack)) {
            node = popFromStack(stack);
            if (hasEquivalenceClass(node)) {
                subtractFromEquivalenceClass(node->equivalenceClass);
            }
            for (int i = 0; i < CHILDREN_SIZE; i++) {
                if (node->children[i] != NULL) {
                    pushToStack(&stack, node->children[i]);
                }
            }
            free(node);
        }
        freeStack(stack);
    }
}

void removeTriePath(TrieNode *root, char *string, size_t stringSize) {
    TrieNode *parentNode = findTrieNode(root, string, stringSize - 1);
    int value = string[stringSize - 1];

    if (parentNode != NULL) {
        removeTrieNode(parentNode->children[value]);
        parentNode->children[value] = NULL;
    }
}

bool hasEquivalenceClass(TrieNode *node) {
    return (node != NULL && node->equivalenceClass != NULL);
}

void appendEquivalenceClass(TrieNode *node, EquivalenceClass *equivalenceClass) {
    if (node != NULL && equivalenceClass != NULL) {
        expandEquivalenceClass(equivalenceClass);
        node->equivalenceClass = equivalenceClass;
    }
}
