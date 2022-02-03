#include "process.h"
#include "input.h"
#include "structure.h"
#include "equivalence.h"

#include <stdlib.h>

static ProcessResult initProcessResult() {
    ProcessResult processResult;
    processResult.energy = 0;

    return processResult;
}

static ProcessResult processIgnore() {
    ProcessResult processResult = initProcessResult();
    processResult.type = RESULT_IGNORE;

    return processResult;
}

static ProcessResult processError() {
    ProcessResult processResult = initProcessResult();
    processResult.type = RESULT_ERROR;

    return processResult;
}

static ProcessResult processDeclareCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();

    createTriePath(root, command->historyA, command->historySizeA);
    processResult.type = RESULT_OK;

    return processResult;
}

static ProcessResult processRemoveCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();

    removeTriePath(root, command->historyA, command->historySizeA);
    processResult.type = RESULT_OK;

    return processResult;
}

static ProcessResult processValidCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();

    if (findTrieNode(root, command->historyA, command->historySizeA) != NULL) {
        processResult.type = RESULT_YES;
    } else {
        processResult.type = RESULT_NO;
    }

    return processResult;
}

static ProcessResult processEnergySetCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();
    TrieNode *node = findTrieNode(root, command->historyA, command->historySizeA);

    if (node != NULL) {
        if (hasEquivalenceClass(node)) {
            EquivalenceClass *curr_class = findRoot(node->equivalenceClass);
            curr_class->energy = command->energy;
        } else {
            node->equivalenceClass = newEquivalenceClass(command->energy);
        }
        processResult.type = RESULT_OK;
    } else {
        processResult = processError();
    }

    return processResult;
}

static ProcessResult processEnergyPrintCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();
    TrieNode *node = findTrieNode(root, command->historyA, command->historySizeA);

    if (node != NULL && hasEquivalenceClass(node)) {
        processResult.type = RESULT_ENERGY;
        processResult.energy = getEnergy(node->equivalenceClass);
        if (processResult.energy == 0) {
            processResult = processError();
        }
    } else {
        processResult = processError();
    }

    return processResult;
}


static ProcessResult processEqualCommand(Command *command, TrieNode *root) {
    ProcessResult processResult = initProcessResult();
    TrieNode *nodeA = findTrieNode(root, command->historyA, command->historySizeA);
    TrieNode *nodeB = findTrieNode(root, command->historyB, command->historySizeB);
    EquivalenceClass *equivalenceClassA, *equivalenceClassB;

    if (nodeA != NULL && nodeA == nodeB) {
        processResult.type = RESULT_OK;
    } else if (nodeA != NULL && nodeB != NULL
    && (hasEquivalenceClass(nodeA) || hasEquivalenceClass(nodeB))) {
        if (hasEquivalenceClass(nodeA) && hasEquivalenceClass(nodeB)) {
            equivalenceClassA = findRoot(nodeA->equivalenceClass);
            equivalenceClassB = findRoot(nodeB->equivalenceClass);
            unionEquivalenceClasses(equivalenceClassA, equivalenceClassB);
        } else if (hasEquivalenceClass(nodeA) && !hasEquivalenceClass(nodeB)) {
            equivalenceClassA = findRoot(nodeA->equivalenceClass);
            appendEquivalenceClass(nodeB, equivalenceClassA);
        } else {
            equivalenceClassB = findRoot(nodeB->equivalenceClass);
            appendEquivalenceClass(nodeA, equivalenceClassB);
        }
        processResult.type = RESULT_OK;
    } else {
        processResult = processError();
    }

    return processResult;
}



ProcessResult processCommand(Command *command, ParseResult parseResult, TrieNode *root) {
    ProcessResult processResult;

    if (parseResult == PARSE_OK) {
        if (command->type == DECLARE) {
            processResult = processDeclareCommand(command, root);
        } else if (command->type == REMOVE) {
            processResult = processRemoveCommand(command, root);
        } else if (command->type == VALID) {
            processResult = processValidCommand(command, root);
        } else if (command->type == ENERGY_SET) {
            processResult = processEnergySetCommand(command, root);
        } else if (command->type == ENERGY_PRINT) {
            processResult = processEnergyPrintCommand(command, root);
        } else if (command->type == EQUAL){
            processResult = processEqualCommand(command, root);
        }
    } else if (parseResult == PARSE_IGNORE || parseResult == PARSE_EOF) {
        processResult = processIgnore();
    } else {
        processResult = processError();
    }

    return processResult;
}