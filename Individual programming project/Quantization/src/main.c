#include <stdio.h>
#include "input.h"
#include "structure.h"
#include "process.h"
#include "output.h"


int main(void) {
    Command command = initCommand();
    ParseResult parseResult;
    ProcessResult processResult;
    TrieNode *root = newTrieNode();

    do {
        parseResult = getInputLine(&command);
        processResult = processCommand(&command, parseResult, root);
        printOutput(processResult);
        clearCommand(&command);
    } while (!(parseResult == PARSE_EOF || parseResult == PARSE_ERROR_EOF));
    removeTrieNode(root);

    return 0;
}