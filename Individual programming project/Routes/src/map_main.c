#include <stdio.h>
#include "input.h"
#include "map.h"
#include "process.h"
#include "output.h"

int main(void) {
    Command command;
    initCommand(&command);
    ParseResult parseResult;
    ProcessResult processResult;
    initProcessResult(&processResult);
    Map *map = newMap();
    int counter = 1;

    do {
        parseResult = getInputLine(&command);
        processResult = processCommand(&command, parseResult, map);
        printOutput(processResult, counter);
        clearCommand(&command);
        clearProcessResult(&processResult);
        counter++;
    } while (!(parseResult == PARSE_EOF || parseResult == MEMORY_FAIL));

    deleteMap(map);

    return 0;
}
