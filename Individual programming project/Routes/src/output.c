#include <stdio.h>
#include "output.h"

void printOutput(ProcessResult processResult, int counter) {
    switch (processResult.type) {
        case RESULT_ERROR:
            fprintf(stderr, "ERROR %d\n", counter);
            break;
        case RESULT_DESCRIPTION:
            fprintf(stdout, "%s\n", processResult.description);
            break;
        default:
            break;
    }
}