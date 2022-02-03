#include "output.h"
#include "process.h"
#include <stdio.h>

void printOutput(ProcessResult processResult) {
    if (processResult.type == RESULT_OK) {
        fprintf(stdout, "OK\n");
    } else if (processResult.type == RESULT_YES) {
        fprintf(stdout, "YES\n");
    } else if (processResult.type == RESULT_NO) {
        fprintf(stdout, "NO\n");
    } else if (processResult.type == RESULT_ENERGY) {
        fprintf(stdout, "%lu\n", processResult.energy);
    } else if (processResult.type == RESULT_ERROR) {
        fprintf(stderr, "ERROR\n");
    } // Else do nothing.
}