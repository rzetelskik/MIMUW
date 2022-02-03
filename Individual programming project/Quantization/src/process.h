#ifndef QUANTIZATION_PROCESS_H
#define QUANTIZATION_PROCESS_H

#include "input.h"
#include "structure.h"
#include <stdint.h>

// Enum used to distinguish different output forms.
typedef enum {
    RESULT_OK,
    RESULT_YES,
    RESULT_NO,
    RESULT_ENERGY,
    RESULT_IGNORE,
    RESULT_ERROR
} ProcessResultType;

// Structure contains information necessary for printing the output.
typedef struct processResult {
    ProcessResultType type;
    uint64_t energy;
} ProcessResult;

ProcessResult processCommand(Command *command, ParseResult parseResult, TrieNode *root);

#endif //QUANTIZATION_PROCESS_H
