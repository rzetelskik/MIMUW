#ifndef QUANTIZATION_PARSE_H
#define QUANTIZATION_PARSE_H

#include <stdint.h>
#include <stdlib.h>

typedef enum {
    PARSE_OK,
    PARSE_ERROR,
    PARSE_IGNORE,
    // ERROR_EOF requires "ERROR" output before termination.
    PARSE_ERROR_EOF,
    PARSE_EOF
} ParseResult;

typedef enum {
    DECLARE,
    REMOVE,
    VALID,
    ENERGY_SET,
    ENERGY_PRINT,
    EQUAL
} CommandType;

// Command structure contains its type and a set of arguments.
typedef struct command {
    CommandType type;
    char *historyA;
    char *historyB;
    size_t historySizeA;
    size_t historySizeB;
    uint64_t energy;
} Command;

Command initCommand();

ParseResult getInputLine(Command *command);

//Free histories and clear all the argument slots.
void clearCommand(Command *command);


#endif //QUANTIZATION_PARSE_H
