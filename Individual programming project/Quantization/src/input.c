#include "input.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define ARRAY_REALLOCATION_CONSTANT 2
#define MAX_REQUEST_SIZE 8

// Enum used to distinguish all special characters.
typedef enum {
    DIGIT,
    SPACE,
    HASH,
    END_OF_LINE,
    END_OF_FILE,
    ERROR
} CharType;

static char *newArray(size_t size) {
    char *array = malloc(size * sizeof(char));
    if (array == NULL) exit(1);

    return array;
}

static void expandArray(size_t *size, char **array) {
    *size = *size * ARRAY_REALLOCATION_CONSTANT;
    *array = realloc(*array, *size * sizeof(char));
    if (*array == NULL) exit(1);
}

static void freeHistory(char **history) {
    if (*history != NULL) {
        free(*history);
        *history = NULL;
    }
}

void clearCommand(Command *command) {
    freeHistory(&command->historyA);
    freeHistory(&command->historyB);
    command->historySizeA = 0;
    command->historySizeB = 0;
    command->energy = 0;
}

Command initCommand() {
    Command command;
    command.historyA = NULL;
    command.historyB = NULL;
    clearCommand(&command);

    return command;
}

static bool isDigit(int c) {
    return (c >= '0' && c <= '9');
}

static int charToDigit(int c) {
    return c - '0';
}

static bool canAppendU64(uint64_t num, int c) {
    return (num <= (UINT64_MAX - c)/10);
}

static bool getInputRequest(char *request, int *c, int *first) {
    int i = 0;
    *c = getchar();
    *first = *c;

    while (!(*c == EOF || *c == '\n' || *c == ' ' || *c == '#')) {
        if (i >= MAX_REQUEST_SIZE || *c < 'A' || *c > 'Z') {
            return false;
        } else {
            request[i] = *c;
        }
        i++;
        *c = getchar();
    }

    while (i < MAX_REQUEST_SIZE) {
        request[i] = 0;
        i++;
    }

    return true;
}

static bool getInputHistory(char **history, int *c, size_t *historySize) {
    size_t i = 0, size = 2;
    *c = getchar();

    while (!(*c == EOF || *c == '\n' || *c == ' ')) {
        if (*c >= '0' && *c <= '3') {
            if (i == 0) {
                *history = newArray(size);
            } else if (i == size) {
                expandArray(&size, history);
            }
            (*history)[i] = charToDigit(*c);
        } else {
            return false;
        }
        i++;
        *c = getchar();
    }
    *historySize = i;

    // Return true if there was any correct input.
    return (i > 0);
}

static bool getInputEnergy(uint64_t *x, int *c) {
    *c = getchar();
    bool noInput = true;

    while (!(*c == EOF || *c == '\n' || *c == ' ')) {
        if (!isDigit(*c) || !canAppendU64(*x, charToDigit(*c))) {
                return false;
        } else {
            if (noInput && *c > '0') {
                *x = charToDigit(*c);
                noInput = false;
            } else {
                *x = (*x)*10 + charToDigit(*c);
            }
        }
        *c = getchar();
    }

    // Return true if there was any correct input.
    return !(noInput);
}

static CharType getCharType(int c) {
    if (isDigit(c)) {
        return DIGIT;
    } else if (c == ' ') {
        return SPACE;
    } else if (c == '#') {
        return HASH;
    } else if (c == EOF) {
        return END_OF_FILE;
    } else if (c == '\n') {
        return END_OF_LINE;
    } else {
        return ERROR;
    }
}

static CharType finishLine(int *c) {
    while (!(*c == '\n' || *c == EOF)) {
        *c = getchar();
    };

    if (*c == '\n') {
        return END_OF_LINE;
    } else {
        return END_OF_FILE;
    }
}

/* The following functions are divided according to the
 * needs of the initial request. Requests that require the same set
 * of arguments use the same function to determine if the input
 * was provided correctly.
 */

static bool isCorrectCommandDeclareRemoveValid(Command *command, int *c, CommandType type) {
    if (getInputHistory(&command->historyA, c, &command->historySizeA) && getCharType(*c) == END_OF_LINE) {
        command->type = type;
        return true;
    }

    return false;
}

static bool isCorrectCommandEnergy(Command *command, int *c) {
    if (getInputHistory(&command->historyA, c, &command->historySizeA)) {
        if (getCharType(*c) == END_OF_LINE) {
            command->type = ENERGY_PRINT;
            return true;
        } else if (getCharType(*c) == SPACE && getInputEnergy(&command->energy, c)
        && getCharType(*c) == END_OF_LINE) {
            command->type = ENERGY_SET;
            return true;
        }
    }

    return false;
}

static bool isCorrectCommandEqual(Command *command, int *c) {
    if (getInputHistory(&command->historyA, c, &command->historySizeA) && getCharType(*c) == SPACE
    && getInputHistory(&command->historyB, c, &command->historySizeB) && getCharType(*c) == END_OF_LINE) {
        command->type = EQUAL;
        return true;
    }

    return false;
}

// Gather commands and determine if they produce a valid command.
static bool isCorrectCommand(Command *command, char *request, int *c, int *first) {
    bool correct;

    if (getInputRequest(request, c, first) && getCharType(*c) == SPACE) {
        if (strcmp(request, "DECLARE") == 0) {
            correct = isCorrectCommandDeclareRemoveValid(command, c, DECLARE);
        } else if (strcmp(request, "REMOVE") == 0) {
            correct = isCorrectCommandDeclareRemoveValid(command, c, REMOVE);
        } else if (strcmp(request, "VALID") == 0) {
            correct = isCorrectCommandDeclareRemoveValid(command, c, VALID);
        } else if (strcmp(request, "ENERGY") == 0) {
            correct = isCorrectCommandEnergy(command, c);
        } else if (strcmp(request, "EQUAL") == 0) {
            correct = isCorrectCommandEqual(command, c);
        } else {
            correct = false;
        }
    } else {
        correct = false;
    }

    return correct;
}

//Get a line of input and return its status (used to determine the necessary action).
ParseResult getInputLine(Command *command) {
    int c = 0, first = 0;
    char request[MAX_REQUEST_SIZE] = {0};
    bool correct = isCorrectCommand(command, request, &c, &first);

    if (correct) {
        return PARSE_OK;
    } else if (getCharType(first) == END_OF_LINE || getCharType(first) == HASH) {
        if (finishLine(&c) == END_OF_FILE) {
            return PARSE_EOF;
        } else {
            return PARSE_IGNORE;
        }
    } else {
        if (finishLine(&c) == END_OF_FILE) {
            if (getCharType(first) != END_OF_FILE) {
                return PARSE_ERROR_EOF;
            } else {
                return PARSE_EOF;
            }
        } else {
            return PARSE_ERROR;
        }
    }
}