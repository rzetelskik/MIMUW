#define _GNU_SOURCE

#include "input.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define EOL '\n'
#define HASH '#'
#define SEMICOLON ';'
#define MINUS '-'
#define NULL_CHAR '\0'

static RoadSet *newRoadSet(char *origin, char *destination, unsigned length, int year) {
    RoadSet *roadSet = malloc(sizeof(RoadSet));
    if (roadSet != NULL) {
        roadSet->origin = origin;
        roadSet->destination = destination;
        roadSet->length = length;
        roadSet->year = year;
    }
    return roadSet;
}

static void initRoadSetList(RoadSetList *roadSetList) {
    if (roadSetList != NULL) {
        initList(&roadSetList->list, &free);
    }
}

static bool roadSetListAppend(RoadSetList *roadSetList, RoadSet *roadSet) {
    return listAppend(&roadSetList->list, roadSet);
}

static void clearRoadSetList(RoadSetList *roadSetList) {
    freeList(&roadSetList->list, true, false);
}

void initCommand(Command *command) {
    command->lineptr = NULL;
    command->city1 = NULL;
    command->city2 = NULL;
    command->length = 0;
    command->year = 0;
    command->id = 0;
    initRoadSetList(&command->roadSetList);
}

void clearCommand(Command *command) {
    if (command->lineptr != NULL) {
        free(command->lineptr);
    }
    clearRoadSetList(&command->roadSetList);
    initCommand(command);
}

static int getFirstChar(char *string) {
    if (string != NULL) {
        return string[0];
    }
    return 0;
}

static bool isDigit(int c) {
    return (c >= '0' && c <= '9');
}

static int charToDigit(int c) {
    return c - '0';
}

static bool canAppendUnsigned(unsigned num, int c) {
    return (num <= (UINT_MAX - c)/10);
}

static bool stringToUnsigned(char *string, unsigned *x) {
    size_t size = strlen(string), i = 0;
    bool first = true;
    int c;

    while (i < size) {
        c = string[i];
        if (isDigit(c) && canAppendUnsigned(*x, charToDigit(c))) {
            if (first) {
                *x = charToDigit(c);
                first = false;
            } else {
                *x = (*x)*10 + charToDigit(c);
            }
        } else {
            return false;
        }
        i++;
    }
    return (!first);
}

static bool canAppendInt(int num, int c, bool negative) {
    return ((unsigned)num <= (unsigned)(INT_MAX - c + (negative ? 1 : 0))/10);
}

static bool stringToInteger(char *string, int *x) {
    size_t size = strlen(string), i = 0;
    bool first = true, negative = false;
    int c;

    if (string[i] == MINUS) {
        negative = true;
        i++;
    }
    while (i < size) {
        c = string[i];
        if (isDigit(c) && canAppendInt(*x, charToDigit(c), negative)) {
            if (first) {
                *x = charToDigit(c);
                first = false;
            } else {
                *x = (*x)*10 + charToDigit(c);
            }
        } else {
            return false;
        }
        i++;
    }
    if (negative) {
        *x *= -1;
    }
    return (!first);
}

static bool getInputSection(char **section, char **line, size_t *size) {
    if (*size > 0) {
        *section = *line;
        size_t i = 0;

        while (i < *size && (*line)[i] != EOL && (*line)[i] != SEMICOLON) {
            if((*line)[i] == NULL_CHAR) {
                return false;
            }
            i++;
        }

        if (i > 0 && ((*line)[i] == SEMICOLON || (*line)[i] == EOL)) {
            (*line)[i] = '\0';
            *line = *line + i + 1;
            *size -= i + 1;
            return true;
        }

    }
    return false;
}

static bool getCommandType(Command *command, char **line, size_t *size) {
    char *section = NULL;
    unsigned id = 0;

    getInputSection(&section, line, size);
    if (strcmp(section, "addRoad") == 0) {
        command->type = ADD_ROAD;
        return true;
    }
    if (strcmp(section, "repairRoad") == 0) {
        command->type = REPAIR_ROAD;
        return true;
    }
    if (strcmp(section, "newRoute") == 0) {
        command->type = NEW_ROUTE;
        return true;
    }
    if (strcmp(section, "extendRoute") == 0) {
        command->type = EXTEND_ROUTE;
        return true;
    }
    if (strcmp(section, "removeRoad") == 0) {
        command->type = REMOVE_ROAD;
        return true;
    }
    if (strcmp(section, "removeRoute") == 0) {
        command->type = REMOVE_ROUTE;
        return true;
    }
    if (strcmp(section, "getRouteDescription") == 0) {
        command->type = GET_ROUTE_DESCRIPTION;
        return true;
    }
    if (stringToUnsigned(section, &id)) {
        command->type = CREATE_ROUTE;
        command->id = id;
        return true;
    }
    return false;
}

static bool getCommandCreateRoute(Command *command, char **line, size_t *size, bool *memOk) {
    char *origin, *destination, *tmp;

    bool correct = (getInputSection(&destination, line, size) && *size > 0);
    while (correct && *memOk && *size > 0) {
        unsigned length = 0;
        int year = 0;
        origin = destination;
        correct = (getInputSection(&tmp, line, size) && stringToUnsigned(tmp, &length) &&
                   getInputSection(&tmp, line, size) && stringToInteger(tmp, &year) &&
                   getInputSection(&destination, line, size));
        if (correct) {
            RoadSet *roadSet = newRoadSet(origin, destination, length, year);
            if (roadSet != NULL) {
                *memOk = roadSetListAppend(&command->roadSetList, roadSet);
                if (!*memOk) {
                    free(roadSet);
                }
            } else {
                *memOk = false;
            }
        }
    }

    return (correct && *memOk && *size == 0);
}

static bool getCommandAddRoad(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&command->city1, line, size) &&
                    getInputSection(&command->city2, line, size) &&
                    getInputSection(&section, line, size) && stringToUnsigned(section, &command->length) &&
                    getInputSection(&section, line, size) && stringToInteger(section, &command->year));

    return (correct && *size == 0);
}

static bool getCommandRepairRoad(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&command->city1, line, size) &&
                    getInputSection(&command->city2, line, size) &&
                    getInputSection(&section, line, size) && stringToInteger(section, &command->year));

    return (correct && *size == 0);
}

static bool getCommandNewRoute(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&section, line, size) && stringToUnsigned(section, &command->id) &&
            getInputSection(&command->city1, line, size) && getInputSection(&command->city2, line, size));

    return (correct && *size == 0);
}

static bool getCommandExtendRoute(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&section, line, size) && stringToUnsigned(section, &command->id) &&
            getInputSection(&command->city1, line, size));

    return (correct && *size == 0);
}

static bool getCommandRemoveRoad(Command *command, char **line, size_t *size) {

    bool correct = (getInputSection(&command->city1, line, size) && getInputSection(&command->city2, line, size));

    return (correct && *size == 0);
}

static bool getCommandRemoveRoute(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&section, line, size) && stringToUnsigned(section, &command->id));

    return (correct && *size == 0);
}

static bool getCommandGetRouteDescription(Command *command, char **line, size_t *size) {
    char *section;

    bool correct = (getInputSection(&section, line, size) && stringToUnsigned(section, &command->id));

    return (correct && *size == 0);
}

static bool getCommand(Command *command, char **line, size_t *size, bool *memOk) {
    if (getCommandType(command, line, size)) {
        switch (command->type) {
            case CREATE_ROUTE:
                return getCommandCreateRoute(command, line, size, memOk);
            case ADD_ROAD:
                return getCommandAddRoad(command, line, size);
            case REPAIR_ROAD:
                return getCommandRepairRoad(command, line, size);
            case NEW_ROUTE:
                return getCommandNewRoute(command, line, size);
            case EXTEND_ROUTE:
                return getCommandExtendRoute(command, line, size);
            case REMOVE_ROAD:
                return getCommandRemoveRoad(command, line, size);
            case REMOVE_ROUTE:
                return getCommandRemoveRoute(command, line, size);
            case GET_ROUTE_DESCRIPTION:
                return getCommandGetRouteDescription(command, line, size);
        }
    }

    return false;
}

ParseResult getInputLine(Command *command) {
    bool memOk = true;
    char *line = NULL;
    size_t length = 0, size;

    ssize_t result = getline(&line, &length, stdin);
    command->lineptr = line;
    size = result;

    if (line == NULL) {
        return MEMORY_FAIL;
    } else {
        if (result != -1) {
            int firstChar = getFirstChar(line);

            if (firstChar == HASH || firstChar == EOL) {
                return PARSE_IGNORE;
            }
            if (getCommand(command, &line, &size, &memOk)) {
                return PARSE_OK;
            }
            if (!memOk) {
                return MEMORY_FAIL;
            }
        } else {
            return PARSE_EOF;
        }
        return PARSE_ERROR;
    }

}