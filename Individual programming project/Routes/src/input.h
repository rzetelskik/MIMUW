/** @file
 * Input reading and handling class interface.
 * @author Kacper Rzetelski
 * @date 18.05.2019
 */

#ifndef INPUT_H
#define INPUT_H

#include "gen_list.h"

/**
 * Structure for storing the data necessary for a new road creation.
 */
typedef struct RoadSet {
    ///Name of the city of origin.
    char *origin;
    ///Name of the city of destination.
    char *destination;
    ///Length of the road.
    unsigned length;
    ///Road's year of build.
    int year;
} RoadSet;


/**
 * RoadSet list structure.
 */
typedef struct RoadSetList {
    ///Generic list.
    List list;
} RoadSetList;

/**
 * Enum used to determine the result of parsing the input.
 */
typedef enum {
    PARSE_OK,
    PARSE_ERROR,
    PARSE_IGNORE,
    PARSE_EOF,
    MEMORY_FAIL
} ParseResult;

/**
 * Enum used to determine what type of command has been provided.
 */
typedef enum {
    CREATE_ROUTE,
    ADD_ROAD,
    REPAIR_ROAD,
    NEW_ROUTE,
    EXTEND_ROUTE,
    REMOVE_ROAD,
    REMOVE_ROUTE,
    GET_ROUTE_DESCRIPTION
} CommandType;

/**
 * Structure for storing the data necessary for executing all types
 * of commands.
 */
typedef struct Command {
    ///Pointer to the array of characters read from one line of input.
    char *lineptr;
    ///Type of the command that has been provided.
    CommandType type;
    ///A list of RoadSet structures (used in route creation).
    RoadSetList roadSetList;
    ///Name of a city (used in add/repair road command).
    char *city1;
    ///Name of a city (used in add/repair road command).
    char *city2;
    ///Length of the road (used in add/repair road command).
    unsigned length;
    ///Road's year of build (used in add/repair road command).
    int year;
    ///Route's id number (used in route creation/description).
    unsigned id;
} Command;

/**@brief Initialises a command.
 * Initialises a command with default values.
 * @param[in, out] command - pointer to a command.
 */
void initCommand(Command *command);

/**@brief Clears the command.
 * Clears the command by resetting the attributes to default values
 * and freeing the memory that is no longer needed.
 * @param[in, out] command - pointer to a command.
 */
void clearCommand(Command *command);

/**@brief Gets a line from input.
 * Reads one line of input and parses it. CommandType is determined and Command
 * is filled with data accordingly.
 * @param[in, out] command - pointer to a command.
 * @return @p PARSE_OK if the line of input has been parsed correctly,
 * @p PARSE_IGNORE if the line is to be ignored, @p PARSE_EOF if the end of file has
 * been reached, @p MEMORY_FAIL if the memory could not have been allocated and
 * @p PARSE_ERROR if the line of input could not have been correctly parsed.
 */
ParseResult getInputLine(Command *command);

#endif //INPUT_H
