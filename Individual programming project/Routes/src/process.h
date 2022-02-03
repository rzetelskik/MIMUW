/** @file
 * Command processing class interface.
 *
 * @author Kacper Rzetelski
 * @date 18.05.2019
 */

#ifndef PROCESS_H
#define PROCESS_H

#include "input.h"
#include "map.h"

/**
 * Enum used to determine the result of
 * processing the provided command.
 */
typedef enum {
    RESULT_OK,
    RESULT_DESCRIPTION,
    RESULT_ERROR
} ProcessResultType;

/**
 * Structure for storing the processed data necessary
 * for providing the output.
 */
typedef struct ProcessResult {
    ///Type of result of processing the command.
    ProcessResultType type;
    ///Description to be printed out for GetRouteDescription command.
    char const* description;
} ProcessResult;

/**@brief Initialises a ProcessResult structure.
 * Initialises a ProcessResult structure with default values.
 * @param[in, out] processResult - pointer to processing result container.
 */
void initProcessResult(ProcessResult *processResult);

/**@brief Clears a ProcessResult structure.
 * Clears a ProcessResult structure by resetting the attributes to default values
 * and freeing the memory that is no longer needed.
 * @param[in, out] processResult - pointer to processing result container.
 */
void clearProcessResult(ProcessResult *processResult);

/**@brief Processes a provided command.
 * Processes a command accordingly to its type. Checks if all the command's
 * parameters are correct and if the request can be executed. If so, performs all the
 * actions necessary.
 * @param[in] command - pointer to a command.
 * @param[in, out] parseResult - pointer to processing result container.
 * @param[in, out] map - pointer to the map.
 * @return Returs the ProcessResult structure filled with the information needed for
 * further execution of the request (output).
 */
ProcessResult processCommand(Command *command, ParseResult parseResult, Map *map);

#endif //PROCESS_H
