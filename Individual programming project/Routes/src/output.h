/** @file
 * Output handling class interface.
 * @author Kacper Rzetelski
 * @date 18.05.2019
 */

#ifndef OUTPUT_H
#define OUTPUT_H

#include "process.h"

/**@brief Prints the output.
 * Prints the output accordingly with the data provided in
 * ProcessResult structure. For RESULT_ERROR prints ERROR and input
 * line number, for RESULT_DESCRIPTION prints the description of a route
 * and for RESULT_OK does not perform any actions.
 * @param[in] processResult - data processing result structure.
 * @param[in] counter - input line number.
 */
void printOutput(ProcessResult processResult, int counter);

#endif //OUTPUT_H
