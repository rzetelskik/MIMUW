/** @file
 * Data checking class.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */

#ifndef INPUTCHECK_H
#define INPUTCHECK_H

#include <stdbool.h>

///Route type (declared in route.h).
typedef struct Route Route;

/**@brief Checks if two strings are the same content-wise.
 * @param[in] city1 - pointer to character array.
 * @param[in] city2 - pointer to character array.
 * @return @p true if the same, @p false if not or any of the
 * parameters points to NULL.
 */
bool areNamesEqual(const char *city1, const char *city2);

/**@brief Checks if the name of the city is correct.
 * Checks if the name of the city does not contain semicolons,
 * characters with ASCII codes in the range of 0 to 31 and if its
 * length is at least 1.
 * @param[in] city - pointer to the name of the city.
 * @return @p true if correct, @p false otherwise.
 */
bool isNameCorrect(const char *city);

/**@brief Checks if the road's length is correct.
 * Checks if the road's length is bigger than zero.
 * @param[in] length - length of the road.
 * @return @p true if correct, @p false otherwise.
 */
bool isLengthCorrect(unsigned length);

/**@brief Checks if the year the road was built/repaired
 * in is correct.
 * Checks if the year is not equal to zero.
 * @param[in] year - build year.
 * @return @p true if correct, @p false otherwise.
 */
bool isYearCorrect(int year);

/**@brief Checks if route's id is correct.
 * Checks if the route's id is in the range of 1 to 999.
 * @param[in] id - id number.
 * @return @p true if correct, @p false otherwise.
 */
bool isRouteIdCorrect(unsigned id);

/**@brief Counts characters describing a route to be printed out.
 * @param[in] route - pointer to route.
 * @return number of characters.
 */
int countCharacters(Route *route);

#endif //// INPUTCHECK_H
