/** @file
 * Route class interface.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */
#ifndef ROUTE_H
#define ROUTE_H
///Predefined amount of routes allowed in a map.
#define ROUTE_ARRAY_SIZE 999

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "gen_list.h"

///City type (declared in city.h).
typedef struct City City;
///CityList type (declared in city.h).
typedef struct CityList CityList;

/**
 * Route structure.
 */
typedef struct Route {
    ///Boolean value determining if the route is active.
    bool active;
    ///Route's unique id number.
    unsigned id;
    ///Pointer to a list of cities that the route passes through.
    CityList *cities;
} Route;

/**
 * Route array structure.
 */
typedef struct RouteArray {
    ///Array of routes.
    Route array[ROUTE_ARRAY_SIZE];
} RouteArray;

/**
 * Route list structure using the generic list implementation.
 */
typedef struct RouteList {
    ///Generic list.
    List list;
} RouteList;

/**
 * Vertex structure used in graph.
 */
typedef struct Vertex {
    ///Distance already travelled to reach this vertex.
    uint64_t distance;
    ///The oldest road's year of build.
    int oldestRoad;
    ///Pointer to the previously visited city.
    City *prevCity;
    ///Boolean value determining if this vertex has already been visited.
    bool visited;
    ///Boolean value stating if a path to this vertex can be unambiguously determined.
    bool undetermined;
} Vertex;

/**
 * Optimal route finding result structure.
 */
typedef struct RouteResult {
    ///The total length of the route.
    uint64_t distance;
    ///The oldest road's year of build.
    int oldestRoad;
    ///Pointer to the list of cities that the route passes through.
    CityList *cityList;
    ///Boolean value determining if the route was found unambiguously.
    bool undetermined;
} RouteResult;

/**@brief Searches for a route in a route array.
 * @param[in] routeArray - pointer to route array.
 * @param[in] id - id of a route.
 * @return Pointer to route.
 */
Route *getRoute(RouteArray *routeArray, unsigned id);

/**@brief Checks if a route is active.
 * Looks for a route with a given id number and checks if
 * it is active (if it has already been declared).
 * @param[in] routeArray - pointer to an array of routes.
 * @param[in] id - id number of a route to be searched for.
 * @return @p true if the route is active, @p false if it is inactive.
 */
bool isRouteActive(RouteArray *routeArray, unsigned id);

/**@brief Clears the information about a route.
 * Deletes the information about a given route and marks it as inactive.
 * @param[in, out] routeArray - pointer to an array of routes.
 * @param[in] id - id number of a route to be cleared.
 */
void clearRoute(RouteArray *routeArray, unsigned id);

/**@brief Initialises route array.
 * Initialises route array with routes in default, inactive state.
 * @param[in, out] routeArray - pointer to a route array.
 */
void initRouteArray(RouteArray *routeArray);

/**@brief Clears the contents of a route array.
 * Cleares the contents of a route array and frees the memory
 * that is no longer needed.
 * @param[in, out] routeArray - pointer to a route array.
 */
void clearRouteArray(RouteArray *routeArray);

/**@brief Initialises a route list.
 * @param[in,out] routeList - pointer to a route list.
 */
void initRouteList(RouteList *routeList);

/**@brief Removes a route from a route list.
 * Searches for a route in a route list and if found - removes it.
 * @param[in, out] routeList - pointer to a route list.
 * @param[in] route - pointer to a route.
 */
void removeFromRouteList(RouteList *routeList, Route *route);

/**@brief Frees route list from memory.
 * @param[in,out] routeList - pointer to a route list.
 */
void freeRouteList(RouteList *routeList);

/**@brief Frees route result from memory.
 * @param[in,out] routeResult - pointer to a route result.
 */
void freeRouteResult(RouteResult *routeResult);

/**@brief Finds the most optimal route between the two cities.
 * Finds the most optimal route between the two cities. The route is considered
 * most optimal if it is the shortest. If there is more than one such route,
 * it finds the oldest road in the routes and chooses the one with
 * the newest one. If the oldest roads are equally old, it marks the result as
 * undefined.
 * @param[in] numVertices - total number of cities on a map.
 * @param[in] origin - city to start from.
 * @param[in] target - city to end at.
 * @param[in] thisRoute - route to be avoided while searching.
 * @return Pointer to optimal route result structure if the search was successful,
 * NULL if unsuccessful or memory could not be allocated.
 */
RouteResult *findBestRoute(int numVertices, City *origin, City *target, Route *thisRoute);

/**@brief Mark all the roads in a route as belonging to this route.
 * @param[in] route - pointer to a route.
 */
void markRouteRoads(Route *route);

/**@brief Unmarks all the roads in a route.
 * Iterates through all the segments of a given route and deletes the
 * information they contain about being a part of this route.
 * @param[in] route - pointer to a route.
 */
void unmarkRouteRoads(Route *route);

/**@brief Checks if the route goes through the selected city.
 * @param[in] route - pointer to a route.
 * @param[in] city - pointer to city.
 * @return @p true if the route goes through city, @p false otherwise.
 */
bool isCityInRoute(Route *route, City *city);

/**@brief Selects the best out of two route finding results.
 * Selects the best out of two route finding results using the same indicators
 * as in finding the best route.
 * @param[in] result1 - pointer to result.
 * @param[in] result2 - pointer to result.
 * @return Pointer to the best result of the two or NULL if was
 * not able to determine.
 */
RouteResult *pickResult(RouteResult *result1, RouteResult *result2);

/**@brief Concatenates a list of cities to an already existing route.
 * Concatenates the new cities to either the first or the last element of
 * a route so that they match.
 * @param[in] segment - pointer to city list.
 * @param[in,out] route - pointer to a route.
 * @return @p true if the concatenation was successful, @p false otherwise or if memory
 * could not be allocated.
 */
bool concatCityListToRoute(CityList *segment, Route *route);

/**@brief Inserts a city list into a route.
 * Inserts a city list into a route so that the ends match.
 * @param[in] segment - pointer to city list.
 * @param[in,out] route - pointer to a route.
 * @param[in] cityA - city to start inserting at.
 * @param[in] cityB - city to end inserting at.
 * @return @p true if insertion was successful, @p false if otherwise or
 * if memory could not be allocated.
 */
bool insertCityListIntoRoute(CityList *segment, Route *route, City *cityA, City *cityB);


#endif //ROUTE_H
