/** @file
 * Route map class interface.
 *
 * @author Łukasz Kamiński <kamis@mimuw.edu.pl>, Marcin Peczarski <marpe@mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 20.03.2019
 */

#ifndef __MAP_H__
#define __MAP_H__

#include "city.h"

/**
 * Route map structure.
 */
typedef struct Map {
    ///HashMap of created cities.
    CityHashMap cityHashmap;
    ///Array of routes.
    RouteArray routeArray;
} Map;

 /** @brief Creates a new structure.
  * Creates a new, empty structure that does not contain any cities, roads our routes.
  * @return Pointer to a newly created structure or NULL if the memory could not be
  * allocated.
  */
Map* newMap(void);

/** @brief Deletes structure.
 * Deletes the structure pointer by @p map.
 * Does nothing if the pointer is NULL.
 * @param[in] map - pointer to the structure to be deleted.
 */
void deleteMap(Map *map);

/** @brief Adds road between two different cities to the map.
 * If any of the cities does not exist, it is added to the map and then
 * a road between the cities is also added.
 * @param[in,out] map    – pointer to a route map;
 * @param[in] city1      – pointer to a city name;
 * @param[in] city2      – pointer to a city name;
 * @param[in] length     – road length in km;
 * @param[in] builtYear  – road's year of build;
 * @return Value @p true if the road has been added. Value @p false if
 * an error occurred: one of the parameters was incorrect, cities' names
 * are the same, the road already exists or the memory could not be allocated.
 */
bool addRoad(Map *map, const char *city1, const char *city2,
             unsigned length, int builtYear);

/** @brief Modifies the date of the road's last repair.
 * Changes the year of the last repair of the road between the two cities
 * or sets it if the road has not been repaired yet.
 * @param[in,out] map    – pointer to route map;
 * @param[in] city1      – pointer to a city name;
 * @param[in] city2      – pointer to a city name;
 * @param[in] repairYear – road's last repair year;
 * @return Value @p true if the modification was successful.
 * Value @p false if an error occurred: one of the parameters was incorrect,
 * there is no road between the selected cities, given year is prior to the one
 * already known for the selected road.
 */
bool repairRoad(Map *map, const char *city1, const char *city2, int repairYear);

/** @brief Connects two different cities with a route.
 * Creates a route between the two cities and gives it a selected number.
 * Find the shortest path among existing roads. If there is more than one
 * way to do so, for each option it looks for a road with the oldest year
 * of build/repair and then selects the newer one.
 * @param[in,out] map    – pointer to a route map;
 * @param[in] routeId    – route's id number;
 * @param[in] city1      – pointer to a city name;
 * @param[in] city2      – pointer to a city name;
 * @return Value @p true if the route has been created.
 * Value @p false if an error occurred: one of the parameters was incorrect,
 * a route with a given number already exists, one of the given cities does not
 * exist, both cities have the same name, route could not have been determined
 * unambiguously or the memory could not be allocated.
 */
bool newRoute(Map *map, unsigned routeId,
              const char *city1, const char *city2);

/** @brief Extends route to the given city.
 * Adds road sections leading to a city to a route in such a way that the
 * the new section is the shortest possible. If there is more than one
 * way to do so, for each option it looks for a road with the oldest year
 * of build/repair and then selects the newer one.
 * @param[in,out] map    – pointer to a route map;
 * @param[in] routeId    – route's id number;
 * @param[in] city       – pointer to a city name;
 * @return Value @p true if the route has been extended.
 * Value @p false if an error occurred: one of the parameters was incorrect,
 * a route with a given number does not exist, a city with a given name does not exist.
 * the given route already goes through the given city, the given route already ends
 * in the given city, route section could not have been determined
 * unambiguously or the memory could not be allocated.
 */
bool extendRoute(Map *map, unsigned routeId, const char *city);

/** @brief Removes a road between two different cities.
 * Removes a road between two cities. If removing the given road disconnects
 * a route, it gets complemented in such a way that the new section is the
 * shortest possible. If there is more than one way to do so,
 * for each option it looks for a road with the oldest year
 * of build/repair and then selects the newer one.
 * @param[in,out] map    – pointer to a route map;
 * @param[in] city1      – pointer to a city name;
 * @param[in] city2      – pointer to a city name;
 * @return Value @p true if an error occurred and the road could not have
 * been removed: one of the parameters was incorrect, one of the cities does not
 * exist, a road between the given cities does not exist, route section could not
 * have been determined unambiguously or the memory could not be allocated.
 */
bool removeRoad(Map *map, const char *city1, const char *city2);

/** @brief Removes a route from the map.
 * Removes a route from the map and all the information about it, i.e.
 * the roads and cities are no longer a part of it. It does not delete
 * nor change any of the roads and cities in the map.
 * @param[in, out] map - pointer to a route map;
 * @param[in] routeId - route's id number;
 * @return @p true if the route has been successfully removed,
 * @p false if the parameters were incorrect or the route has already
 * been removed or has never been created.
 */
bool removeRoute(Map *map, unsigned routeId);

/** @brief Provides the information about a route.
 * Returns a pointer to a string, which contains the information about
 * a route. Allocates memory for such a string. Returns an empty string if there
 * is no route with such number. Allocated memory needs to be freed with free
 * function. Information is provided in the following format:
 * route number:city name:road length:year of build or last repair:city name:
 * road length:year of build or last repair:city name;...;city name.
 * Order of the cities on the list is such that @p city1 and @p city2 provided
 * in @ref newRoute function call, which created such route, are written out in
 * such succession.
 * @param[in ,out] map - pointer to a route map;
 * @param[in] routeId - route's id number.
 * @return Pointer to a string or NULL if the memory could not have been allocated.
 */
char const* getRouteDescription(Map *map, unsigned routeId);

#endif /* __MAP_H__ */
