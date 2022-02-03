/** @file
 * Road class interface.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */
 #ifndef ROAD_H
#define ROAD_H

#include "gen_list.h"
#include "route.h"

/**
 * Road structure.
 */
typedef struct Road {
    ///Pointer to the city of destination.
    struct City *destination;
    ///List of routes that the road is a part of.
    RouteList routeList;
    ///Length of the road.
    unsigned length;
    ///Road's year of build.
    int buildYear;
} Road;

/**
 * Road list structure using generic list implementation.
 */
typedef struct RoadList {
    ///Generic list.
    List list;
} RoadList;

/**@brief Creates and initialises a new road.
 * @param[in] destination - pointer to a city of destination.
 * @param[in] length - length of the road.
 * @param[in] buildYear - year in which the road was built.
 * @return pointer to a new Road structure if it was created successfully. NULL otherwise.
 */
Road *newRoad(struct City *destination, unsigned length, int buildYear);


/**@brief Searches in a list for a road leading to a specified destination.
 * @param[in] roadList - a pointer to road list.
 * @param[in] destination - pointer to city of destination.
 * @return pointer to Road structure if it was found, NULL otherwise.
 */
Road *getRoad(RoadList *roadList, struct City *destination);

/**@brief Deletes a road from a road list.
 * @param[in,out] roadList - a pointer to road list.
 * @param[in] road - pointer to road.
 */
void deleteRoad(RoadList *roadList, Road *road);

/**@brief Free the road from memory.
 * @param[in] road - pointer to road.
 */
void freeRoad(void *road);

/**@brief Initialises a road list.
 * @param[in,out] roadList - pointer to a road list.
 */
void initRoadList(RoadList *roadList);

/**@brief Appends road to a road list.
 * @param[in,out] roadList - pointer to road list.
 * @param[in] road - pointer to road to be inserted.
 * @return @p true if inserted successfully, @p false otherwise.
 */
bool roadListAppend(RoadList *roadList, Road *road);

/**@brief Creates a new road and connects two cities.
 * @param[in,out] cityA - pointer to a city to be connected.
 * @param[in,out] cityB - pointer to a city to be connected.
 * @param[in] length - length of the road.
 * @param[in] buildYear - year in which the road was built.
 * @return @p true if created successfully, @p false otherwise.
 */
bool createRoad(struct City *cityA, struct City *cityB, unsigned length, int buildYear);

/**@brief Clears the contents of a road list.
 * @param[in,out] roadList - road list to be cleared.
 */
void clearRoadList(RoadList *roadList);

#endif //ROAD_H
