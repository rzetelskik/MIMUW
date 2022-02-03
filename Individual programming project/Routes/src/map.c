#include "map.h"
#include "city.h"
#include "datacheck.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

Map *newMap(void) {
    Map *map = malloc(sizeof(Map));

    if (map != NULL) {
        initRouteArray(&(map->routeArray));

        if (initCityHashmap(&(map->cityHashmap))) {
            return map;
        }

        free(map);
    }
    return NULL;
}

void deleteMap(Map *map) {
    if (map != NULL) {
        clearCityHashmap(&(map->cityHashmap));
        clearRouteArray(&(map->routeArray));

        free(map);
    }
}

bool addRoad(Map *map, const char *city1, const char *city2, unsigned length, int builtYear) {
    if (map != NULL && !areNamesEqual(city1, city2) && isNameCorrect(city1) &&
    isNameCorrect(city2) && isLengthCorrect(length) && isYearCorrect(builtYear)) {

        City *cityA = getCityFromHashmap(&(map->cityHashmap), (char*) city1);
        City *cityB = getCityFromHashmap(&(map->cityHashmap), (char*) city2);

        if (cityA == NULL) {
            cityA = insertCityToHashmap(&(map->cityHashmap), (char*) city1);
        }
        if (cityB == NULL) {
            cityB = insertCityToHashmap(&(map->cityHashmap), (char*) city2);
        }
        if (cityA != NULL && cityB != NULL && getRoad(&(cityA->roadList), cityB) == NULL) {
            return createRoad(cityA, cityB, length, builtYear);
        }
    }
    return false;
}

bool repairRoad(Map *map, const char *city1, const char *city2, int repairYear) {
    if (map != NULL && !areNamesEqual(city1, city2) && isNameCorrect(city1) &&
    isNameCorrect(city2) && isYearCorrect(repairYear)) {

        City *cityA = getCityFromHashmap(&(map->cityHashmap), (char*) city1);
        City *cityB = getCityFromHashmap(&(map->cityHashmap), (char*) city2);

        if (cityA != NULL && cityB != NULL) {
            Road *roadA = getRoad(&(cityA->roadList), cityB);
            Road *roadB = getRoad(&(cityB->roadList), cityA);

            if (roadA != NULL && roadB != NULL && repairYear >= roadA->buildYear) {
                roadA->buildYear = repairYear;
                roadB->buildYear = repairYear;

                return true;
            }
        }
    }
    return false;
}

bool newRoute(Map *map, unsigned routeId, const char *city1, const char *city2) {
    if (map != NULL && !areNamesEqual(city1, city2) && isNameCorrect(city1) &&
    isNameCorrect(city2) && isRouteIdCorrect(routeId) && !isRouteActive(&map->routeArray, routeId)) {

        City *cityA = getCityFromHashmap(&(map->cityHashmap), (char*) city1);
        City *cityB = getCityFromHashmap(&(map->cityHashmap), (char*) city2);

        if (cityA != NULL && cityB != NULL) {
            Route *route = getRoute(&(map->routeArray), routeId);
            RouteResult *result = findBestRoute(map->cityHashmap.cityCount, cityA, cityB, route);

            if (result != NULL) {
                if (!result->undetermined) {
                    route->active = true;
                    route->cities = result->cityList;
                    markRouteRoads(route);

                    free(result);
                    return true;
                }

                freeRouteResult(result);
            }
        }
    }
    return false;
}

bool extendRoute(Map *map, unsigned routeId, const char *city) {
    if (map != NULL && isNameCorrect(city) && isRouteIdCorrect(routeId) && isRouteActive(&map->routeArray, routeId)) {
        City *newCity = getCityFromHashmap(&(map->cityHashmap), (char*) city);
        Route *route = getRoute(&(map->routeArray), routeId);

        if (newCity != NULL && !isCityInRoute(route, newCity)) {
            RouteResult *extension1 = findBestRoute(map->cityHashmap.cityCount, newCity,
                                                 getFirstInCityList(route->cities), route);
            RouteResult *extension2 = findBestRoute(map->cityHashmap.cityCount, newCity,
                                                 getLastInCityList(route->cities), route);

            RouteResult *bestExtension = pickResult(extension1, extension2);
            bool memOk = false;

            if (bestExtension != NULL) {
                memOk = concatCityListToRoute(bestExtension->cityList, route);
            }
            if (extension1 != NULL) {
                freeRouteResult(extension1);
            }
            if (extension2 != NULL) {
                freeRouteResult(extension2);
            }
            if (memOk) {
                markRouteRoads(route);
                return true;
            }
        }
    }
    return false;
}

static bool canRemoveRoad(Map *map, City *cityA, City *cityB, Road *roadA) {
    bool canRemove = true;

    RouteList *routeList = &(roadA->routeList);
    ListNode *node = routeList->list.first;

    while (canRemove && node != NULL) {
        RouteResult *extension = findBestRoute(map->cityHashmap.cityCount, cityA, cityB, (Route*) node->data);

        if (extension == NULL) {
            canRemove = false;
        } else {
            if (extension->undetermined) {
                canRemove = false;
            }
            freeRouteResult(extension);
        }
        node = node->next;
    }

    return canRemove;
}

bool removeRoad(Map *map, const char *city1, const char *city2) {
    if (map != NULL && !areNamesEqual(city1, city2) &&
    isNameCorrect(city1) && isNameCorrect(city2)) {
        City *cityA = getCityFromHashmap(&map->cityHashmap, (char*) city1);
        City *cityB = getCityFromHashmap(&map->cityHashmap, (char*) city2);

        if (cityA != NULL && cityB != NULL) {
            Road *roadA = getRoad(&(cityA->roadList), cityB);
            Road *roadB = getRoad(&(cityB->roadList), cityA);

            if (roadA != NULL && roadB != NULL && canRemoveRoad(map, cityA, cityB, roadA)) {
                RouteList *routeList = &(roadA->routeList);
                ListNode *routeNode = routeList->list.first;
                bool memOK = true;

                while (memOK && routeNode != NULL) {
                    RouteResult *extension = findBestRoute(map->cityHashmap.cityCount,
                            cityA, cityB, (Route *) routeNode->data);
                    memOK = insertCityListIntoRoute(extension->cityList, routeNode->data, cityA, cityB);
                    markRouteRoads(routeNode->data);

                    freeRouteResult(extension);
                    routeNode = routeNode->next;
                }

                if (memOK) {
                    deleteRoad(&cityA->roadList, roadA);
                    deleteRoad(&cityB->roadList, roadB);

                    return true;
                }
            }
        }
    }
    return false;
}

bool removeRoute(Map *map, unsigned routeId) {
    if (map != NULL && isRouteIdCorrect(routeId) && isRouteActive(&map->routeArray, routeId)) {
        Route *route = getRoute(&map->routeArray, routeId);

        unmarkRouteRoads(route);
        clearRoute(&map->routeArray, routeId);

        return true;
    }
    return false;
}


char const* getRouteDescription(Map *map, unsigned routeId) {
    if (map != NULL && isRouteIdCorrect(routeId) && isRouteActive(&map->routeArray, routeId)) {
        Route *route = getRoute(&map->routeArray, routeId);
        int size = countCharacters(route);

        if (size > 0) {
            char *str = calloc(sizeof(char), size);
            char *buffer = calloc(sizeof(char), size);

            if (str != NULL && buffer != NULL) {
                sprintf(buffer, "%u", route->id);
                strcat(str, buffer);
                sprintf(buffer, "%c", ';');
                strcat(str, buffer);
                ListNode *node = route->cities->list.first;
                City *origin, *destination;
                Road *road;
                while (node != NULL && node->next != NULL) {
                    origin = node->data;
                    destination = node->next->data;
                    road = getRoad(&origin->roadList, destination);
                    strcat(str, (origin->name));
                    sprintf(buffer, "%c", ';');
                    strcat(str, buffer);
                    sprintf(buffer, "%u", road->length);
                    strcat(str, buffer);
                    sprintf(buffer, "%c", ';');
                    strcat(str, buffer);
                    sprintf(buffer, "%d", road->buildYear);
                    strcat(str, buffer);
                    sprintf(buffer, "%c", ';');
                    strcat(str, buffer);
                    node = node->next;
                }
                if (node != NULL) {
                    destination = node->data;
                    if (destination != NULL) {
                        strcat(str, (destination->name));
                    }
                }
                sprintf(buffer, "%c", '\0');
                strcat(str, buffer);
                free(buffer);
                return (char const*) str;
            } else {
                return NULL;
            }
        }
    }
    return calloc(sizeof(char), 1);
}