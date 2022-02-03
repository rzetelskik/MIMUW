#include <stdint.h>
#include <values.h>
#include "route.h"
#include <stdio.h>
#include "road.h"
#include "city.h"
#include "gen_pqueue.h"

#define MIN(a,b) (((a)<(b))?(a):(b))

static void initVertex(Vertex *vertex) {
    if (vertex != NULL) {
        vertex->distance = UINT64_MAX;
        vertex->oldestRoad = INT_MIN;
        vertex->prevCity = NULL;
        vertex->visited = false;
        vertex->undetermined = false;
    }
}

static void initVertexArray(Vertex *array, int size, unsigned originId) {
    if (array != NULL && size > 0) {
        for (int i = 0; i < size; i++) {
            initVertex(&(array[i]));
        }
        array[originId].distance = 0;
        array[originId].oldestRoad = INT_MAX;
    }
}

static void updateVertex(Vertex *vertex, uint64_t distance, int oldestRoad, City *prevCity) {
    if (vertex->undetermined) {
        vertex->undetermined = false;
    }
    vertex->distance = distance;
    vertex->oldestRoad = oldestRoad;
    vertex->prevCity = prevCity;
}

void initRoute(Route *route, unsigned id) {
    if (route != NULL) {
        route->id = id;
        route->cities = NULL;
        route->active = false;
    }
}

Route *getRoute(RouteArray *routeArray, unsigned id) {
    if (routeArray != NULL) {
        return &(routeArray->array[id - 1]);
    }
    return NULL;
}

bool isRouteActive(RouteArray *routeArray, unsigned id) {
    if (routeArray != NULL) {
        return ((*routeArray).array[id - 1].active);
    }
    return false;
}

void clearRoute(RouteArray *routeArray, unsigned id) {
    if (routeArray != NULL) {
        Route *route = getRoute(routeArray, id);
        route->active = false;
        if (route->cities != NULL) {
            freeCityList(route->cities);
            route->cities = NULL;
        }
    }
}

void initRouteArray(RouteArray *routeArray) {
    if (routeArray != NULL) {
        for (int i = 0; i < ROUTE_ARRAY_SIZE; i++) {
            initRoute(&(routeArray->array[i]), i + 1);
        }
    }
}

void clearRouteArray(RouteArray *routeArray) {
    if (routeArray != NULL) {
        for (int i = 0; i < ROUTE_ARRAY_SIZE; i++) {
            clearRoute(routeArray, i + 1);
        }
    }
}

void initRouteList(RouteList *routeList) {
    if (routeList != NULL) {
        initList(&(routeList->list), &free);
    }
}

bool routeListPrepend(RouteList *routeList, Route *route) {
    if (routeList != NULL) {
        return listPrepend(&(routeList->list), route);
    }
    return false;
}

bool isInRouteList(RouteList *routeList, Route *route) {
    if (routeList != NULL) {
        return isInList(&(routeList->list), route);
    }
    return false;
}

void removeFromRouteList(RouteList *routeList, Route *route) {
    if(routeList != NULL && route != NULL) {
        removeFromList(&routeList->list, route);
    }
}

void freeRouteList(RouteList *routeList) {
    if (routeList != NULL) {
        freeList(&routeList->list, false, false);
    }
}

RouteResult *newRouteResult() {
    RouteResult *result = malloc(sizeof(RouteResult));
    if (result != NULL) {
        result->undetermined = false;
    }
    return result;
}

void freeRouteResult(RouteResult *routeResult) {
    if (routeResult != NULL) {
        freeCityList(routeResult->cityList);
        free(routeResult);
    }
}

bool canExtend(Route *route, City *origin, City *target, Road *road) {
    if (route != NULL && route->active) {
        return !(isInRouteList(&(road->routeList), route) ||
        (road->destination != origin && road->destination != target && isCityInRoute(route, road->destination)));
    }
    return true;
}

RouteResult *recoverRouteResult(Vertex *vertices, City *target) {
    Vertex *currVertex = &vertices[target->id];

    if (currVertex->visited) {
        bool memOK = false;
        RouteResult *result = newRouteResult();
        CityList *cities = newCityList();

        if (cities != NULL && result != NULL) {
            memOK = cityListPrepend(cities, target);
            result->distance = vertices[target->id].distance;
            result->oldestRoad = vertices[target->id].oldestRoad;
        }

        if (memOK) {
            while (memOK && currVertex->prevCity != NULL && !result->undetermined) {
                if (currVertex->undetermined) {
                    result->undetermined = true;
                }
                memOK = cityListPrepend(cities, currVertex->prevCity);

                currVertex = &vertices[currVertex->prevCity->id];
            }
            if (memOK) {
                result->cityList = cities;
                return result;
            } else {
                freeCityList(cities);
                free(result);
            }
        }
    }
    return NULL;
}

static RouteResult *findBestRouteDijkstra(int numVertices, City *origin, City *target, Route *thisRoute) {
    Vertex vertices[numVertices];
    initVertexArray(vertices, numVertices, origin->id);
    PQueue pQueue;
    initPQueue(&pQueue);

    if (!pQueuePush(&pQueue, UINT64_MAX, origin)) {
        return NULL;
    }
    while (!isPQueueEmpty(&pQueue)) {
        City *currCity = pQueuePop(&pQueue);

        if (!(vertices[currCity->id].visited)) {
            vertices[currCity->id].visited = true;

            if (currCity != target) {
                ListNode *node = currCity->roadList.list.first;

                while (node != NULL) {
                    Road *road = node->data;
                    Vertex *nextVertex = &vertices[road->destination->id];

                    if (canExtend(thisRoute, origin, target, road)) {

                        if (vertices[currCity->id].distance + road->length < nextVertex->distance) {
                            updateVertex(nextVertex, vertices[currCity->id].distance + road->length,
                                    MIN(road->buildYear, vertices[currCity->id].oldestRoad), currCity);
                        } else if (vertices[currCity->id].distance + road->length == nextVertex->distance) {

                            if (nextVertex->oldestRoad < MIN(road->buildYear, vertices[currCity->id].oldestRoad)) {

                                updateVertex(nextVertex, vertices[currCity->id].distance + road->length,
                                             MIN(road->buildYear, vertices[currCity->id].oldestRoad), currCity);

                            } else if (nextVertex->oldestRoad == MIN(road->buildYear, vertices[currCity->id].oldestRoad)) {
                                nextVertex->undetermined = true;
                            }
                        }
                        if (!nextVertex->visited && !pQueuePush(&pQueue, nextVertex->distance, road->destination)) {
                            clearPQueue(&pQueue);
                            return NULL;
                        }
                    }
                    node = node->next;
                }
            } else {
                clearPQueue(&pQueue);
            }
        }
    }

    return recoverRouteResult(vertices, target);
}

RouteResult *findBestRoute(int numVertices, City *origin, City *target, Route *thisRoute) {
    RouteResult *result1 = findBestRouteDijkstra(numVertices, origin, target, thisRoute);
    RouteResult *result2 = findBestRouteDijkstra(numVertices, target, origin, thisRoute);

    if (result1 != NULL && result2 != NULL) {
        if (result1->undetermined || result2->undetermined) {
            result1->undetermined = true;
        }
        freeRouteResult(result2);
        return result1;
    }
    if (result1 != NULL) {
        result1->undetermined = true;
        return result1;
    }
    if (result2 != NULL) {
        result2->undetermined = true;
        return result2;
    }
    return NULL;
}

void markRouteRoads(Route *route) {
    if (route != NULL) {
        ListNode *curr = route->cities->list.first;

        while (curr != NULL && curr->next != NULL) {
            City *cityA = (City*) curr->data;
            City *cityB = (City*) curr->next->data;
            Road *roadA = getRoad(&(cityA->roadList), cityB);
            Road *roadB = getRoad(&(cityB->roadList), cityA);

            if (!isInRouteList(&(roadA->routeList), route)) {
                routeListPrepend(&(roadA->routeList), route);
            }

            if (!isInRouteList(&(roadB->routeList), route)) {
                routeListPrepend(&(roadB->routeList), route);
            }

            curr = curr->next;
        }
    }
}

void unmarkRouteRoads(Route *route) {
    if (route != NULL) {
        ListNode *curr = route->cities->list.first;

        while (curr != NULL && curr->next != NULL) {
            City *cityA = (City*) curr->data;
            City *cityB = (City*) curr->next->data;
            Road *roadA = getRoad(&(cityA->roadList), cityB);
            Road *roadB = getRoad(&(cityB->roadList), cityA);

            if (isInRouteList(&(roadA->routeList), route)) {
                removeFromRouteList(&(roadA->routeList), route);
            }
            if (isInRouteList(&(roadB->routeList), route)) {
                removeFromRouteList(&(roadB->routeList), route);
            }

            curr = curr->next;
        }
    }
}

bool isCityInRoute(Route *route, City *city) {
    if (route != NULL) {
        return isInCityList(route->cities, city);
    }
    return false;
}

RouteResult *pickResult(RouteResult *result1, RouteResult *result2) {
    if (result1 != NULL && result2 != NULL) {
        if (result1->distance < result2->distance && !result1->undetermined) {
            return result1;
        }
        if (result1->distance > result2->distance && !result2->undetermined) {
            return result2;
        }
        if (result1->distance == result2->distance) {
            if (result1->oldestRoad > result2->oldestRoad && !result1->undetermined) {
                return result1;
            }
            if (result1->oldestRoad < result2->oldestRoad && !result2->undetermined) {
                return result2;
            }
        }
    } else if (result1 != NULL && !result1->undetermined) {
        return result1;
    } else if (result2 != NULL && !result2->undetermined) {
        return result2;
    }

    return NULL;
}

bool concatCityListToRoute(CityList *segment, Route *route) {
    if (segment != NULL && route != NULL) {
        bool memOK = true;
        CityList *ogList = route->cities;
        ListNode *currSegNode = segment->list.first;
        City *firstOg = getFirstInCityList(ogList);
        City *lastOg = getLastInCityList(ogList);

        if (currSegNode->data == firstOg) {
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != firstOg && currSegNode->data != lastOg) {
                    memOK = listPrepend(&ogList->list, currSegNode->data);
                }
                currSegNode = currSegNode->next;
            }
            return memOK;
        }
        if (currSegNode->data == lastOg) {
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != firstOg && currSegNode->data != lastOg) {
                    memOK = listAppend(&ogList->list, currSegNode->data);
                }
                currSegNode = currSegNode->next;
            }
            return memOK;
        }
        if (getLastInCityList(segment) == firstOg) {
            while (currSegNode->next != NULL) {
                currSegNode = currSegNode->next;
            }
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != firstOg && currSegNode->data != lastOg) {
                    memOK = listPrepend(&ogList->list, currSegNode->data);
                }
                currSegNode = currSegNode->prev;
            }
            return memOK;
        }
        if (getLastInCityList(segment) == lastOg) {
            while (currSegNode->next != NULL) {
                currSegNode = currSegNode->next;
            }
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != firstOg && currSegNode->data != lastOg) {
                    memOK = listAppend(&ogList->list, currSegNode->data);
                }
                currSegNode = currSegNode->prev;
            }
            return memOK;
        }
    }
    return false;

}

bool insertCityListIntoRoute(CityList *segment, Route *route, City *cityA, City *cityB) {
    if (segment != NULL && route != NULL) {
        bool memOK = true;
        CityList *ogList = route->cities;
        City *otherCity;
        ListNode *currOgNode = ogList->list.first, *currSegNode = segment->list.first;
        while (currOgNode->data != cityA && currOgNode->data != cityB) {
            currOgNode = currOgNode->next;
        }
        if (currOgNode->data == cityA) {
            otherCity = cityB;
        } else {
            otherCity = cityA;
        }
        if (currOgNode->data == currSegNode->data) {
            while (currOgNode->data != otherCity) {
                currOgNode = currOgNode->next;
            }
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != cityA && currSegNode->data != cityB) {
                    memOK = listInsertBefore(&ogList->list, currOgNode, currSegNode->data);
                }
                currSegNode = currSegNode->next;
            }
        } else {
            while (memOK && currSegNode != NULL) {
                if (currSegNode->data != cityA && currSegNode->data != cityB) {
                    memOK = listInsertAfter(&ogList->list, currOgNode, currSegNode->data);
                }
                currSegNode = currSegNode->next;
            }
        }
        return memOK;
    }
    return false;
}