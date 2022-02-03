#include <stdlib.h>
#include <stdbool.h>
#include "road.h"
#include "city.h"
#include "gen_list.h"

Road *newRoad(City *destination, unsigned length, int buildYear) {
    Road *road = malloc(sizeof(Road));

    if (road != NULL) {
        road->destination = destination;
        road->length = length;
        road->buildYear = buildYear;
        initRouteList(&(road->routeList));
    }

    return road;
}

void deleteRoad(RoadList *roadList, Road *road) {
    if (roadList != NULL) {
        removeFromList(&(roadList->list), road);
    }
    freeRoad(road);
}

void freeRoad(void *road) {
    if (road != NULL) {
        freeRouteList(&((Road*)road)->routeList);
        free(road);
    }
}

void initRoadList(RoadList *roadList) {
    if (roadList != NULL) {
        initList(&(roadList->list), &freeRoad);
    }
}

Road *getRoad(RoadList *roadList, City *destination) {
    if (roadList != NULL) {
        ListNode *node = roadList->list.first;

        while (node != NULL && ((Road*)node->data)->destination != destination) {
            node = node->next;
        }

        if (node != NULL && ((Road*)node->data)->destination == destination) {
            return (Road*)node->data;
        }
    }

    return NULL;
}

bool createRoad(City *cityA, City *cityB, unsigned length, int buildYear) {
    if (cityA != NULL && cityB != NULL) {
        Road *roadA = newRoad(cityB, length, buildYear);
        if (roadA != NULL) {
            Road *roadB = newRoad(cityA, length, buildYear);
            if (roadB != NULL) {
                if (roadListAppend(&(cityA->roadList), roadA)) {
                    if (roadListAppend(&(cityB->roadList), roadB)) {
                        return true;
                    } else {
                        deleteRoad(&(cityA->roadList), roadA);
                        freeRoad(roadB);
                    }
                } else {
                    freeRoad(roadA);
                    freeRoad(roadB);
                }
            } else {
                freeRoad(roadA);
            }
        }
    }
    return false;
}

bool roadListAppend(RoadList *roadList, Road *road) {
    return listAppend(&(roadList->list), road);
}

void clearRoadList(RoadList *roadList) {
    freeList(&(roadList->list), true, false);
}

