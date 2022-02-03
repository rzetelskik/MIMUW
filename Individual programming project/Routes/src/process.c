#include "process.h"
#include "datacheck.h"
#include <string.h>

void initProcessResult(ProcessResult *processResult) {
    if (processResult != NULL) {
        processResult->description = NULL;
    }
}

void clearProcessResult(ProcessResult *processResult) {
    if (processResult != NULL) {
        if (processResult->description != NULL) {
            free((void*)processResult->description);
        }
        initProcessResult(processResult);
    }
}

static ProcessResult processOk() {
    ProcessResult processResult;

    processResult.description = NULL;
    processResult.type = RESULT_OK;

    return processResult;
}

static ProcessResult processError() {
    ProcessResult processResult;

    processResult.description = NULL;
    processResult.type = RESULT_ERROR;

    return processResult;
}

static bool isCityNameDoubledInParams(RoadSetList *roadSetList, ListNode *checkedNode, char *cityName) {
    if (roadSetList != NULL) {
        ListNode *currNode = roadSetList->list.first;

        while (currNode != NULL && currNode != checkedNode) {
            if (strcmp(cityName, ((RoadSet*)currNode->data)->origin) == 0) {
                return true;
            }
            currNode = currNode->next;
        }
    }
    return false;
}

static bool isCreateRouteCommandCorrect(Command *command, Map *map, Route **route) {
    if (isRouteIdCorrect(command->id) && !isRouteActive(&map->routeArray, command->id)) {
        (*route) = getRoute(&map->routeArray, command->id);

        bool correct = true;
        ListNode *currNode = command->roadSetList.list.first;

        while (correct && currNode != NULL) {
            RoadSet *roadSet = (RoadSet *) currNode->data;

            if (!areNamesEqual(roadSet->origin, roadSet->destination) && isNameCorrect(roadSet->origin) &&
                isNameCorrect(roadSet->destination) && isLengthCorrect(roadSet->length) && isYearCorrect(roadSet->year) &&
                !isCityNameDoubledInParams(&command->roadSetList, currNode, roadSet->destination)) {

                City *cityA = getCityFromHashmap(&map->cityHashmap, roadSet->origin);
                City *cityB = getCityFromHashmap(&map->cityHashmap, roadSet->destination);

                if (cityA != NULL && cityB != NULL) {
                    Road *road = getRoad(&(cityA->roadList), cityB);

                    if (road != NULL) {
                        correct = (road->length == roadSet->length && roadSet->year >= road->buildYear);
                    }
                }
            } else {
                correct = false;
            }
            currNode = currNode->next;
        }
        return correct;
    }
    return false;
}

static bool prepareRoadsForRoute(Command *command, Map *map) {
    bool correct = true;
    ListNode *currNode = command->roadSetList.list.first;

    while (correct && currNode != NULL) {
        RoadSet *roadSet = (RoadSet *) currNode->data;

        City *cityA = getCityFromHashmap(&map->cityHashmap, roadSet->origin);
        City *cityB = getCityFromHashmap(&map->cityHashmap, roadSet->destination);

        if (cityA != NULL && cityB != NULL) {
            Road *road = getRoad(&(cityA->roadList), cityB);

            if (road != NULL && road->length == roadSet->length) {
                correct = repairRoad(map, roadSet->origin, roadSet->destination, roadSet->year);
            } else {
                correct = addRoad(map, roadSet->origin, roadSet->destination, roadSet->length, roadSet->year);
            }
        } else {
            correct = addRoad(map, roadSet->origin, roadSet->destination, roadSet->length, roadSet->year);
        }
        currNode = currNode->next;
    }

    return correct;
}

static ProcessResult processCreateRouteCommand(Command *command, Map *map) {
    Route *route = NULL;
    if (isCreateRouteCommandCorrect(command, map, &route) && prepareRoadsForRoute(command, map)) {
        bool memOk = true;
        ListNode *currNode = command->roadSetList.list.first;
        CityList *cityList = newCityList();

        while (memOk && currNode != NULL) {
            RoadSet *roadSet = (RoadSet*)currNode->data;

            memOk = cityListAppend(cityList, getCityFromHashmap(&map->cityHashmap, roadSet->origin));

            if (memOk && currNode->next == NULL) {
                memOk = cityListAppend(cityList, getCityFromHashmap(&map->cityHashmap, roadSet->destination));
            }

            currNode = currNode->next;
        }

        if (memOk) {
            route->active = true;
            route->cities = cityList;
            markRouteRoads(route);
            return processOk();
        }
        freeCityList(cityList);
    }
    return processError();
}

static ProcessResult processAddRoadCommand(Command *command, Map *map) {
    if (addRoad(map, command->city1, command->city2, command->length, command->year)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processRepairRoadCommand(Command *command, Map *map) {
    if (repairRoad(map, command->city1, command->city2, command->year)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processNewRouteCommand(Command *command, Map *map) {
    if (newRoute(map, command->id, command->city1, command->city2)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processExtendRouteCommand(Command *command, Map *map) {
    if (extendRoute(map, command->id, command->city1)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processRemoveRoadCommand(Command *command, Map *map) {
    if (removeRoad(map, command->city1, command->city2)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processRemoveRouteCommand(Command *command, Map *map) {
    if (removeRoute(map, command->id)) {
        return processOk();
    } else {
        return processError();
    }
}

static ProcessResult processGetRouteDescriptionCommand(Command *command, Map *map) {
    char const* description = getRouteDescription(map, command->id);

    if (description != NULL) {
        ProcessResult processResult;

        processResult.type = RESULT_DESCRIPTION;
        processResult.description = description;

        return processResult;
    } else {
        return processError();
    }
}

ProcessResult processCommand(Command *command, ParseResult parseResult, Map *map) {
    ProcessResult processResult;

    switch (parseResult) {
        case PARSE_OK:
            switch (command->type) {
                case CREATE_ROUTE:
                    processResult =  processCreateRouteCommand(command, map);
                    break;
                case ADD_ROAD:
                    processResult = processAddRoadCommand(command, map);
                    break;
                case REPAIR_ROAD:
                    processResult = processRepairRoadCommand(command, map);
                    break;
                case NEW_ROUTE:
                    processResult = processNewRouteCommand(command, map);
                    break;
                case EXTEND_ROUTE:
                    processResult = processExtendRouteCommand(command, map);
                    break;
                case REMOVE_ROAD:
                    processResult = processRemoveRoadCommand(command, map);
                    break;
                case REMOVE_ROUTE:
                    processResult = processRemoveRouteCommand(command, map);
                    break;
                case GET_ROUTE_DESCRIPTION:
                    processResult = processGetRouteDescriptionCommand(command, map);
                    break;
            }
            break;
        case MEMORY_FAIL:
        case PARSE_EOF:
        case PARSE_IGNORE:
            processResult = processOk();
            break;
        default:
            processResult = processError();
    }

    return processResult;
}