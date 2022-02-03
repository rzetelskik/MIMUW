#include "city.h"
#include "road.h"

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define CITY_HASHMAP_SIZE 15737

City *newCity(char *name, unsigned id) {
    City *city = malloc(sizeof(City));

    if (city != NULL) {
        char *copy = malloc((strlen(name) + 1) * sizeof(char));
        if (copy == NULL) {
            free(city);
            return NULL;
        } else {
            city->id = id;
            city->name = strcpy(copy, name);
            initRoadList(&(city->roadList));
        }
    }
    return city;
}

void freeCity(void *city) {
    if (city != NULL) {
        clearRoadList(&(((City *) city)->roadList));
        free(((City*)city)->name);
        free(city);
    }
}

CityList *newCityList() {
    CityList *cityList = malloc(sizeof(CityList));
    if (cityList != NULL) {
        initList(&(cityList->list), &free);
    }
    return cityList;
}

bool cityListPrepend(CityList *cityList, City *city) {
    if (cityList != NULL) {
        return listPrepend(&(cityList->list), city);
    }
    return false;
}

bool cityListAppend(CityList *cityList, City *city) {
    if (cityList != NULL) {
        return listAppend(&(cityList->list), city);
    }
    return false;
}

City *getFirstInCityList(CityList *cityList) {
    if (cityList != NULL) {
        return (City*) getFirst(&cityList->list);
    }
    return NULL;
}

City *getLastInCityList(CityList *cityList) {
    if (cityList != NULL) {
        return (City*) getLast(&cityList->list);
    }
    return NULL;
}

bool isInCityList(CityList *cityList, City *city) {
    if (cityList != NULL) {
        return isInList(&(cityList->list), city);
    }
    return false;
}

void freeCityList(CityList *cityList) {
    if (cityList != NULL) {
        freeList(&(cityList->list), false, false);
        free(cityList);
    }
}

static void freeCityHashmapEntry(void *hashmapEntry) {
    if (hashmapEntry != NULL) {
        freeCity(((HashMapEntry*)hashmapEntry)->value);
        free(hashmapEntry);
    }
}

//String hashing implementation from JVM
static unsigned int hashString(void *name) {
    unsigned int hash = 0;
    int length = strlen((char*)name);
    if (length > 0) {
        for (int i = 0; i < length; i++) {
            hash = ((hash << 5) - hash) + ((char*)name)[i];
        }
    }
    return (unsigned int) hash % CITY_HASHMAP_SIZE;
}

static bool compareStrings(void *key1, void *key2) {
    return (strcmp((char*)key1, (char*)key2) == 0);
}

bool initCityHashmap(CityHashMap *cityHashmap) {
    cityHashmap->cityCount = 0;
    return initHashmap(&(cityHashmap->hashMap), CITY_HASHMAP_SIZE, &hashString, &compareStrings, &freeCityHashmapEntry);
}

City *getCityFromHashmap(CityHashMap *cityHashmap, char *key) {
    return (City*) hashmapGet(&(cityHashmap->hashMap), key);
}

City *insertCityToHashmap(CityHashMap *cityHashmap, char *key) {
    City *city = newCity(key, cityHashmap->cityCount);
    if (city != NULL) {
        if (hashmapInsert(&(cityHashmap->hashMap), city->name, city)) {
            cityHashmap->cityCount++;
        } else {
            freeCity(city);
            return NULL;
        }
    }
    return city;
}

void clearCityHashmap(CityHashMap *cityHashmap) {
    if (cityHashmap != NULL) {
        clearHashMap(&(cityHashmap->hashMap));
    }
}

