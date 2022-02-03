/** @file
 * City class interface.
 *
 * @author Kacper Rzetelski
 * @date 29.04.2019
 */
#ifndef CITY_H
#define CITY_H

#include "gen_hashmap.h"
#include "gen_list.h"
#include "road.h"

/**
 * City structure.
 */
typedef struct City {
    ///City's id number.
    unsigned id;
    ///Name of the city.
    char *name;
    ///List of the roads that go out of the city.
    RoadList roadList;
} City;

/**
 * City list structure using generic list implementation.
 */
typedef struct CityList {
    ///Generic list structure.
    List list;
} CityList;

/**
 * City HashMap structure using generic hashMap implementation.
 */
typedef struct CityHashMap {
    ///Generic hashMap structure.
    HashMap hashMap;
    ///Amount of cities already added to the structure.
    unsigned cityCount;
} CityHashMap;

/**@brief Creates a new city.
 * A new city is created with a unique id number and assigned name.
 * @param[in] name - name of a city.
 * @param[in] id - id a of a city.
 * @return Pointer to a city if created successfully, NULL otherwise.
 */
City *newCity(char *name, unsigned id);

/**@brief Free a city from memory.
 * @param[in, out] city - a city to be freed.
 */
void freeCity(void *city);

/**@brief Creates a new city list.
 * @return Pointer to a new city list if created successfully, NULL otherwise.
 */
CityList *newCityList();

/**@brief Prepends data to a city list.
 * @param[in,out] cityList - pointer to a city list.
 * @param[in] city - pointer to a city to be inserted.
 * @return @p true if added successfully, @p false otherwise.
 */
bool cityListPrepend(CityList *cityList, City *city);

/**@brief Appends data to a city list.
 * @param[in,out] cityList - pointer to a city list.
 * @param[in] city - pointer to a city to be inserted.
 * @return @p true if added successfully, @p false otherwise.
 */
bool cityListAppend(CityList *cityList, City *city);

/**@brief Searches for the first element in a city list.
 * @param[in] cityList - pointer to a city list.
 * @return Pointer to city if the first element exists, NULL otherwise.
 */
City *getFirstInCityList(CityList *cityList);

/**@brief Searches for the last element in a city list.
 * @param[in] cityList - pointer to a city list.
 * @return Pointer to city if the last element exists, NULL otherwise.
 */
City *getLastInCityList(CityList *cityList);

/**@brief Searches for a city in a city list.
 * @param[in] cityList - pointer to a city list.
 * @param[in] city - pointer to a city to be searched for.
 * @return @p true if the city has been found in the list,
 * @p false otherwise.
 */
bool isInCityList(CityList *cityList, City *city);

/**@brief Free the city list from memory.
 * @param[in,out] cityList - city list to be freed.
 */
void freeCityList(CityList *cityList);

/**@brief Initialises the city hashMap.
 * @param cityHashMap - pointer to a city hashMap.
 */
bool initCityHashmap(CityHashMap *cityHashMap);

/**@brief Searches for a city in a hashMap.
 * @param[in] cityHashMap - city hashMap.
 * @param[in] key - key for searching.
 * @return Pointer to the city if found, NULL otherwise.
 */
City *getCityFromHashmap(CityHashMap *cityHashMap, char *key);

/**@brief Creates a city and inserts it to a hashMap.
 * @param[in,out] cityHashMap - city hashMap.
 * @param[in] key - key to find a location for data.
 * @return Pointer to the city created if successful, NULL otherwise.
 */
City *insertCityToHashmap(CityHashMap *cityHashMap, char *key);

/**@brief Clear the contents of the city hashMap.
 * @param[in,out] cityHashMap - city HashMap.
 */
void clearCityHashmap(CityHashMap *cityHashMap);




#endif //CITY_H
