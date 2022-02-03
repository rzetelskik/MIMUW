#include "datacheck.h"
#include "route.h"
#include "city.h"
#include <string.h>
#include <limits.h>

#define MAX_INT_LENGTH 11
#define MAX_UINT_LENGTH 10
#define ABS(x)  (((x) < 0) ? -(x) : (x))

bool areNamesEqual(const char *city1, const char *city2) {
    return (strcmp(city1, city2) == 0);
}

bool isNameCorrect(const char *city) {
    int length = strlen(city);
    char c;

    for (int i = 0; i < length; i++) {
        c = city[i];
        if ((c >= 0 && c <= 31) || c == ';') {
            return false;
        }
    }
    if (length == 0 || city[length] != 0) return false;

    return true;
}

bool isLengthCorrect(unsigned length) {
    return (length > 0);
}

bool isYearCorrect(int year) {
    return (year != 0);
}

bool isRouteIdCorrect(unsigned id) {
    return (id >= 1 && id <= 999);
}

unsigned countUnsignedDigits(unsigned num) {
    unsigned count = 0;
    while (num > 0) {
        num /= 10;
        count++;
    }
    return count;
}

unsigned countIntegerDigits(int num) {
    if (num == INT_MIN) {
        return MAX_INT_LENGTH;
    } else {
        unsigned count = 0;
        if (num < 0) {
            num = ABS(num);
            //Takes minus sign into account.
            count += 1;
        }
        count += countUnsignedDigits((unsigned) num) + 1;
        return count;
    }
}

int countCharacters(Route *route) {
    if (route != NULL) {
        //Extra slot for /0 at the end of the string.
        int total = 1;
        total += countUnsignedDigits(route->id) + 1;
        ListNode *node = route->cities->list.first;
        City *origin = NULL, *destination = NULL;
        Road *road = NULL;
        while (node != NULL && node->next != NULL) {
            origin = node->data;
            destination = node->next->data;
            road = getRoad(&origin->roadList, destination);
            total += strlen(origin->name) + 1;
            total += countUnsignedDigits(road->length) + 1;
            total += countIntegerDigits(road->buildYear) + 1;
            node = node->next;
        }
        if (node != NULL) {
            total += strlen(destination->name);
        }

        return total;
    }
    return 0;
}