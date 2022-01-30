#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void printInt(int i) {
    printf("%d\n", i);
}

void printString(char *s) {
    if (s) {
        printf("%s\n", s);
    } else {
        printf("\n");
    }
}

void error() {
    exit(1);
}

int readInt() {
    int ret;
    scanf("%d", &ret);
    getchar();
    return ret;
}

char* readString() {
    char *buff = NULL;
    size_t len;

    int read = getline(&buff, &len, stdin); 
    if (read < 0) return NULL;

    buff[read - 1] = '\0';
    return buff;
}

char *_concat(char *s1, char *s2) {
    char *t = malloc(strlen(s1) + strlen(s2) + 1);
    return strcat(strcpy(t,s1), s2);
}

int _strcmp(char *s1, char *s2) {
    return strcmp(s1, s2) == 0;
}