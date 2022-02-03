#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <libgen.h>
#include "err.h"
#include "libppm.h"

extern void modify_component(uint8_t *matrix, int width, int height, int8_t component, int8_t num);

FILE *sourceFile;
PPM *ppm;

__attribute__((destructor))
void clean() {
    if (sourceFile) 
        fclose(sourceFile);
    if (ppm)
        free_ppm(ppm);
}

int main(int argc, char *argv[]) {
    int8_t component;
    int8_t num;

    if (argc < 4)
        fatal("Usage: %s file component num [test_mode]", argv[0]);

    sourceFile = fopen(argv[1], "r");
    if (!sourceFile)
        fatal("fopen");

    if (strlen(argv[2]) != 1)
        fatal ("invalid component");

    switch (argv[2][0]) {
        case 'R':
        case 'r':
            component = 1;
            break;
        case 'G':
        case 'g':
            component = 2;
            break;
        case 'B':
        case 'b':
            component = 3;
            break;
        default:
            fatal ("invalid component");
    }    

    int tmp_num = atoi(argv[3]);
    if (tmp_num > 127 || tmp_num < -127)
        fatal("invalid value");
    
    num = tmp_num;

    ppm = read_ppm(sourceFile);
    if (!ppm)
        fatal("read_ppm");

    modify_component(ppm->r, ppm->width, ppm->height, component, num);

    if (argv[4]) {
        write_ppm(ppm, stdout);
        return 0;
    }
    
    char *name = basename(argv[1]);
    char *dir = dirname(argv[1]);
    char destFilename[strlen(name) + strlen(dir) + 2];
    strcpy(destFilename, dir);
    strcat(destFilename, "/Y");
    strcat(destFilename, name);

    FILE *destinationFile = fopen(destFilename, "w");
    if (!destinationFile)
        fatal("fopen");

    write_ppm(ppm, destinationFile);
    fclose(destinationFile);
    
    return 0;
}