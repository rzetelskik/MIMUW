#ifndef LIBPPM_H
#define LIBPPM_H

#include <stdio.h>
#include <stdint.h>

typedef enum ppmVersion {
    P3,
    P6
} PPMVersion;

typedef struct ppm {
    int width;
    int height;
    uint8_t *r;
    uint8_t *g;
    uint8_t *b;
    PPMVersion version;
} PPM;


PPM *read_ppm(FILE *f);

void write_ppm(PPM *ppm, FILE *f);

void free_ppm(PPM *ppm);

#endif