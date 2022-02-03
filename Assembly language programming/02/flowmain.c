#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cpuid.h>
#include "err.h"

#define ALIGN(SIZE, ALIGNMENT) (((SIZE) / (ALIGNMENT) + ((SIZE) % (ALIGNMENT) ? 1 : 0)) * (ALIGNMENT))

extern void start(int width, int height, float *M, float weight);
extern void step(float T[]);

static FILE *fp = NULL;
static char *line = NULL, *parseline = NULL;

void cpuid(int info[4], int InfoType){
    __cpuid_count(InfoType, 0, info[0], info[1], info[2], info[3]);
}

void print_matrix(float *M, int height, int width, int aligned_width, int step) {
    printf("MATRIX steps %d:\n", step);
    for (int r = 0; r < height; r++) {
        for (int c = 1; c < width + 1; c++) {
            printf("   %.4f ", M[r * aligned_width + c]);    
        }
        printf("\n");
    }
}

int avx2_supported() {
    int info[4];

    cpuid(info, 0);
    if (info[0] < 0x00000007)
        return 0;

    cpuid(info, 0x00000007);

    return info[1] & bit_AVX2;
}

__attribute__((destructor))
void clean() {
    if (fp) 
        fclose(fp);
    if (line)
        free(line);
}

int main(int argc, char *argv[]) {
    size_t len;
    ssize_t read;
    int width, height, steps;
    int r, c, offset;
    float weight;
    float scanned;

    if (argc != 2)
        fatal("Usage: %s input_file", argv[0]);

    if (!avx2_supported())
        fatal("this processor does not support AVX2");

    fp = fopen(argv[1], "r");
    if (!fp)
        fatal("No such file: %s", argv[1]);

    if (getline(&line, &len, fp) < 0)
        fatal("getline");

    if (sscanf(line, "%d %d %f", &width, &height, &weight) != 3 || !width || !height)
        fatal("incorrect width, height or weight");

    // Align to use ymm registers comfortably
    int aligned_width = ALIGN(width + 1, 8);
    size_t aligned = 1 + aligned_width * height;
    float M[aligned];
    memset(M, 0, aligned * sizeof(float));
    float *M_offset = &M[1];

    r = 0;
    while ((read = getline(&line, &len, fp)) > 0 && r < height) {
        c = 1;
        parseline = line;
        while (c < width + 1 && (parseline - line) < read && sscanf(parseline, "%f%n", &M_offset[r * aligned_width + c], &offset) != 2) {
            c++;
            parseline += offset;
        }
        if ((parseline - line) != read - 1 || *parseline != '\n' || width + 1 != c)
            fatal("incorrect row");
        r++;
    }

    if (r != height)
        fatal("missing rows");

    if (sscanf(line, "%d", &steps) != 1 && steps < 0)
        fatal("steps value incorrect");

    start(aligned_width, height, M, weight);

    print_matrix(M_offset, height, width, aligned_width, 0);

    int s = 0;
    float T[height];
    while (s < steps && (read = getline(&line, &len, fp)) > 0) {
        r = 0;
        parseline = line;
        while (r < height && (parseline - line) < read && sscanf(parseline, "%f%n", &T[r], &offset) != 2) {
            r++;
            parseline += offset;
        }
        if ((parseline - line) != read - 1 || *parseline != '\n' || height != r)
            fatal("incorrect step");

        step(T);

        print_matrix(M_offset, height, width, aligned_width, ++s);
    }

    return 0;
}