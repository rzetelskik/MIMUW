#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "err.h"

extern void start(int width, int height, char *T);
extern void run(int n_steps);

static FILE *fp = NULL;
static char *line = NULL;

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
    int width = 0, height = 0, gen = 0;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s input_file gen_num\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    fp = fopen(argv[1], "r");
    if (!fp)
        fatal("No such file: %s", argv[1]);

    gen = atoi(argv[2]);
    if (gen < 0)
        fatal("gen < 0 is not allowed");
    
    if (getline(&line, &len, fp) < 0)
        syserr("getline");

    sscanf(line, "%d %d", &width, &height);
    if (!width || !height)
        fatal("width and height can not be zero");

    char T[height][width + 1];
    memset(T, 0, height*(width+1));

    int r = 0, c; 
    while ((read = getline(&line, &len, fp)) > 0 && r < height) {
        c = 0;
        if (read != 2 * width)
            syserr("incorrect row length");
        while (c < width) {
            T[r][c] = line[2 * c] == '1' ? '#' : ' ';
            c++;
        }
        T[r][c] = '\0';
        r++;
    }

    start(width, height, (char*) T);
    run(gen);

    for (r = 0; r < height; r++) {
        for (c = 0; c < width; c++) {
            printf("%c", T[r][c]);
        }
        printf("\n");
    }

    return 0;
}