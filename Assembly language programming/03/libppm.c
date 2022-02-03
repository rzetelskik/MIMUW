#include "libppm.h"
#include <stdlib.h>
#include <string.h>

#define EOL '\n'

static void drop_eol_and_comment(FILE *f) {
    int c;
    c = fgetc(f);
    if (c != EOL) {
        ungetc(c, f);
        return;
    }

    c = fgetc(f);
    if (c != '#') {
        ungetc(c, f);
        return;
    }

    while ((c = fgetc(f) != EOL));
    ungetc(c, f);
}

static int get_version(FILE *f, PPMVersion *ppmVersion) {
    char buff[3];

    if (fread(buff, sizeof(char), 2, f) != 2)
        return 1;
    
    if (!strcmp(buff, "P3")) {
        *ppmVersion = P3;
        return 0;
    }

    if (!strcmp(buff, "P6")) {
        *ppmVersion = P6;
        return 0;
    }

    return 1;
}

static int get_size(FILE *f, int *width, int *height) {
    int discard;
    if (fscanf(f, "%d %d %d", width, height, &discard) != 3)
        return 1;
    
    return 0;
}

static int parse_p3(FILE *f, PPM *ppm) {
    int size = ppm->height * ppm->width, r, g, b;
    
    for (int i = 0; i < size; ++i) {
		if (fscanf(f, "%d %d %d", &r, &g, &b) != 3)
			return 1;
        
        ppm->r[i] = r;
        ppm->g[i] = g;
        ppm->b[i] = b;
    }

    return 0;
} 


static int parse_p6(FILE *f, PPM *ppm) {
    char c;
    if (fread(&c, 1, 1, f) != 1) 
        return 1;
    
    int size = ppm->height * ppm->width;
    for (int i = 0; i < size; ++i) {
        if (fread(&ppm->r[i], 1, 1, f) != 1)
            return 1;

        if (fread(&ppm->g[i], 1, 1, f) != 1)
            return 1;

        if (fread(&ppm->b[i], 1, 1, f) != 1)
            return 1;
    }

    return 0;
}

static PPM *new(PPMVersion version, int width, int height) {
    PPM *ppm = calloc(1, sizeof(PPM));
    if (!ppm)
        return NULL;

    ppm->version = version;
    ppm->width = width;
    ppm->height = height;

    int size = width * height;
    ppm->r = malloc(3 * size);
    if (!ppm->r) {
        free(ppm);
        return NULL;
    }
    ppm->g = ppm->r + size;
    ppm->b = ppm->r + 2 * size;

    return ppm;
}

void free_ppm(PPM *ppm) {
    free(ppm->r);
    free(ppm);
}

PPM *read_ppm(FILE *f) {
    PPMVersion ppmVersion;
    int width, height;
    
    if (get_version(f, &ppmVersion) != 0)
        return NULL;
    
    drop_eol_and_comment(f);

    if (get_size(f, &width, &height) != 0) 
        return NULL;
    
    PPM *ppm = new(ppmVersion, width, height);
    if (!ppm)
        return NULL;

    if (ppmVersion == P3) {
        if (parse_p3(f, ppm) != 0) {
            free_ppm(ppm);
            return NULL;
        }
    } else {
        if (parse_p6(f, ppm) != 0) {
            free_ppm(ppm);
            return NULL;
        }
    }

    return ppm;
}

void write_ppm(PPM *ppm, FILE *f) {
    if (ppm->version == P3)
        fprintf(f, "P3\n");
    else
        fprintf(f, "P6\n");

	fprintf(f, "%d %d\n", ppm->width, ppm->height);

	fprintf(f, "%d\n", 255);

    int size = ppm->width * ppm->height;
	if (ppm->version == P3) {
		for (int i = 0; i < size; ++i) {
			fprintf(f, "%d %d %d\n", ppm->r[i], ppm->g[i], ppm->b[i]);		
        }
	} else {
		for (int i = 0; i < size; ++i) {
			fwrite(&ppm->r[i], 1, 1, f);
			fwrite(&ppm->g[i], 1, 1, f);
			fwrite(&ppm->b[i], 1, 1, f);
		}
	}
}
