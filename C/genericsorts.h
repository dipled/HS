#ifndef _GENERICSORTS_H
#define _GENERICSORTS_H

#include <stdlib.h>


/* Merge-Sort */

void mg(void *vet, int s, int mid, int e, size_t typesize, int (*comparefn)(const void*, const void*));
void ms(void *vet, int s, int e, size_t typesize, int (*comparefn)(const void*, const void*));
void mergesort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void*, const void*));

#endif
