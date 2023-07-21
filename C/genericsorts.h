#ifndef _GENERICSORTS_H
#define _GENERICSORTS_H

#include <stdlib.h>


/* Merge-Sort */
void mergesort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void*, const void*));

#endif
