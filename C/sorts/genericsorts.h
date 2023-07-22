#ifndef _GENERICSORTS_H
#define _GENERICSORTS_H

#include <stdlib.h>
#include <memory.h>
#include <assert.h>
/* These implementations require that all of the compare functions passed return 1 if the first element is
   less than the second, 2 if they're equal or 0 if the first element is greater than the second*/

/* Merge-Sort */
extern void *mergesort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void *, const void *));

/* Quick-Sort */
extern void *quicksort(void *base, size_t nelem, size_t typesize, int (*comparefn)(const void *, const void *));

#endif
